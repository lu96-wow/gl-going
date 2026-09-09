#lang racket/base
;; =========================================================
;; 06-3d-depth.rkt —— 进入 3D：透视投影 + 深度缓冲
;; 运行：racket 06-3d-depth.rkt    D = 深度测试开/关    ESC = 退出
;; =========================================================
;; 上一课(05)：2D 像素世界，正交投影 P·M。
;; 本课升到 3D，只多两个概念，却让"近大远小 + 前后遮挡"同时成立：
;;
;;   ① 透视投影矩阵 m4-perspective(fovy, aspect, near, far)
;;      —— 把 3D 视锥压成 NDC 盒子。它让 w 分量不再是 1，
;;         顶点着色器输出 vec4 后 GPU 自动做"透视除法"(xyz/w)：
;;         离相机越远 w 越大 → 越挤向中心 → 视觉上更小。
;;         near/far 之外的顶点会被裁剪掉。
;;         ★顶点着色器只负责"输出裁剪坐标"；之后 图元装配→视锥裁剪→
;;           透视除法(xyz÷w)→视口映射 全是 GPU 固定步骤，GLSL 碰不到。
;;         ★near 设太小 / far 设太大，深度精度会变差（z 以非线性方式
;;           存进深度缓冲），所以取 0.1 / 100 这种量级，别写 0.0001/1e9。
;;   ② 深度缓冲（Z 缓冲）
;;      —— glEnable(GL_DEPTH_TEST) + 每帧清 GL_DEPTH_BUFFER_BIT。
;;         每个片元画之前先比较自己的 z 与缓冲里已存的 z，
;;         远的画不过近的 → 遮挡关系正确。
;;
;; 新 API：
;;   glEnable(GL_DEPTH_TEST)           深度测试开关
;;   glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)   帧尾加清深度
;;   m4-perspective(fovy 度, aspect, near, far)（本课库里的新工具）
;;
;; ★顶点着色器必须输出 vec4：位置是 3D 的了（vec3 aPos）。
;;   立方体 = 24 个顶点（每个面 4 个，法线以后光照课才需要，
;;   这里每个顶点带颜色即可）+ 36 个索引。
;; =========================================================

(require racket/gui opengl)
(require "lib.rkt")

(define PI (acos -1.0))
(define start-ms (current-inexact-milliseconds))
(define depth-on? (box #t))

;; 顶点着色器：uMVP = P(投影) · V(把世界后移的假相机) · M(模型)。
;; 位置已是 vec3，乘矩阵后得到裁剪坐标。
(define vert-src
  (glsl-pretty
   (glsl
    (version 330 core)
    (layout (location 0) in vec3 aPos)
    (layout (location 1) in vec3 aColor)
    (uniform mat4 uMVP)
    (out vec3 vColor)
    (define (main) void
      (set! vColor aColor)
      (set! gl_Position (* uMVP (vec4 aPos 1.0)))))))

(define frag-src
  (glsl-pretty
   (glsl
    (version 330 core)
    (in vec3 vColor)
    (out vec4 FragColor)
    (define (main) void
      (set! FragColor (vec4 vColor 1.0))))))

(define cfg (new gl-config%))
(send cfg set-legacy? #f)
(send cfg set-double-buffered #t)
(send cfg set-depth-size 1)          ; 向窗口要一块深度缓冲
(define frame
  (new (class frame%
         (augment* [on-close (lambda () (exit 0))])
         (super-new))
       (label "06 3D 透视与深度") (width 800) (height 600)))

(define fw (box 800)) (define fh (box 600))
(define init? (box #f))
(define prog #f) (define vao-cube 0) (define loc-mvp 0)

(define canvas
  (new (class canvas%
         (inherit with-gl-context swap-gl-buffers)
         (define/override (on-size w h)
           (with-gl-context
            (lambda ()
              (define-values (gw gh) (send this get-gl-client-size))
              (set-box! fw gw) (set-box! fh gh)
              (glViewport 0 0 gw gh)
              (glEnable GL_DEPTH_TEST)
              (glClearColor 0.07 0.08 0.14 1.0))))
         (define/override (on-char e)
           (define code (send e get-key-code))
           (when (not (eq? code 'release))
             (cond
               [(eq? code 'escape) (exit 0)]
               [(or (eq? code #\d) (eq? code #\D))
                (set-box! depth-on? (not (unbox depth-on?)))
                (printf (if (unbox depth-on?) "深度测试 开（前后遮挡正确）~%"
                            "深度测试 关（谁后画谁盖住前面——看错误）~%"))])))
         (define/override (on-paint)
           (with-gl-context
            (lambda ()
              (unless (unbox init?)
                (set-box! init? #t)
                (set! prog (build-program vert-src frag-src))
                (set! loc-mvp (glGetUniformLocation prog "uMVP"))
                ;; 立方体：6 面 × 4 顶点 [x,y,z + r,g,b]，36 索引
                (define pos8
                  '((-1.0 -1.0  1.0) ( 1.0 -1.0  1.0) ( 1.0  1.0  1.0) (-1.0  1.0  1.0)
                    (-1.0 -1.0 -1.0) ( 1.0 -1.0 -1.0) ( 1.0  1.0 -1.0) (-1.0  1.0 -1.0)))
                (define faces
                  (list (list 0.85 0.20 0.20 '(0 1 2 3))   ; +z 前
                        (list 0.20 0.80 0.25 '(5 4 7 6))   ; -z 后
                        (list 0.95 0.60 0.10 '(1 5 6 2))   ; +x 右
                        (list 0.95 0.85 0.15 '(4 0 3 7))   ; -x 左
                        (list 0.20 0.60 0.95 '(3 2 6 7))   ; +y 上
                        (list 0.75 0.30 0.90 '(4 5 1 0)))) ; -y 下
                (define verts
                  (apply f32vector
                         (apply append
                                (for/list ([f faces])
                                  (apply append
                                         (for/list ([j (in-range 4)])
                                           (define p (list-ref pos8 (list-ref (list-ref f 3) j)))
                                           (list (car p) (cadr p) (caddr p)
                                                 (car f) (cadr f) (caddr f))))))))
                (define idx
                  (apply u16vector
                         (apply append
                                (for/list ([i (in-range 6)])
                                  (define b (* i 4))
                                  (list b (+ b 1) (+ b 2) b (+ b 2) (+ b 3))))))
                (define v (u32vector-ref (glGenVertexArrays 1) 0))
                (glBindVertexArray v)
                (define vb (u32vector-ref (glGenBuffers 1) 0))
                (glBindBuffer GL_ARRAY_BUFFER vb)
                (glBufferData GL_ARRAY_BUFFER (gl-vector-sizeof verts) verts GL_STATIC_DRAW)
                (define s6 (* 6 4))
                (glVertexAttribPointer 0 3 GL_FLOAT #f s6 0)
                (glEnableVertexAttribArray 0)
                (glVertexAttribPointer 1 3 GL_FLOAT #f s6 (* 3 4))
                (glEnableVertexAttribArray 1)
                (define eb (u32vector-ref (glGenBuffers 1) 0))
                (glBindBuffer GL_ELEMENT_ARRAY_BUFFER eb)
                (glBufferData GL_ELEMENT_ARRAY_BUFFER (gl-vector-sizeof idx) idx GL_STATIC_DRAW)
                (glBindVertexArray 0)
                (set! vao-cube v))

              (define t (/ (- (current-inexact-milliseconds) start-ms) 1000.0))
              (define gw (unbox fw)) (define gh (unbox fh))
              (define aspect (/ (exact->inexact gw) (exact->inexact gh)))
              ;; P：透视。V：这课用最简单的手动"相机" = 把世界往后退 6
              (define P (m4-perspective 45.0 aspect 0.1 100.0))
              (define V (m4-translate 0.0 0.0 -6.0))

              (define (draw-cube-at M)
                (glUniformMatrix4fv loc-mvp 1 #f (mat4 (m4-mult (m4-mult P V) M)))
                (glBindVertexArray vao-cube)
                (glDrawElements GL_TRIANGLES 36 GL_UNSIGNED_SHORT 0))

              (if (unbox depth-on?) (glEnable GL_DEPTH_TEST) (glDisable GL_DEPTH_TEST))
              (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
              (glUseProgram prog)

              ;; 三颗并排的立方体：各自自转，前后拉开距离（z 不同）
              ;; 三颗小立方体：旋转时占√3≈1.73 的体积，所以边长缩到 0.6
              (for ([k (in-range 3)])
                (define x (- (* 2.6 (- k 1)) 0.0))
                (define M (m4-mult (m4-translate x 0.0 (- 1.0 (* 1.2 k)))
                                   (m4-mult (m4-mult (m4-rot-y (* t (+ 40.0 (* k 30.0))))
                                                     (m4-rot-x (* t 30.0)))
                                            (m4-scale 0.6 0.6 0.6))))
                (draw-cube-at M))

              (send this swap-gl-buffers))))
         (super-new))
       (style '(gl no-autoclear))
       (gl-config cfg)
       (parent frame)))

(define ticker
  (new timer% (interval 16)
       (notify-callback
        (lambda () (send canvas refresh)))))
(send frame show #t)
(send canvas focus)
