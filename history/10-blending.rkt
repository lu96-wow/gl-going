#lang racket/base
;; =========================================================
;; 10-blending.rkt —— 混合：半透明物体（glBlendFunc）
;; 运行：racket 10-blending.rkt   B = 混合开/关   ESC = 退出
;; =========================================================
;; 深度测试解决"遮挡"，但它只能二选一：这个片元过还是不过。
;; 半透明需要"这个片元 60% 透过 + 底下的 40% 露出来"——这就是**混合**：
;;
;;   新 API：
;;     glEnable(GL_BLEND)
;;     glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
;;       = 输出色 = 源色(新画的,带 alpha) 按比例与 目标色(缓冲里已有) 混合：
;;         out = src.rgb × src.a + dst.rgb × (1 − src.a)
;;
;;   片元着色器给 FragColor 的 alpha 就是"不透明度"来源。
;;   （我们此前所有 FragColor 的 alpha 都写死 1.0，等于从不混合。）
;;
;; 图形原理：透明物有两个"坑"，看懂就不会翻车——
;;   ① 混合 ≠ 深度测试：它要的是"背后已经画好的颜色"，所以
;;      透明物体必须**先画不透明、再画透明**（画家算法的一部分），
;;      还要按"离相机远→近"排好，否则前后透明片会互相穿帮。
;;   ② 画透明时通常 glDepthMask(GL_FALSE)：不写深度缓冲。
;;      否则一个透明片先被写进深度，后面离它更远的透明片就被深度
;;      测试挡掉，再也混不进来。
;;
;; 演示：不透明两色立方体 + 一前一后两块彩色半透明"玻璃"。
;; B 键关掉混合：alpha 被无视，玻璃画成实心 → 立刻挡住后面。
;; =========================================================

(require racket/gui opengl)
(require "lib.rkt")

(define PI (acos -1.0))
(define start-ms (current-inexact-milliseconds))
(define blend-on? (box #t))

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
    (uniform float uAlpha)
    (out vec4 FragColor)
    (define (main) void
      (set! FragColor (vec4 vColor uAlpha))))))

;; 用法小抄：混合发生在片元着色器输出**之后**的固定阶段——GLSL 只能决定
;; 输出多少 alpha，能不能混、怎么混是 glBlendFunc 的事。此前所有课的 alpha
;; 都写 1.0（不透明），所以从未触发过混合。

(define cfg (new gl-config%))
(send cfg set-legacy? #f)
(send cfg set-double-buffered #t)
(send cfg set-depth-size 1)
(define frame
  (new (class frame%
         (augment* [on-close (lambda () (exit 0))])
         (super-new))
       (label "10 半透明混合") (width 800) (height 600)))

(define fw (box 800)) (define fh (box 600))
(define init? (box #f))
(define prog #f) (define vao-cube 0)
(define loc-mvp 0) (define loc-alpha 0)

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
              (glClearColor 0.06 0.07 0.12 1.0))))
         (define/override (on-char e)
           (define code (send e get-key-code))
           (when (not (eq? code 'release))
             (cond
               [(eq? code 'escape) (exit 0)]
               [(or (eq? code #\b) (eq? code #\B))
                (set-box! blend-on? (not (unbox blend-on?)))
                (printf (if (unbox blend-on?) "混合 开（玻璃透出后面）~%"
                            "混合 关（alpha 被忽略，全画实心）~%"))])))
         (define/override (on-paint)
           (with-gl-context
            (lambda ()
              (unless (unbox init?)
                (set-box! init? #t)
                (set! prog (build-program vert-src frag-src))
                (set! loc-mvp   (glGetUniformLocation prog "uMVP"))
                (set! loc-alpha (glGetUniformLocation prog "uAlpha"))
                ;; 24 顶点带色立方体（同 05）
                (define pos8
                  '((-1.0 -1.0  1.0) ( 1.0 -1.0  1.0) ( 1.0  1.0  1.0) (-1.0  1.0  1.0)
                    (-1.0 -1.0 -1.0) ( 1.0 -1.0 -1.0) ( 1.0  1.0 -1.0) (-1.0  1.0 -1.0)))
                (define faces
                  (list (list 0.85 0.20 0.20 '(0 1 2 3)) (list 0.20 0.80 0.25 '(5 4 7 6))
                        (list 0.95 0.60 0.10 '(1 5 6 2)) (list 0.95 0.85 0.15 '(4 0 3 7))
                        (list 0.20 0.60 0.95 '(3 2 6 7)) (list 0.75 0.30 0.90 '(4 5 1 0))))
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
              (define P (m4-perspective 45.0 aspect 0.1 100.0))
              (define V (m4-look-at 0.0 2.6 7.5  0.0 0.6 0.0  0.0 1.0 0.0))

              (if (unbox blend-on?)
                  (begin (glEnable GL_BLEND)
                         (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA))
                  (glDisable GL_BLEND))
              (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
              (glUseProgram prog)
              (glUniform1f loc-alpha 1.0)

              (define (draw-cube m)
                (glUniformMatrix4fv loc-mvp 1 #f (mat4 (m4-mult (m4-mult P V) m)))
                (glBindVertexArray vao-cube)
                (glDrawElements GL_TRIANGLES 36 GL_UNSIGNED_SHORT 0))

              ;; ---- 第一遍：不透明物体（正常画，写深度）----
              ;; 两侧小立方体 + 中央立方体（都缩到 0.5，保持间距）
              (glDepthMask #t)
              (glUniform1f loc-alpha 1.0)
              (for ([i (in-range 2)])
                (define x (* 1.9 (- i 0.5)))
                (draw-cube (m4-mult (m4-translate x 0.5 0.0)
                                    (m4-mult (m4-rot-y (* t 40.0))
                                             (m4-scale 0.5 0.5 0.5)))))
              (draw-cube (m4-mult (m4-translate 0.0 0.6 0.0)
                                  (m4-mult (m4-rot-y (* t -30.0))
                                           (m4-scale 0.5 0.5 0.5))))
              ;; ---- 第二遍：半透明"玻璃板"（关深度写入）----
              ;; 玻璃 = 单位立方体把 z 压扁成薄板：S(1.1, 1.1, 0.02)
              ;; 它正好挡在中央立方体前方 → 混合开时能透出后面的立方体
              (glDepthMask #f)
              (glUniform1f loc-alpha 0.35)
              (define (glass y z)
                (glUniformMatrix4fv loc-mvp 1 #f
                                    (mat4 (m4-mult (m4-mult P V)
                                                      (m4-mult (m4-translate 0.0 y z)
                                                               (m4-scale 1.1 1.1 0.02)))))
                (glDrawElements GL_TRIANGLES 36 GL_UNSIGNED_SHORT 0))
              (glass 0.9 1.2)       ; 靠近相机，挡在中央立方体前
              (glDepthMask #t)
              (glDisable GL_BLEND)

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
