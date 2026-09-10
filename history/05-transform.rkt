#lang racket/base
;; =========================================================
;; 05-transform.rkt —— 变换：模型矩阵（T·R·S）与投影矩阵
;; 运行：racket 05-transform.rkt    ESC/点X = 退出
;; =========================================================
;; 上一课(04)：每帧用 uniform 传"偏移量"移动方块——手动、一次一个参数。
;; 本课把它系统化：所有"位置/朝向/大小"统一用一个 **4×4 矩阵** 表达，
;; 顶点只需乘一次矩阵就完成变换。这是整个 3D 的核心数学，新 API 只有：
;;
;;   glUniformMatrix4fv(loc, 1, #f, mat4f32)   ← 上传 4×4 矩阵（列主序）
;;
;; 图形原理（矩阵三部曲）：
;;   OpenGL 约定顶点最终要落在"裁剪坐标"。我们给它一串矩阵，逐个相乘：
;;
;;     gl_Position = uMVP * vec4(顶点局部坐标, 1.0)
;;
;;   本课的矩阵组成（一学就会的顺口溜：先缩放，再旋转，最后平移）：
;;     M = T(位置) · Rz(角度) · S(尺寸)      ← "模型矩阵"：局部→世界
;;     P = 正交投影（把"像素坐标"映射成 NDC）  ← 投影矩阵：世界→裁剪
;;   ★矩阵乘法不交换：T·R·S 表示"先 S 缩放、再 R 旋转、最后 T 平移"，
;;     顺序反了（比如先平移再缩放）结果完全不同——窗口里两个方块就是
;;     正反顺序的对比，能看到行为差别。
;;
;; 演示（世界坐标 = 屏幕像素，800×600 窗口，左上为原点、y 向下）：
;;   中央大矩形自转；它右上角一个小方块绕它公转并自转；
;;   中心一个小红点标记。M 键切换模型矩阵顺序 T·R·S ↔ R·T·S，
;;   观察“绕自身转”（对）与“绕屏幕原点甩出去”（错）的差别。
;; =========================================================

(require racket/gui opengl)
(require "lib.rkt")

(define PI (acos -1.0))
(define start-ms (current-inexact-milliseconds))
(define order? (box #t))          ; #t = T·R·S（先缩旋再平移）

;; 顶点着色器：aPos 是单位方块角点（中心在原点、边长 1，范围 [-.5,.5]）
(define vert-src
  (glsl-pretty
   (glsl
    (version 330 core)
    (layout (location 0) in vec2 aPos)
    (uniform mat4 uMVP)
    (define (main) void
      (set! gl_Position (* uMVP (vec4 aPos 0.0 1.0)))))))

(define frag-src
  (glsl-pretty
   (glsl
    (version 330 core)
    (uniform vec3 uColor)
    (out vec4 FragColor)
    (define (main) void
      (set! FragColor (vec4 uColor 1.0))))))

;; 用法小抄：
;;   为什么是 4×4？平移是"加"、旋转缩放是"乘"，用 4×4 的齐次坐标可以把
;;   加伪装成乘 → 所有变换统一成矩阵乘法，还能先乘成一个矩阵一次上传。
;;   为什么强调"列主序"？glUniformMatrix4fv 按列读内存；lib.rkt 的 f64vector
;;   同样按列存。顺序写反等于转置，旋转会歪着来。

(define cfg (new gl-config%))
(send cfg set-legacy? #f)
(send cfg set-double-buffered #t)
(define frame
  (new (class frame%
         (augment* [on-close (lambda () (exit 0))])
         (super-new))
       (label "05 变换矩阵") (width 800) (height 600)))

(define fw (box 800)) (define fh (box 600))
(define init? (box #f))
(define prog #f) (define vao 0)
(define loc-mvp 0) (define loc-color 0)

(define canvas
  (new (class canvas%
         (inherit with-gl-context swap-gl-buffers)
         (define/override (on-size w h)
           (with-gl-context
            (lambda ()
              (define-values (gw gh) (send this get-gl-client-size))
              (set-box! fw gw) (set-box! fh gh)
              (glViewport 0 0 gw gh)
              (glClearColor 0.07 0.08 0.14 1.0))))
         (define/override (on-char e)
           (define code (send e get-key-code))
           (when (not (eq? code 'release))
             (cond
               [(eq? code 'escape) (exit 0)]
               [(or (eq? code #\m) (eq? code #\M))
                (set-box! order? (not (unbox order?)))
                (printf (if (unbox order?) "顺序 T·R·S（先缩旋再平移）~%" "顺序 R·T·S（旋转先于平移，观察公转错位）~%"))])))
         (define/override (on-paint)
           (with-gl-context
            (lambda ()
              (unless (unbox init?)
                (set-box! init? #t)
                (set! prog (build-program vert-src frag-src))
                (set! loc-mvp   (glGetUniformLocation prog "uMVP"))
                (set! loc-color (glGetUniformLocation prog "uColor"))
                ;; 单位方块（EBO）：中心在原点、边长 1
                (define verts (f32vector -0.5 -0.5   0.5 -0.5   0.5 0.5  -0.5 0.5))
                (define idx (u16vector 0 1 2 0 2 3))
                (define v (u32vector-ref (glGenVertexArrays 1) 0))
                (glBindVertexArray v)
                (define vb (u32vector-ref (glGenBuffers 1) 0))
                (glBindBuffer GL_ARRAY_BUFFER vb)
                (glBufferData GL_ARRAY_BUFFER (gl-vector-sizeof verts) verts GL_STATIC_DRAW)
                (glVertexAttribPointer 0 2 GL_FLOAT #f (* 2 4) 0)
                (glEnableVertexAttribArray 0)
                (define eb (u32vector-ref (glGenBuffers 1) 0))
                (glBindBuffer GL_ELEMENT_ARRAY_BUFFER eb)
                (glBufferData GL_ELEMENT_ARRAY_BUFFER (gl-vector-sizeof idx) idx GL_STATIC_DRAW)
                (glBindVertexArray 0)
                (set! vao v))

              (define t (/ (- (current-inexact-milliseconds) start-ms) 1000.0))
              (define gw (unbox fw)) (define gh (unbox fh))
              ;; P：像素世界 → NDC（正交）。左上是 (0,0)，y 向下
              (define P (m4-ortho 0.0 (exact->inexact gw) (exact->inexact gh) 0.0 -1.0 1.0))

              (glClear GL_COLOR_BUFFER_BIT)
              (glUseProgram prog)
              (glBindVertexArray vao)

              ;; 画一个"单位方块"：给中心/半宽半高/角度/颜色，拼模型矩阵
              (define (draw-quad cx cy hx hy ang color)
                (define S (m4-scale (* 2.0 hx) (* 2.0 hy) 1.0))
                (define R (m4-rot-z ang))
                (define T (m4-translate cx cy 0.0))
                (define M (if (unbox order?)
                              (m4-mult T (m4-mult R S))    ; T·R·S（标准）
                              (m4-mult R (m4-mult T S)))) ; R·T·S（错误示范）
                (glUniformMatrix4fv loc-mvp 1 #f (mat4 (m4-mult P M)))
                (glUniform3f loc-color (list-ref color 0) (list-ref color 1) (list-ref color 2))
                (glDrawElements GL_TRIANGLES 6 GL_UNSIGNED_SHORT 0))

              ;; 中央大矩形：绕它自己中心转
              (draw-quad (/ gw 2.0) (/ gh 2.0) 150.0 90.0 (* t 55.0) '(0.30 0.65 0.95))
              ;; 右上角小方块：公转（绕大矩形中心）+ 自转
              (define a (* t 95.0))
              (define rad (* (/ PI 180.0) a))
              (define ox (+ (/ gw 2.0) (* 200.0 (cos rad))))
              (define oy (+ (/ gh 2.0) (* 150.0 (sin rad))))
              (draw-quad ox oy 34.0 34.0 (* t 200.0) '(0.95 0.70 0.30))
              ;; 大矩形中心的小标记
              (draw-quad (/ gw 2.0) (/ gh 2.0) 6.0 6.0 0.0 '(1.0 0.3 0.3))

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
