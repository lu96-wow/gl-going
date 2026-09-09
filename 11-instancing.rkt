#lang racket/base
;; =========================================================
;; 11-instancing.rkt —— 实例化：一次 draw 画 100 个立方体
;; 运行：racket 11-instancing.rkt   ESC/点X = 退出
;; =========================================================
;; 场景里 100 个相同网格，普通做法 = 100 次 draw call（每次都走一遍
;; CPU→GPU 的状态切换）。本课让它变 1 次，新 API 两个：
;;
;;   ① glVertexAttribDivisor(loc, 1)
;;      —— 默认(除数 0)属性"每个顶点取一次"；除数 1 表示"每个实例取
;;         一次"。于是同一份网格被画 N 次时，每次可以从实例数组里拿
;;         到属于自己的那组属性（位置/颜色/相位…）。
;;   ② glDrawElementsInstanced(图元, 个数, 类型, 0, 实例数)
;;      —— 把"画一次"变成"画 N 次"，N 个实例共享同一份网格数据。
;;
;; 图形原理：CPU→GPU 是窄通道。能一次传完的绝不传 100 次——
;; 实例化把"变化的部分"(每实例偏移/颜色)做成一个数组，GPU 画第 i 个
;; 实例时自动取第 i 行；几何本身(立方体)只存一份。游戏里成千上万的
;; 草、树、粒子都是这个套路。
;;
;; ★步长陷阱（本课最容易踩的坑）：实例数组每行 7 个 float，
;;   属性 stride 必须是 7×4=28 字节。若 CPU 写的数据行与 stride 不一致，
;;   读出来的"偏移"全是串位的乱值——画面就会莫名堆成几根线/几个点。
;;
;; 演示：10×10=100 个立方体排成方阵，高度按 (时间+相位) 起浪。
;; =========================================================

(require racket/gui opengl)
(require "lib.rkt")

(define PI (acos -1.0))
(define start-ms (current-inexact-milliseconds))
(define N 100)                    ; 10×10 个实例

;; 顶点着色器：aPos 是共享的立方体角点(模型空间)；aOffset/aColor/aPhase
;; 是"每个实例一份"的属性（glVertexAttribDivisor 设为 1 的那个数组）。
;; 每个实例按自己的相位 bob 上下浮动，再摆到自己的网格位置。
(define vert-src
  (glsl-pretty
   (glsl
    (version 330 core)
    (layout (location 0) in vec3 aPos)
    (layout (location 1) in vec3 aOffset)
    (layout (location 2) in vec3 aColor)
    (layout (location 3) in float aPhase)
    (uniform mat4 uVP)
    (uniform float uTime)
    (out vec3 vColor)
    (define (main) void
      (set! vColor aColor)
      (float bob (* 0.35 (sin (+ (* uTime 2.0) aPhase))))
      (vec3 p (+ (+ (* aPos 0.45) aOffset) (vec3 0.0 bob 0.0)))
      (set! gl_Position (* uVP (vec4 p 1.0)))))))

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
(send cfg set-depth-size 1)
(define frame
  (new (class frame%
         (augment* [on-close (lambda () (exit 0))])
         (super-new))
       (label "11 实例化") (width 800) (height 600)))

(define fw (box 800)) (define fh (box 600))
(define init? (box #f))
(define prog #f) (define vao 0)
(define loc-vp 0) (define loc-time 0)

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
              (glClearColor 0.05 0.06 0.10 1.0))))
         (define/override (on-char e)
           (when (eq? (send e get-key-code) 'escape) (exit 0)))
         (define/override (on-paint)
           (with-gl-context
            (lambda ()
              (unless (unbox init?)
                (set-box! init? #t)
                (set! prog (build-program vert-src frag-src))
                (set! loc-vp   (glGetUniformLocation prog "uVP"))
                (set! loc-time (glGetUniformLocation prog "uTime"))
                (define v (u32vector-ref (glGenVertexArrays 1) 0))
                (glBindVertexArray v)

                ;; ① 共享网格：单位立方体 8 顶点 + 36 索引（只存一次）
                (define pos8
                  (f32vector -1.0 -1.0  1.0   1.0 -1.0  1.0   1.0  1.0  1.0  -1.0  1.0  1.0
                             -1.0 -1.0 -1.0   1.0 -1.0 -1.0   1.0  1.0 -1.0  -1.0  1.0 -1.0))
                (define idx
                  (u16vector 0 1 2  0 2 3  5 4 7  5 7 6  1 5 6  1 6 2
                             4 0 3  4 3 7  3 2 6  3 6 7  4 5 1  4 1 0))
                (define vb (u32vector-ref (glGenBuffers 1) 0))
                (glBindBuffer GL_ARRAY_BUFFER vb)
                (glBufferData GL_ARRAY_BUFFER (gl-vector-sizeof pos8) pos8 GL_STATIC_DRAW)
                (glVertexAttribPointer 0 3 GL_FLOAT #f (* 3 4) 0)
                (glEnableVertexAttribArray 0)
                (define eb (u32vector-ref (glGenBuffers 1) 0))
                (glBindBuffer GL_ELEMENT_ARRAY_BUFFER eb)
                (glBufferData GL_ELEMENT_ARRAY_BUFFER (gl-vector-sizeof idx) idx GL_STATIC_DRAW)

                ;; ② 实例数组：每实例 7 float = 偏移(xyz) 颜色(rgb) 相位(w)
                (define inst
                  (apply f32vector
                         (apply append
                                (for/list ([i (in-range N)])
                                  (define ix (exact->inexact (quotient i 10)))
                                  (define iz (exact->inexact (remainder i 10)))
                                  (define x (- (* ix 1.1) 4.95))
                                  (define z (- (* iz 1.1) 4.95))
                                  (define cr (+ 0.15 (* 0.5 (/ ix 9.0))))
                                  (define cg (+ 0.25 (* 0.55 (/ iz 9.0))))
                                  (list x 0.0 z cr cg 0.9
                                        (+ (* 1.7 ix) (* 2.3 iz)))))))
                (define ib (u32vector-ref (glGenBuffers 1) 0))
                (glBindBuffer GL_ARRAY_BUFFER ib)
                (glBufferData GL_ARRAY_BUFFER (gl-vector-sizeof inst) inst GL_STATIC_DRAW)
                ;; 三个"每实例"属性：stride 必须与数据行 28 字节一致
                (define s7 (* 7 4))
                (glVertexAttribPointer 1 3 GL_FLOAT #f s7 0)
                (glVertexAttribDivisor 1 1)                        ; ★每实例取一次
                (glEnableVertexAttribArray 1)
                (glVertexAttribPointer 2 3 GL_FLOAT #f s7 (* 3 4))
                (glVertexAttribDivisor 2 1)
                (glEnableVertexAttribArray 2)
                (glVertexAttribPointer 3 1 GL_FLOAT #f s7 (* 6 4))
                (glVertexAttribDivisor 3 1)
                (glEnableVertexAttribArray 3)
                (glBindVertexArray 0)
                (set! vao v))

              (define t (/ (- (current-inexact-milliseconds) start-ms) 1000.0))
              (define gw (unbox fw)) (define gh (unbox fh))
              (define aspect (/ (exact->inexact gw) (exact->inexact gh)))
              (define P (m4-perspective 45.0 aspect 0.1 100.0))
              (define ca (* (/ PI 180.0) (* t 12.0)))
              (define V (m4-look-at (* 11.0 (sin ca)) 7.0 (* 11.0 (cos ca))
                                    0.0 0.0 0.0  0.0 1.0 0.0))

              (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
              (glUseProgram prog)
              (glUniformMatrix4fv loc-vp 1 #f (mat4 (m4-mult P V)))
              (glUniform1f loc-time t)
              (glBindVertexArray vao)
              ;; ★一次调用画 100 个
              (glDrawElementsInstanced GL_TRIANGLES 36 GL_UNSIGNED_SHORT 0 N)

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
