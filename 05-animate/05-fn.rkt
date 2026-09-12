#lang racket/base
;; =========================================================
;; 05-animate/05-fn.rkt —— 第五步：自写函数（define）
;; 运行：racket 05-animate/05-fn.rkt    点 X = 退出
;; =========================================================
;; 上一步：会用 for 重复。本步学"给公式起名字"——自写函数，同一段算三遍。
;;
;; 本步新增（1 组，同属"自写函数"这一件事）：
;;   define 函数 —— 函数名 + 带类型的参数 + 返回类型 + 函数体
;;
;; ★语法（DSL 写法 → 展开成 GLSL）：
;;   (define (wave (float x) (float phase)) float
;;     函数体……)
;;     →  float wave(float x, float phase) { ... }
;;   结构：函数名 wave；参数是 (类型 名字) 一列，如 (float x)；
;;         返回类型写在参数表之后（这里是 float）；体里最后写一个表达式，
;;         就是返回值（DSL 自动帮你加 return，展开后能看到）。
;;   ★参数按值传递：调 (wave a b) 时 a、b 的值拷进去，函数里改不了外面。
;;
;; ★函数放哪：GLSL 里函数必须写在 main 之前。DSL 里直接写在 (glsl ...) 顶层，
;;   (glsl) 宏会自动排到正确位置——你只管写，顺序它来管。
;;
;; 本步视觉（三色流动条纹）：wave(x, phase) = 把正弦映射到 0..1 的"波"。
;;   同一函数调三次、只改 phase（0 / 2 / 4），分别当 红/绿/蓝 三个通道 →
;;   三条波相位错开，合成流动的彩色条纹。这就是函数的意义：写一遍、用多遍。
;; =========================================================

(require "lib-gui.rkt")
(require "lib.rkt")

(define start-ms (current-inexact-milliseconds))

(define vert-src
  (glsl (version 330 core)
        (layout (location 0) in vec2 aPos)
        (layout (location 1) in vec2 aUV)
        (out vec2 vUV)
        (define (main) void
          (set! vUV aUV)
          (set! gl_Position (vec4 aPos 0.0 1.0)))))

;; 片元着色器（本步主角）：
;;   (define (wave (float x) (float phase)) float ...)
;;     自写函数：输入 x 和 phase 两个 float，返回 float。
;;     体是 (* 0.5 (+ 1.0 (sin (+ x phase))))——把 sin(-1..1) 映射到 0..1。
;;   main 里调三次 (wave ...)，phase 分别 0 / 2 / 4，当三个颜色通道。
(define frag-src
  (glsl (version 330 core)
        (in vec2 vUV)
        (uniform float uTime)
        (out vec4 FragColor)
        (define (wave (float x) (float phase)) float
          (* 0.5 (+ 1.0 (sin (+ x phase)))))
        (define (main) void
          (vec2 p (- (* vUV 2.0) 1.0))
          (vec3 c (vec3 (wave (* (x p) 6.0) uTime)
                        (wave (* (x p) 6.0) (+ uTime 2.0))
                        (wave (* (x p) 6.0) (+ uTime 4.0))))
          (set! FragColor (vec4 c 1.0)))))

(printf "片元着色器展开为：\n~a\n" (glsl-pretty frag-src))

(define verts
  (vec (vec4 -1.0 -1.0  0.0 0.0)
       (vec4  1.0 -1.0  1.0 0.0)
       (vec4  1.0  1.0  1.0 1.0)
       (vec4 -1.0 -1.0  0.0 0.0)
       (vec4  1.0  1.0  1.0 1.0)
       (vec4 -1.0  1.0  0.0 1.0)))

(define (draw)
  (define t (/ (- (current-inexact-milliseconds) start-ms) 1000.0))
  (glClearColor 0.10 0.12 0.20 1.0)
  (glClear GL_COLOR_BUFFER_BIT)
  (glUseProgram prog)
  (glUniform1f loc-time t)
  (glBindVertexArray vao)
  (glDrawArrays GL_TRIANGLES 0 6))

(define-values (frame canvas)
  (make-window #:title "05-05 自写函数" #:width 400 #:height 400 #:draw draw))

(define prog (send canvas with-gl-context (lambda () (build-program (GL_VERTEX_SHADER vert-src) (GL_FRAGMENT_SHADER frag-src)))))
(define loc-time (send canvas with-gl-context (lambda () (glGetUniformLocation prog "uTime"))))
(define vao
  (send canvas with-gl-context
        (lambda ()
          (define vbo (u32vector-ref (glGenBuffers 1) 0))
          (glBindBuffer GL_ARRAY_BUFFER vbo)
          (glBufferData GL_ARRAY_BUFFER (gl-vector-sizeof (vec->f32vector verts)) (vec->f32vector verts) GL_STATIC_DRAW)
          (define v (u32vector-ref (glGenVertexArrays 1) 0))
          (glBindVertexArray v)
          (glVertexAttribPointer 0 2 GL_FLOAT #f 16 0)
          (glEnableVertexAttribArray 0)
          (glVertexAttribPointer 1 2 GL_FLOAT #f 16 8)
          (glEnableVertexAttribArray 1)
          (glBindVertexArray 0)
          v)))

(define ticker
  (new timer% (interval 16)
       (notify-callback
        (lambda () (send canvas refresh)))))

(send frame show #t)
