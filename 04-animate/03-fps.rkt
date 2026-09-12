#lang racket/base
;; =========================================================
;; 04-animate/03-fps.rkt —— 帧率计算（FPS）
;; 运行：racket 04-animate/03-fps.rkt    点 X = 退出
;; =========================================================

;; 动画跑起来了。本步量一量"每秒画了多少帧"——FPS（frames per second）。
;; 方法：每画一帧计数器 +1；每隔一段时间，用 帧数 ÷ 时间 算出 FPS。
;;
;; 本步新增：
;;   current-inexact-milliseconds —— 取当前毫秒时间（01 步已用）
;;   FPS = 帧数 ÷ 秒数：每满 1 秒用 printf 打印一次到终端

(require "../02-triangle/04-gui-tool.rkt")
(require "../racket-glsl/rewrite.rkt")
(require "../racket-glsl/rename-vector.rkt")
(require "../racket-glsl/tool.rkt")

(define start-ms (current-inexact-milliseconds))

;; 着色器 + 顶点数据（同 02）。
(define vert-src
  (glsl (version 330 core)
        (layout (location 0) in vec2 aPos)
        (uniform float uTime)
        (define (main) void
          (set! gl_Position
                (vec4 (+ aPos (vec2 (* 0.5 (sin uTime)) 0.0)) 0.0 1.0)))))
(define frag-src
  (glsl (version 330 core)
        (out vec4 FragColor)
        (define (main) void
          (set! FragColor (vec4 1.0 0.35 0.2 1.0)))))
(define verts (vec (vec2 -0.5 -0.5) (vec2 0.5 -0.5) (vec2 0.0 0.5)))

;; 帧计数：frame-count = 这段时间画了几帧；last-ms = 上次算 FPS 的时刻。
(define frame-count 0)
(define last-ms (current-inexact-milliseconds))

(define (draw)
  (define t (/ (- (current-inexact-milliseconds) start-ms) 1000.0))
  (use-program prog)
  (glUniform1f loc-time t)
  (glBindVertexArray vao)
  (glDrawArrays GL_TRIANGLES 0 3)
  ;; ── 数帧 + 算 FPS ──
  (set! frame-count (add1 frame-count))
  (define now (current-inexact-milliseconds))
  (define elapsed (- now last-ms))
  (when (>= elapsed 1000)                             ; 每满 1 秒算一次
    (define fps (/ frame-count (/ elapsed 1000.0)))   ; FPS = 帧数 ÷ 秒数
    (set! frame-count 0)
    (set! last-ms now)
    (printf "FPS ≈ ~a\n" (round fps))))  ; 打印到终端（不碰窗口标题）

(define-values (frame canvas)
  (make-window #:title "04-03 帧率" #:width 400 #:height 400 #:draw draw))

;; 初始化（同 02）。
(define prog
  (send canvas with-gl-context
    (lambda () (build-program (GL_VERTEX_SHADER vert-src) (GL_FRAGMENT_SHADER frag-src)))))
(define loc-time
  (send canvas with-gl-context (lambda () (uniform-location prog "uTime"))))
(define vao
  (send canvas with-gl-context
    (lambda ()
      (define data (vec->f32vector verts))
      (define vbo (u32vector-ref (glGenBuffers 1) 0))
      (glBindBuffer GL_ARRAY_BUFFER vbo)
      (glBufferData GL_ARRAY_BUFFER (gl-vector-sizeof data) data GL_STATIC_DRAW)
      (define v (u32vector-ref (glGenVertexArrays 1) 0))
      (glBindVertexArray v)
      (glVertexAttribPointer 0 2 GL_FLOAT #f 8 0)
      (glEnableVertexAttribArray 0)
      (glBindVertexArray 0)
      v)))

(define ticker
  (new timer% (interval 16) (notify-callback (lambda () (send canvas refresh)))))

(send frame show #t)
