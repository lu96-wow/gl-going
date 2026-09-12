#lang racket/base
;; =========================================================
;; 05-animate/02-uniform.rkt —— 第二步：把时间传进 shader（uniform）
;; 运行：racket glsl/05-animate/02-uniform.rkt    点 X = 退出
;; =========================================================
;; 上一步：timer 让"帧"持续发生。本步让"每帧和每帧不一样"——把时间传进 shader。
;;
;; 本步新增（2 个，同属"传参数给 shader"这一件事）：
;;   uniform        —— shader 里声明"本次 draw 全体共用的参数"
;;   glUniform1f    —— 从 CPU 上传这个参数的值（1 个 float）
;;
;; ★uniform 是什么（和 02 课的 attribute 正好互补）：
;;   attribute(in) = 每个顶点一份   → 存在 VBO 里，顶点着色器每顶点取一次
;;   uniform       = 每次 draw 一份 → 用 glUniform* 从 CPU 上传，本次 draw 通吃
;;   时间 uTime 对所有顶点/片元都一样，所以是 uniform 而不是 attribute。
;;
;; ★CPU→GPU 的数据流（动画的最小模型）：
;;   Racket 每帧量时间 t → glUniform1f 传进 GPU → shader 读 uTime 算颜色。
;;   数据在 CPU 上流动，shader 只是"读参数的纯函数"。下次 draw 传新 t，画面就动。
;;   这就是所有游戏循环里"CPU 驱动 GPU"的最小例子。
;;
;; ★用法三件套：
;;   (uniform float uTime)     shader 里声明（名字自己起）
;;   glGetUniformLocation      拿这个 uniform 的"位置号"（只在初始化时查一次）
;;   glUniform1f 位置号 值      每帧上传新值（必须在 glUseProgram 之后）
;;
;; 本步视觉：03 的同心圆，但 ring = fract(d*6 - uTime)——减掉时间，环就一圈圈
;;   往外跑，画面活了。
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
;;   (uniform float uTime)   ★声明：本次 draw 全体共用的 float
;;   (fract (- (* d 6.0) uTime))  03 的 fract(d*6) 再减 uTime：
;;     时间每 +1，环的位置就整体移动一格 → 环往外跑
(define frag-src
  (glsl (version 330 core)
        (in vec2 vUV)
        (uniform float uTime)
        (out vec4 FragColor)
        (define (main) void
          (vec2 p (- (* vUV 2.0) 1.0))
          (float d (length p))
          (float ring (fract (- (* d 6.0) uTime)))
          (vec3 c (mix (vec3 0.10 0.15 0.40) (vec3 0.10 0.70 1.00) ring))
          (set! c (* c (- 1.0 (* 0.55 d))))
          (set! FragColor (vec4 (clamp c 0.0 1.0) 1.0)))))

(printf "片元着色器展开为：\n~a\n" (glsl-pretty frag-src))

(define verts
  (vec (vec4 -1.0 -1.0  0.0 0.0)
       (vec4  1.0 -1.0  1.0 0.0)
       (vec4  1.0  1.0  1.0 1.0)
       (vec4 -1.0 -1.0  0.0 0.0)
       (vec4  1.0  1.0  1.0 1.0)
       (vec4 -1.0  1.0  0.0 1.0)))

;; 每帧：清屏 → 上程序 → ★传新时间 → 绑 VAO → 画。
;; 注意顺序：glUniform1f 要在 glUseProgram 之后（uniform 位置属于某个程序）。
(define (draw)
  (define t (/ (- (current-inexact-milliseconds) start-ms) 1000.0))
  (glClearColor 0.10 0.12 0.20 1.0)
  (glClear GL_COLOR_BUFFER_BIT)
  (glUseProgram prog)
  (glUniform1f loc-time t)
  (glBindVertexArray vao)
  (glDrawArrays GL_TRIANGLES 0 6))

(define-values (frame canvas)
  (make-window #:title "05-02 圈圈动起来" #:width 400 #:height 400 #:draw draw))

;; 初始化：程序 + VAO + 查 uniform 位置（"uTime" 的位置号只查一次）
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
