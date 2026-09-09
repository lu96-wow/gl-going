#lang racket/base
;; =========================================================
;; 04-uniform-time.rkt —— uniform：每帧从 CPU 传参给着色器
;; 运行：racket 04-uniform-time.rkt    ESC/点X = 退出
;; =========================================================
;; 上一课(03)：数据（VBO/EBO）与画法（图元/索引）。
;; 本课新概念：uniform。它是"一次 draw 里对所有顶点/片元都不变的参数"，
;; 和 attribute（逐顶点、跟着顶点数据走）正好互补：
;;
;;   attribute：每个顶点一份   → 存在 VBO 里，顶点着色器每顶点取一次
;;   uniform  ：每次 draw 一份 → 用 glUniformXf 从 CPU 上传，本次 draw 通吃
;;
;; 新 API（就 3 个）：
;;   glGetUniformLocation(prog, "名字")    → 查 uniform 的位置
;;   glUniform1f / glUniform3f / …          → 上传 1 个/3 个 float
;;   （流程 = 拿位置 → glUseProgram → 每帧改 uniform → draw）
;;
;; 图形原理（动画是怎么来的）：
;;   GL 本身不知道"时间"。Racket 每帧量一个时间 t（current-inexact-milliseconds），
;;   把 sin/cos 算好再 glUniform 传进着色器——数据在 CPU 上流动，
;;   着色器只是"读参数"的纯函数。这就是所有游戏循环里"CPU 驱动 GPU"
;;   的最小例子。这次 draw 用 uTime，下次 draw 再传新 uTime，画面就活了。
;;
;; 演示：同一个"方块"网格（02 的 EBO 四边形）画两次：
;;   左   ：uOffset 上下弹跳 + uColor 随 t 变色（每帧在 Racket 里算好传入）
;;   右   ：静止的对照（颜色不变）
;; 注意两个 draw 之间 glUniform 是"独立重新设置"的——一个 uniform
;; 的值只属于它之后的那次 draw，这正是"每次 draw 一份"的意思。
;; =========================================================

(require racket/gui opengl)
(require "lib.rkt")

(define PI (acos -1.0))
(define start-ms (current-inexact-milliseconds))

;; 顶点着色器：
;;   uniform vec2 uOffset   本次 draw 全体共用的平移量（CPU 用 glUniform2f 传）
;;   vColor 填个占位值即可 —— 真正颜色在片元里从 uniform 取
(define vert-src
  (glsl-pretty
   (glsl
    (version 330 core)
    (layout (location 0) in vec2 aPos)
    (uniform vec2 uOffset)
    (out vec3 vColor)
    (define (main) void
      (set! vColor (vec3 0.0))
      (set! gl_Position (vec4 (+ aPos uOffset) 0.0 1.0))))))

;; 片元着色器：颜色不来自顶点，而来自 uniform（本次 draw 共用的一个值）
(define frag-src
  (glsl-pretty
   (glsl
    (version 330 core)
    (in vec3 vColor)
    (uniform vec3 uColor)
    (out vec4 FragColor)
    (define (main) void
      (set! FragColor (vec4 uColor 1.0))))))

(define cfg (new gl-config%))
(send cfg set-legacy? #f)
(send cfg set-double-buffered #t)
(define frame
  (new (class frame%
         (augment* [on-close (lambda () (exit 0))])
         (super-new))
       (label "04 uniform 与时间") (width 600) (height 600)))

(define init? (box #f))
(define prog #f)
(define vao 0)
(define loc-offset 0) (define loc-color 0)

(define canvas
  (new (class canvas%
         (inherit with-gl-context swap-gl-buffers)
         (define/override (on-size w h)
           (with-gl-context
            (lambda ()
              (define-values (fw fh) (send this get-gl-client-size))
              (glViewport 0 0 fw fh)
              (glClearColor 0.07 0.08 0.14 1.0))))
         (define/override (on-char e)
           (when (eq? (send e get-key-code) 'escape) (exit 0)))
         (define/override (on-paint)
           (with-gl-context
            (lambda ()
              (unless (unbox init?)
                (set-box! init? #t)
                (set! prog (build-program vert-src frag-src))
                (set! loc-offset (glGetUniformLocation prog "uOffset"))
                (set! loc-color  (glGetUniformLocation prog "uColor"))
                ;; 方块网格：EBO 4 顶点 + 6 索引（02 同款）
                (define verts (f32vector -0.30 -0.30   0.30 -0.30
                                         0.30  0.30  -0.30  0.30))
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
              (glClear GL_COLOR_BUFFER_BIT)
              (glUseProgram prog)
              (glBindVertexArray vao)

              ;; ---- 左：弹跳方块，参数每帧在 Racket 算好传进去 ----
              (glUniform2f loc-offset -0.45 (* 0.35 (abs (sin (* t 2.0)))))
              (glUniform3f loc-color (+ 0.5 (* 0.5 (cos (* t 1.3))))
                           (+ 0.5 (* 0.5 (sin (* t 0.9))))
                           0.9)
              (glDrawElements GL_TRIANGLES 6 GL_UNSIGNED_SHORT 0)

              ;; ---- 右：静止对照（同一程序，同一 VAO，只改了 uniform）----
              (glUniform2f loc-offset 0.45 -0.05)
              (glUniform3f loc-color 0.45 0.70 0.95)
              (glDrawElements GL_TRIANGLES 6 GL_UNSIGNED_SHORT 0)

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
