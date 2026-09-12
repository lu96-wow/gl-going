#lang racket/base
;; =========================================================
;; 04-animate/04-control.rkt —— 帧率控制（目标帧率 + 暂停）
;; 运行：racket 04-animate/04-control.rkt    空格 = 暂停/继续   点 X = 退出
;; =========================================================

;; 前两步让三角形动起来了。本步把"帧率"控制起来：
;;   ① 目标帧率：timer 的 interval 决定每秒最多画几帧（1000 / 帧率 = 毫秒）
;;   ② 暂停/继续：停掉/重启 timer，画面就不动 / 继续动
;;
;; 键盘空格切换暂停——所以本步自建窗口（要加 on-char 键盘钩子），
;; 顺便认识键盘输入（后面相机课还会用）。

(require racket/gui opengl)                 ; 窗口、画布、timer% + gl* 函数常量
(require "../racket-glsl/rewrite.rkt")
(require "../racket-glsl/rename-vector.rkt")
(require "../racket-glsl/tool.rkt")

(define start-ms (current-inexact-milliseconds))

;; 着色器 + 顶点数据（同 02/03）。
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

;; 帧率控制的两个状态：
(define interval-ms 16)   ; 目标 60 帧/秒（1000/60 ≈ 16.7ms）
(define paused? #f)       ; 是否暂停中

;; 键盘：空格切换暂停/继续。
;;   key-event% 的 get-key-code 返回按键符号（空格 = 'space）。
;;   timer 的 stop = 暂停；start interval = 继续（用同一个间隔重启）。
(define (on-key e)
  (when (eq? (send e get-key-code) 'space)
    (set! paused? (not paused?))
    (if paused?
        (begin (send ticker stop) (send frame set-label "04-04 已暂停（空格继续）"))
        (begin (send ticker start interval-ms) (send frame set-label "04-04 运行中（空格暂停）")))))

;; 每帧画的内容（同 02）。
(define (draw)
  (define t (/ (- (current-inexact-milliseconds) start-ms) 1000.0))
  (use-program prog)
  (glUniform1f loc-time t)
  (glBindVertexArray vao)
  (glDrawArrays GL_TRIANGLES 0 3))

;; 窗口（同 02 课：点 X 退出）。键盘钩子不在这里，在下面画布上。
(define frame
  (new (class frame%
         (augment* [on-close (lambda () (exit 0))])
         (super-new))
       (label "04-04 运行中（空格暂停）") (width 400) (height 400)))

;; 上下文配置 + 画布（同 02 课：on-size 视口 + on-paint 清屏/画/翻页）。
(define cfg (new gl-config%))
(send cfg set-legacy? #f)          ; core profile
(send cfg set-double-buffered #t)  ; 双缓冲

(define canvas
  (new (class canvas%
         (inherit with-gl-context swap-gl-buffers)
         ;; on-char：画布收到键盘事件时调用（key-event% 的 get-key-code 返回按键符号，
         ;; 空格 = 'space）。键盘钩子挂在画布上，而不是窗口上。
         (define/override (on-char e) (on-key e))
         (define/override (on-size w h)
           (with-gl-context
            (lambda ()
              (define-values (fw fh) (send this get-gl-client-size))
              (glViewport 0 0 fw fh))))
         (define/override (on-paint)
           (with-gl-context
            (lambda ()
              (glClearColor 0.10 0.12 0.20 1.0)
              (glClear GL_COLOR_BUFFER_BIT)
              (draw)
              (send this swap-gl-buffers))))
         (super-new))
       (style '(gl no-autoclear))
       (gl-config cfg)
       (parent frame)))

;; 初始化（同 02/03）。
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

;; 定时器（同 01）。暂停/继续由 on-key 里的 stop/start 控制。
(define ticker
  (new timer%
       (interval interval-ms)
       (notify-callback (lambda () (send canvas refresh)))))

(send frame show #t)
