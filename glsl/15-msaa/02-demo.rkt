#lang racket/base
;; =========================================================
;; 15-msaa/02-demo.rkt —— 第二步：综合，抗锯齿演示
;; 运行：racket glsl/15-msaa/02-demo.rkt     M = MSAA 开/关   滚轮 = 拉近拉远   点X = 退出
;; =========================================================
;; 上一步：学会了 MSAA 原理和 M 切换。本步**不引入新语法**，把场景做丰富：
;; 多片细长 blade + 多条 1px 细线，让不同角度的斜边同时暴露锯齿。
;; 设计要点（锯齿 = 细 + 高对比 + 浅角度）：近黑背景、亮色薄片、1px 细线。
;; =========================================================

(require "lib-gui.rkt")
(require "lib.rkt")

(define start-ms (current-inexact-milliseconds))
(define msaa-on? (box #t))
(define dist (box 6.0))   ; 相机距离（滚轮调）
(define SAMPLES 4)

(define vert-src
  (glsl (version 330 core)
        (layout (location 0) in vec3 aPos)
        (layout (location 1) in vec3 aColor)
        (uniform mat4 uMVP)
        (out vec3 vColor)
        (define (main) void
          (set! vColor aColor)
          (set! gl_Position (* uMVP (vec4 aPos 1.0))))))
(define frag-src
  (glsl (version 330 core)
        (in vec3 vColor)
        (out vec4 FragColor)
        (define (main) void
          (set! FragColor (vec4 vColor 1.0)))))

;; 细长 blade（薄三角形）+ 1px 细线
(define (blade-verts color)
  (concat-vecs (vec3 -0.05 -1.7 0.0) color
               (vec3  0.05 -1.7 0.0) color
               (vec3  0.0   1.7 0.0) color))
(define blade1 (blade-verts (vec3 0.35 0.85 1.0)))   ; 蓝
(define blade2 (blade-verts (vec3 1.00 0.70 0.30)))  ; 橙
(define blade3 (blade-verts (vec3 0.55 0.95 0.50)))  ; 绿
(define (line-verts x1 y1 x2 y2)
  (concat-vecs (vec3 x1 y1 0.0) (vec3 1.0 1.0 1.0)
               (vec3 x2 y2 0.0) (vec3 1.0 1.0 1.0)))
(define line1 (line-verts -1.8  0.6  1.8 -0.3))   ; 浅角度
(define line2 (line-verts -1.8 -0.6  1.8  0.6))   ; 另一条，交叉

(define fbo-ms (box 0)) (define rbo-color (box 0)) (define rbo-depth (box 0))
(define fbo-w (box 0)) (define fbo-h (box 0))

(define (make-msaa! w h)
  (when (> (unbox fbo-ms) 0)
    (glDeleteFramebuffers 1 (u32vector (unbox fbo-ms)))
    (glDeleteRenderbuffers 1 (u32vector (unbox rbo-color)))
    (glDeleteRenderbuffers 1 (u32vector (unbox rbo-depth))))
  (define rc (u32vector-ref (glGenRenderbuffers 1) 0))
  (glBindRenderbuffer GL_RENDERBUFFER rc)
  (glRenderbufferStorageMultisample GL_RENDERBUFFER SAMPLES GL_RGBA8 w h)
  (define rd (u32vector-ref (glGenRenderbuffers 1) 0))
  (glBindRenderbuffer GL_RENDERBUFFER rd)
  (glRenderbufferStorageMultisample GL_RENDERBUFFER SAMPLES GL_DEPTH_COMPONENT24 w h)
  (define fb (u32vector-ref (glGenFramebuffers 1) 0))
  (glBindFramebuffer GL_FRAMEBUFFER fb)
  (glFramebufferRenderbuffer GL_FRAMEBUFFER GL_COLOR_ATTACHMENT0 GL_RENDERBUFFER rc)
  (glFramebufferRenderbuffer GL_FRAMEBUFFER GL_DEPTH_ATTACHMENT GL_RENDERBUFFER rd)
  (when (not (= (glCheckFramebufferStatus GL_FRAMEBUFFER) GL_FRAMEBUFFER_COMPLETE))
    (printf "MSAA FBO 不完整~%"))
  (glBindFramebuffer GL_FRAMEBUFFER 0)
  (set-box! fbo-ms fb) (set-box! rbo-color rc) (set-box! rbo-depth rd))

(define (on-char e)
  (define code (send e get-key-code))
  (when (not (eq? code 'release))
    (cond
      [(or (eq? code #\m) (eq? code #\M))
       (set-box! msaa-on? (not (unbox msaa-on?)))
       (printf (if (unbox msaa-on?) "MSAA 开（4× 平滑）~%" "MSAA 关（看锯齿）~%"))]
      [(eq? code 'wheel-up)   (set-box! dist (max 3.0 (min 15.0 (* (unbox dist) 0.9))))]
      [(eq? code 'wheel-down) (set-box! dist (max 3.0 (min 15.0 (* (unbox dist) 1.1))))]
      [(eq? code 'escape) (exit 0)])))

(define (draw-scene)
  (define t (/ (- (current-inexact-milliseconds) start-ms) 1000.0))
  (define-values (w h) (send canvas get-gl-client-size))
  (define aspect (/ (exact->inexact w) (exact->inexact h)))
  (define P (mat4-perspective 45.0 aspect 0.1 100.0))
  (define V (mat4-translate 0.0 0.0 (- (unbox dist))))
  (define ang (* t 25.0))

  (glClearColor 0.02 0.02 0.05 1.0)
  (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
  (glUseProgram prog)
  ;; 三片 blade 绕 z 转，夹角各 60°
  (define (blade vao deg)
    (glBindVertexArray vao)
    (glUniformMatrix4fv loc-mvp 1 #f (mat4-mult (mat4-mult P V) (mat4-rot-z (+ ang deg))))
    (glDrawArrays GL_TRIANGLES 0 3))
  (blade vao-blade1 0.0)
  (blade vao-blade2 60.0)
  (blade vao-blade3 120.0)
  ;; 两条静止 1px 细线（浅角度 → 常驻"台阶"）
  (glLineWidth 1.0)
  (glBindVertexArray vao-line1)
  (glUniformMatrix4fv loc-mvp 1 #f (mat4-mult P V))
  (glDrawArrays GL_LINES 0 2)
  (glBindVertexArray vao-line2)
  (glDrawArrays GL_LINES 0 2))

(define (draw)
  (define-values (w h) (send canvas get-gl-client-size))
  (when (or (not (= (unbox fbo-w) w)) (not (= (unbox fbo-h) h)))
    (make-msaa! w h)
    (set-box! fbo-w w) (set-box! fbo-h h))

  (if (unbox msaa-on?)
      (begin
        (glBindFramebuffer GL_FRAMEBUFFER (unbox fbo-ms))
        (glViewport 0 0 w h)
        (glEnable GL_DEPTH_TEST)
        (draw-scene)
        (glBindFramebuffer GL_READ_FRAMEBUFFER (unbox fbo-ms))
        (glBindFramebuffer GL_DRAW_FRAMEBUFFER 0)
        (glBlitFramebuffer 0 0 w h  0 0 w h  GL_COLOR_BUFFER_BIT GL_NEAREST)
        (glBindFramebuffer GL_FRAMEBUFFER 0))
      (begin
        (glBindFramebuffer GL_FRAMEBUFFER 0)
        (glViewport 0 0 w h)
        (glEnable GL_DEPTH_TEST)
        (draw-scene))))

(define-values (frame canvas)
  (make-window #:title "15-02 抗锯齿（综合）"
               #:width 800 #:height 600 #:draw draw #:on-char on-char))

(define prog (send canvas with-gl-context (lambda () (build-program (GL_VERTEX_SHADER vert-src) (GL_FRAGMENT_SHADER frag-src)))))
(define loc-mvp (send canvas with-gl-context (lambda () (glGetUniformLocation prog "uMVP"))))

(define (make-vao verts)
  (send canvas with-gl-context
        (lambda ()
          (define vbo (u32vector-ref (glGenBuffers 1) 0))
          (glBindBuffer GL_ARRAY_BUFFER vbo)
          (glBufferData GL_ARRAY_BUFFER (gl-vector-sizeof verts) verts GL_STATIC_DRAW)
          (define v (u32vector-ref (glGenVertexArrays 1) 0))
          (glBindVertexArray v)
          (glVertexAttribPointer 0 (glsl-size 'vec3) GL_FLOAT #f (glsl-stride-bytes 'vec3 'vec3) 0)
          (glEnableVertexAttribArray 0)
          (glVertexAttribPointer 1 (glsl-size 'vec3) GL_FLOAT #f (glsl-stride-bytes 'vec3 'vec3) (glsl-stride-bytes 'vec3))
          (glEnableVertexAttribArray 1)
          (glBindVertexArray 0)
          v)))
(define vao-blade1 (make-vao blade1))
(define vao-blade2 (make-vao blade2))
(define vao-blade3 (make-vao blade3))
(define vao-line1  (make-vao line1))
(define vao-line2  (make-vao line2))

(define ticker
  (new timer% (interval 16)
       (notify-callback (lambda () (send canvas refresh)))))

(send frame show #t)
(send canvas focus)
