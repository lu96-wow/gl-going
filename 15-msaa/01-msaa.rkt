#lang racket/base
;; =========================================================
;; 15-msaa/01-msaa.rkt —— 第一步：锯齿成因 + MSAA 原理
;; 运行：racket 15-msaa/01-msaa.rkt     M = MSAA 开/关   滚轮 = 拉近拉远   点X = 退出
;; =========================================================
;; 物体斜边在屏幕上出现"狗牙"。本步搞懂为什么，并用 MSAA 消掉它。
;;
;; 本步新增（2 个）：
;;   ① 锯齿成因 + MSAA 原理 —— 像素是格子，斜边切过格子；多个采样点投票
;;   ② glRenderbufferStorageMultisample + glBlitFramebuffer —— 多重采样缓冲 + 解析
;;
;; ★锯齿（aliasing）怎么来的：屏幕像素是方格子，几何斜边却从格子中间切过。
;;   片元着色器每个格子只算一次 → 整格染成一种颜色 → 斜边变成台阶状的"狗牙"。
;;
;; ★MSAA 思路：颜色只算一次，但"覆盖多少"用**多个采样点**投票——斜边经过的
;;   格子，4 个采样点里有几个落在三角形内，边缘颜色就按比例混合。开销只花在
;;   边缘格子，比"整幅 4 倍分辨率再缩小"便宜得多。
;;
;; ★怎么让锯齿"看得清"（本课场景设计的关键）：
;;   锯齿最明显的边 = **细 + 高对比 + 浅角度**。
;;     - 细：1px 的线、细长薄片，几乎"全是边"，台阶比例最大；
;;     - 高对比：亮色形状 + 近黑背景，台阶一眼可见；
;;     - 浅角度：边越接近水平/竖直，台阶的"台阶面"越长越显眼。
;;   ★注意：粗线会**掩盖**锯齿（台阶被填平），所以本课用 1px 细线、不用粗线。
;;
;; ★实现（建在 14 课 FBO 之上）：
;;   ① 颜色/深度都用"多重采样渲染缓冲"（glRenderbufferStorageMultisample，4×）
;;   ② 场景画进这个 4× FBO
;;   ③ glBlitFramebuffer 把多采样结果"解析 resolve"到窗口——拷的过程自动
;;      把 4 个子样本平均成 1 个像素（多采样缓冲不能直接上屏，必须 resolve）。
;;
;; 本步视觉：近黑背景上，两片细长 blade（薄三角形）交叉缓慢旋转，加一条
;;   静止的 1px 细斜线。按 M 切换 MSAA，盯住斜边看台阶变平滑。
;; =========================================================

(require "lib-gui.rkt")
(require "lib.rkt")

(define start-ms (current-inexact-milliseconds))
(define msaa-on? (box #t))
(define dist (box 6.0))   ; 相机距离（滚轮调，拉近看得更清）
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

;; 细长 blade：很薄的三角形（宽 0.1、长 3.4），几乎"全是边"→ 锯齿最大化
(define (blade-verts color)
  (concat-vecs (vec3 -0.05 -1.7 0.0) color
               (vec3  0.05 -1.7 0.0) color
               (vec3  0.0   1.7 0.0) color))
(define blade1 (blade-verts (vec3 0.35 0.85 1.0)))   ; 蓝
(define blade2 (blade-verts (vec3 1.00 0.70 0.30)))  ; 橙
;; 一条 1px 细斜线（静止、浅角度 → 最明显的"台阶"）
(define line-verts
  (concat-vecs (vec3 -1.8 0.6 0.0) (vec3 1.0 1.0 1.0)
               (vec3  1.8 -0.3 0.0) (vec3 1.0 1.0 1.0)))

;; MSAA 缓冲（box，窗口缩放时重建）
(define fbo-ms (box 0)) (define rbo-color (box 0)) (define rbo-depth (box 0))
(define fbo-w (box 0)) (define fbo-h (box 0))

(define (make-msaa! w h)
  (when (> (unbox fbo-ms) 0)
    (glDeleteFramebuffers 1 (u32vector (unbox fbo-ms)))
    (glDeleteRenderbuffers 1 (u32vector (unbox rbo-color)))
    (glDeleteRenderbuffers 1 (u32vector (unbox rbo-depth))))
  (define rc (u32vector-ref (glGenRenderbuffers 1) 0))
  (glBindRenderbuffer GL_RENDERBUFFER rc)
  (glRenderbufferStorageMultisample GL_RENDERBUFFER SAMPLES GL_RGBA8 w h)   ; ★4× 颜色
  (define rd (u32vector-ref (glGenRenderbuffers 1) 0))
  (glBindRenderbuffer GL_RENDERBUFFER rd)
  (glRenderbufferStorageMultisample GL_RENDERBUFFER SAMPLES GL_DEPTH_COMPONENT24 w h) ; ★4× 深度
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
      ;; 滚轮是键盘事件（key-event%），不是鼠标事件
      [(eq? code 'wheel-up)   (set-box! dist (max 3.0 (min 15.0 (* (unbox dist) 0.9))))]
      [(eq? code 'wheel-down) (set-box! dist (max 3.0 (min 15.0 (* (unbox dist) 1.1))))]
      [(eq? code 'escape) (exit 0)])))

;; 画场景：两片细长 blade 交叉慢转 + 一条静止 1px 细斜线
(define (draw-scene)
  (define t (/ (- (current-inexact-milliseconds) start-ms) 1000.0))
  (define-values (w h) (send canvas get-gl-client-size))
  (define aspect (/ (exact->inexact w) (exact->inexact h)))
  (define P (mat4-perspective 45.0 aspect 0.1 100.0))
  (define V (mat4-translate 0.0 0.0 (- (unbox dist))))
  (define ang (* t 25.0))    ; 缓慢转 → 台阶稳定可见

  (glClearColor 0.02 0.02 0.05 1.0)   ; 近黑背景 → 高对比
  (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
  (glUseProgram prog)
  ;; 两片 blade，绕 z 转（在屏幕平面内转），互相垂直
  (glBindVertexArray vao-blade1)
  (glUniformMatrix4fv loc-mvp 1 #f (mat4-mult (mat4-mult P V) (mat4-rot-z ang)))
  (glDrawArrays GL_TRIANGLES 0 3)
  (glBindVertexArray vao-blade2)
  (glUniformMatrix4fv loc-mvp 1 #f (mat4-mult (mat4-mult P V) (mat4-rot-z (+ ang 90.0))))
  (glDrawArrays GL_TRIANGLES 0 3)
  ;; 1px 细斜线（静止，浅角度 → 最明显的台阶）
  (glBindVertexArray vao-line)
  (glLineWidth 1.0)
  (glUniformMatrix4fv loc-mvp 1 #f (mat4-mult P V))
  (glDrawArrays GL_LINES 0 2))

(define (draw)
  (define-values (w h) (send canvas get-gl-client-size))
  (when (or (not (= (unbox fbo-w) w)) (not (= (unbox fbo-h) h)))
    (make-msaa! w h)
    (set-box! fbo-w w) (set-box! fbo-h h))

  (if (unbox msaa-on?)
      ;; MSAA：画进 4× 缓冲，再解析到窗口
      (begin
        (glBindFramebuffer GL_FRAMEBUFFER (unbox fbo-ms))
        (glViewport 0 0 w h)
        (glEnable GL_DEPTH_TEST)
        (draw-scene)
        (glBindFramebuffer GL_READ_FRAMEBUFFER (unbox fbo-ms))
        (glBindFramebuffer GL_DRAW_FRAMEBUFFER 0)
        (glBlitFramebuffer 0 0 w h  0 0 w h  GL_COLOR_BUFFER_BIT GL_NEAREST) ; ★解析
        (glBindFramebuffer GL_FRAMEBUFFER 0))
      ;; 关：直接画窗口（单采样）
      (begin
        (glBindFramebuffer GL_FRAMEBUFFER 0)
        (glViewport 0 0 w h)
        (glEnable GL_DEPTH_TEST)
        (draw-scene))))

(define-values (frame canvas)
  (make-window #:title "15-01 MSAA（M 切换）"
               #:width 800 #:height 600 #:draw draw #:on-char on-char))

(define prog (send canvas with-gl-context (lambda () (build-program (GL_VERTEX_SHADER vert-src) (GL_FRAGMENT_SHADER frag-src)))))
(define loc-mvp (send canvas with-gl-context (lambda () (glGetUniformLocation prog "uMVP"))))

;; 三个小 VAO：blade1 / blade2 / line（都是 pos+color，stride 24）
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
(define vao-line   (make-vao line-verts))

(define ticker
  (new timer% (interval 16)
       (notify-callback (lambda () (send canvas refresh)))))

(send frame show #t)
(send canvas focus)
