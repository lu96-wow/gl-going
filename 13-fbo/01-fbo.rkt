#lang racket/base
;; =========================================================
;; 13-fbo/01-fbo.rkt —— 第一步：FBO 离屏渲染 + 两遍渲染
;; 运行：racket 13-fbo/01-fbo.rkt    点 X = 退出
;; =========================================================
;; 平时画进"默认帧缓冲"（编号 0 = 窗口）。本步自己拼一个"看不见的画布"，
;; 把场景先画进去，再把它当图片贴回窗口——这就是离屏渲染。
;;
;; 本步新增（2 组）：
;;   ① FBO —— 帧缓冲对象：自己拼的渲染目标（颜色纹理 + 深度渲染缓冲）
;;   ② 两遍渲染 —— 第一遍画进离屏纹理，第二遍把它整屏贴回窗口
;;
;; ★为什么要 FBO：默认只能画到窗口。很多效果需要"先把结果画成一张图"，
;;   再拿这张图做文章（滤镜、镜面、阴影）。FBO = 一个自定义画布，颜色写到
;;   一张纹理里（glFramebufferTexture2D 挂颜色槽），深度用渲染缓冲挂
;;   （纹理里没有深度，所以另配一块 glRenderbufferStorage）。
;;   glBindFramebuffer(fbo) 之后画的都进离屏；bind 回 0 就回窗口。
;;
;; ★两遍渲染：第一遍 3D 场景 → 离屏纹理；第二遍 整屏四边形采样这张纹理
;;   贴回窗口。屏幕每个像素 = 采样离屏纹理。本步"原样采样"，看不出变化，
;;   但离屏这条管线已经通了——下一步就能在每个像素上做滤镜。
;; =========================================================

(require "lib-gui.rkt")
(require "lib.rkt")

(define start-ms (current-inexact-milliseconds))

;; 场景 shader（画进离屏）
(define scene-vert
  (glsl (version 330 core)
        (layout (location 0) in vec3 aPos)
        (layout (location 1) in vec3 aColor)
        (uniform mat4 uMVP)
        (out vec3 vColor)
        (define (main) void
          (set! vColor aColor)
          (set! gl_Position (* uMVP (vec4 aPos 1.0))))))
(define scene-frag
  (glsl (version 330 core)
        (in vec3 vColor)
        (out vec4 FragColor)
        (define (main) void
          (set! FragColor (vec4 vColor 1.0)))))

;; 整屏 shader（把离屏纹理贴回）
(define screen-vert
  (glsl (version 330 core)
        (layout (location 0) in vec2 aPos)
        (layout (location 1) in vec2 aUV)
        (out vec2 vUV)
        (define (main) void
          (set! vUV aUV)
          (set! gl_Position (vec4 aPos 0.0 1.0)))))
(define screen-frag
  (glsl (version 330 core)
        (in vec2 vUV)
        (uniform sampler2D uScreen)
        (out vec4 FragColor)
        (define (main) void
          (set! FragColor (texture uScreen vUV)))))   ; 原样采样

;; 整屏四边形（pos + uv）
(define quad-verts
  (vec (vec4 -1.0 -1.0  0.0 0.0)
       (vec4  1.0 -1.0  1.0 0.0)
       (vec4  1.0  1.0  1.0 1.0)
       (vec4 -1.0  1.0  0.0 1.0)))
(define quad-idx (u16vector 0 1 2  0 2 3))

;; 离屏缓冲对象（box，因为窗口缩放时要重建）
(define fbo (box 0)) (define tex-color (box 0)) (define rbo-depth (box 0))
(define fbo-w (box 0)) (define fbo-h (box 0))

;; 建离屏缓冲（在 draw 里调用，size 变化时重建）
(define (make-offscreen! w h)
  (when (> (unbox fbo) 0)
    (glDeleteFramebuffers 1 (u32vector (unbox fbo)))
    (glDeleteTextures 1 (u32vector (unbox tex-color)))
    (glDeleteRenderbuffers 1 (u32vector (unbox rbo-depth))))
  ;; ① 颜色纹理（先不填数据，画布会往里画）
  (define tex (u32vector-ref (glGenTextures 1) 0))
  (glBindTexture GL_TEXTURE_2D tex)
  (glTexImage2D GL_TEXTURE_2D 0 GL_RGBA w h 0 GL_RGBA GL_UNSIGNED_BYTE #f)
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP_TO_EDGE)
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP_TO_EDGE)
  ;; ② 深度渲染缓冲
  (define rbo (u32vector-ref (glGenRenderbuffers 1) 0))
  (glBindRenderbuffer GL_RENDERBUFFER rbo)
  (glRenderbufferStorage GL_RENDERBUFFER GL_DEPTH_COMPONENT16 w h)
  ;; ③ 组装 FBO：颜色挂纹理、深度挂渲染缓冲，然后检查
  (define fb (u32vector-ref (glGenFramebuffers 1) 0))
  (glBindFramebuffer GL_FRAMEBUFFER fb)
  (glFramebufferTexture2D GL_FRAMEBUFFER GL_COLOR_ATTACHMENT0 GL_TEXTURE_2D tex 0)
  (glFramebufferRenderbuffer GL_FRAMEBUFFER GL_DEPTH_ATTACHMENT GL_RENDERBUFFER rbo)
  (define st (glCheckFramebufferStatus GL_FRAMEBUFFER))
  (glBindFramebuffer GL_FRAMEBUFFER 0)
  (when (not (= st GL_FRAMEBUFFER_COMPLETE)) (printf "FBO 不完整，状态 ~a~%" st))
  (set-box! fbo fb) (set-box! tex-color tex) (set-box! rbo-depth rbo))

(define (draw)
  (define t (/ (- (current-inexact-milliseconds) start-ms) 1000.0))
  (define-values (w h) (send canvas get-gl-client-size))
  ;; 窗口尺寸变了 → 重建离屏缓冲
  (when (or (not (= (unbox fbo-w) w)) (not (= (unbox fbo-h) h)))
    (make-offscreen! w h)
    (set-box! fbo-w w) (set-box! fbo-h h))

  (define aspect (/ (exact->inexact w) (exact->inexact h)))
  (define P (m4-perspective 45.0 aspect 0.1 100.0))
  (define V (m4-translate 0.0 0.0 -6.0))

  ;; ========== 第 1 遍：画进离屏纹理 ==========
  (glBindFramebuffer GL_FRAMEBUFFER (unbox fbo))
  (glViewport 0 0 w h)
  (glEnable GL_DEPTH_TEST)
  (glClearColor 0.30 0.25 0.35 1.0)
  (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
  (glUseProgram prog-scene)
  (define M (m4-mult (m4-mult (m4-rot-y (* t 60.0)) (m4-rot-x (* t 40.0)))
                     (m4-scale 0.6 0.6 0.6)))
  (glUniformMatrix4fv loc-mvp 1 #f (mat4 (m4-mult (m4-mult P V) M)))
  (glBindVertexArray vao-cube)
  (glDrawElements GL_TRIANGLES 36 GL_UNSIGNED_SHORT 0)

  ;; ========== 第 2 遍：离屏纹理整屏贴回窗口 ==========
  (glBindFramebuffer GL_FRAMEBUFFER 0)
  (glViewport 0 0 w h)
  (glDisable GL_DEPTH_TEST)
  (glUseProgram prog-screen)
  (glActiveTexture GL_TEXTURE0)
  (glBindTexture GL_TEXTURE_2D (unbox tex-color))
  (glUniform1i loc-tex 0)
  (glBindVertexArray vao-quad)
  (glDrawElements GL_TRIANGLES 6 GL_UNSIGNED_SHORT 0))

(define-values (frame canvas)
  (make-window #:title "13-01 FBO 离屏渲染" #:width 600 #:height 600 #:draw draw))

(define prog-scene  (send canvas with-gl-context (lambda () (build-program scene-vert scene-frag))))
(define prog-screen (send canvas with-gl-context (lambda () (build-program screen-vert screen-frag))))
(define loc-mvp (send canvas with-gl-context (lambda () (glGetUniformLocation prog-scene "uMVP"))))
(define loc-tex (send canvas with-gl-context (lambda () (glGetUniformLocation prog-screen "uScreen"))))

;; 场景立方体 VAO（复用 cube-verts/cube-idx）
(define vao-cube
  (send canvas with-gl-context
        (lambda ()
          (define vbo (u32vector-ref (glGenBuffers 1) 0))
          (glBindBuffer GL_ARRAY_BUFFER vbo)
          (glBufferData GL_ARRAY_BUFFER (gl-vector-sizeof cube-verts) cube-verts GL_STATIC_DRAW)
          (define v (u32vector-ref (glGenVertexArrays 1) 0))
          (glBindVertexArray v)
          (glVertexAttribPointer 0 (glsl-size 'vec3) GL_FLOAT #f (glsl-stride-bytes 'vec3 'vec3) 0)
          (glEnableVertexAttribArray 0)
          (glVertexAttribPointer 1 (glsl-size 'vec3) GL_FLOAT #f (glsl-stride-bytes 'vec3 'vec3) (glsl-stride-bytes 'vec3))
          (glEnableVertexAttribArray 1)
          (define ebo (u32vector-ref (glGenBuffers 1) 0))
          (glBindBuffer GL_ELEMENT_ARRAY_BUFFER ebo)
          (glBufferData GL_ELEMENT_ARRAY_BUFFER (gl-vector-sizeof cube-idx) cube-idx GL_STATIC_DRAW)
          (glBindVertexArray 0)
          v)))
;; 整屏四边形 VAO
(define vao-quad
  (send canvas with-gl-context
        (lambda ()
          (define vbo (u32vector-ref (glGenBuffers 1) 0))
          (glBindBuffer GL_ARRAY_BUFFER vbo)
          (glBufferData GL_ARRAY_BUFFER (gl-vector-sizeof (vec->f32vector quad-verts)) (vec->f32vector quad-verts) GL_STATIC_DRAW)
          (define v (u32vector-ref (glGenVertexArrays 1) 0))
          (glBindVertexArray v)
          (glVertexAttribPointer 0 (glsl-size 'vec2) GL_FLOAT #f (glsl-stride-bytes 'vec2 'vec2) 0)
          (glEnableVertexAttribArray 0)
          (glVertexAttribPointer 1 (glsl-size 'vec2) GL_FLOAT #f (glsl-stride-bytes 'vec2 'vec2) (glsl-stride-bytes 'vec2))
          (glEnableVertexAttribArray 1)
          (define ebo (u32vector-ref (glGenBuffers 1) 0))
          (glBindBuffer GL_ELEMENT_ARRAY_BUFFER ebo)
          (glBufferData GL_ELEMENT_ARRAY_BUFFER (gl-vector-sizeof quad-idx) quad-idx GL_STATIC_DRAW)
          (glBindVertexArray 0)
          v)))

(define ticker
  (new timer% (interval 16)
       (notify-callback (lambda () (send canvas refresh)))))

(send frame show #t)
