#lang racket/base
;; =========================================================
;; 13-fbo/03-demo.rkt —— 第三步：综合，离屏渲染 + 滤镜场景
;; 运行：racket 13-fbo/03-demo.rkt    1/2/3/4 = 原图/反色/灰度/暗角   点X = 退出
;; =========================================================
;; 本课前两步：FBO 离屏(01)、后处理滤镜(02)。
;; 本步**不引入新语法**，把老教程 12-fbo 的成品拼出来。
;;
;; 场景：中央翻滚立方体 + 两颗绕行小立方体，画进离屏纹理；再整屏贴回时
;;   按 1/2/3/4 切滤镜。"画到纹理再贴回去"这个套路 = 一切后处理的地基。
;; =========================================================

(require "lib-gui.rkt")
(require "lib.rkt")

(define PI (acos -1.0))
(define start-ms (current-inexact-milliseconds))
(define mode (box 0))

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
        (uniform int uMode)
        (out vec4 FragColor)
        (define (main) void
          (vec3 c (rgb (texture uScreen vUV)))
          (cond
            [(= uMode 1) (set! c (- 1.0 c))]
            [(= uMode 2)
             (float g (dot c (vec3 0.299 0.587 0.114)))
             (set! c (vec3 g))]
            [(= uMode 3)
             (set! c (* c (- 1.0 (* 0.55 (length (- vUV 0.5))))))])
          (set! FragColor (vec4 c 1.0)))))

(define quad-verts
  (vec (vec4 -1.0 -1.0  0.0 0.0)
       (vec4  1.0 -1.0  1.0 0.0)
       (vec4  1.0  1.0  1.0 1.0)
       (vec4 -1.0  1.0  0.0 1.0)))
(define quad-idx (u16vector 0 1 2  0 2 3))

(define fbo (box 0)) (define tex-color (box 0)) (define rbo-depth (box 0))
(define fbo-w (box 0)) (define fbo-h (box 0))

(define (make-offscreen! w h)
  (when (> (unbox fbo) 0)
    (glDeleteFramebuffers 1 (u32vector (unbox fbo)))
    (glDeleteTextures 1 (u32vector (unbox tex-color)))
    (glDeleteRenderbuffers 1 (u32vector (unbox rbo-depth))))
  (define tex (u32vector-ref (glGenTextures 1) 0))
  (glBindTexture GL_TEXTURE_2D tex)
  (glTexImage2D GL_TEXTURE_2D 0 GL_RGBA w h 0 GL_RGBA GL_UNSIGNED_BYTE #f)
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP_TO_EDGE)
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP_TO_EDGE)
  (define rbo (u32vector-ref (glGenRenderbuffers 1) 0))
  (glBindRenderbuffer GL_RENDERBUFFER rbo)
  (glRenderbufferStorage GL_RENDERBUFFER GL_DEPTH_COMPONENT16 w h)
  (define fb (u32vector-ref (glGenFramebuffers 1) 0))
  (glBindFramebuffer GL_FRAMEBUFFER fb)
  (glFramebufferTexture2D GL_FRAMEBUFFER GL_COLOR_ATTACHMENT0 GL_TEXTURE_2D tex 0)
  (glFramebufferRenderbuffer GL_FRAMEBUFFER GL_DEPTH_ATTACHMENT GL_RENDERBUFFER rbo)
  (define st (glCheckFramebufferStatus GL_FRAMEBUFFER))
  (glBindFramebuffer GL_FRAMEBUFFER 0)
  (when (not (= st GL_FRAMEBUFFER_COMPLETE)) (printf "FBO 不完整 ~a~%" st))
  (set-box! fbo fb) (set-box! tex-color tex) (set-box! rbo-depth rbo))

(define (on-char e)
  (define code (send e get-key-code))
  (when (not (eq? code 'release))
    (cond
      [(and (char? code) (char<=? #\1 code #\4))
       (set-box! mode (- (char->integer code) (char->integer #\1)))
       (printf "滤镜 ~a~%" (+ (unbox mode) 1))]
      [(eq? code 'escape) (exit 0)])))

(define (draw)
  (define t (/ (- (current-inexact-milliseconds) start-ms) 1000.0))
  (define-values (w h) (send canvas get-gl-client-size))
  (when (or (not (= (unbox fbo-w) w)) (not (= (unbox fbo-h) h)))
    (make-offscreen! w h)
    (set-box! fbo-w w) (set-box! fbo-h h))

  (define aspect (/ (exact->inexact w) (exact->inexact h)))
  (define P (m4-perspective 45.0 aspect 0.1 100.0))
  (define V (m4-translate 0.0 0.0 -6.0))

  (glBindFramebuffer GL_FRAMEBUFFER (unbox fbo))
  (glViewport 0 0 w h)
  (glEnable GL_DEPTH_TEST)
  (glClearColor 0.30 0.25 0.35 1.0)
  (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
  (glUseProgram prog-scene)
  (define (draw-cube m)
    (glUniformMatrix4fv loc-mvp 1 #f (mat4 (m4-mult (m4-mult P V) m)))
    (glBindVertexArray vao-cube)
    (glDrawElements GL_TRIANGLES 36 GL_UNSIGNED_SHORT 0))
  ;; 中央翻滚 + 两颗绕行
  (draw-cube (m4-mult (m4-mult (m4-rot-y (* t 60.0)) (m4-rot-x (* t 40.0)))
                      (m4-scale 0.6 0.6 0.6)))
  (for ([k (in-range 2)])
    (define a (* (/ PI 180.0) (+ (* k 180.0) (* t 80.0))))
    (draw-cube (m4-mult (m4-translate (* 2.2 (cos a)) 0.0 (- (* 2.2 (sin a))))
                        (m4-mult (m4-rot-y (* t -90.0)) (m4-scale 0.4 0.4 0.4)))))

  (glBindFramebuffer GL_FRAMEBUFFER 0)
  (glViewport 0 0 w h)
  (glDisable GL_DEPTH_TEST)
  (glUseProgram prog-screen)
  (glActiveTexture GL_TEXTURE0)
  (glBindTexture GL_TEXTURE_2D (unbox tex-color))
  (glUniform1i loc-tex 0)
  (glUniform1i loc-mode (unbox mode))
  (glBindVertexArray vao-quad)
  (glDrawElements GL_TRIANGLES 6 GL_UNSIGNED_SHORT 0))

(define-values (frame canvas)
  (make-window #:title "13-03 离屏渲染 + 滤镜（综合）"
               #:width 800 #:height 600 #:draw draw #:on-char on-char))

(define prog-scene  (send canvas with-gl-context (lambda () (build-program scene-vert scene-frag))))
(define prog-screen (send canvas with-gl-context (lambda () (build-program screen-vert screen-frag))))
(define loc-mvp  (send canvas with-gl-context (lambda () (glGetUniformLocation prog-scene "uMVP"))))
(define loc-tex  (send canvas with-gl-context (lambda () (glGetUniformLocation prog-screen "uScreen"))))
(define loc-mode (send canvas with-gl-context (lambda () (glGetUniformLocation prog-screen "uMode"))))

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
(send canvas focus)
