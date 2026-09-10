#lang racket/base
;; =========================================================
;; 12-fbo.rkt —— 离屏渲染（FBO）+ 后处理滤镜
;; 运行：racket 12-fbo.rkt   按键 1/2/3/4 = 原图/反色/灰度/暗角   ESC=退出
;; =========================================================
;; 平时我们画进"默认帧缓冲"(0)，它直接对应窗口。本课新 API 让它拐个弯：
;;
;;   ① 三个新对象，自己拼一个"看不见的画布"：
;;      glGenFramebuffers     FBO = 帧缓冲对象（画布本体）
;;      glFramebufferTexture2D   把一张纹理挂到 FBO 的颜色槽
;;      glRenderbufferStorage+glFramebufferRenderbuffer
;;                             深度用渲染缓冲挂（纹理里没有深度）
;;      glCheckFramebufferStatus  验证拼装完整
;;   ② glBindFramebuffer(GL_FRAMEBUFFER, fbo)：之后画的内容进 FBO
;;      glBindFramebuffer(GL_FRAMEBUFFER, 0)：回到窗口
;;
;; 图形原理（后处理 = 你已经在 08 学的"贴图"反过来用）：
;;   第一步：3D 场景画进离屏纹理（看不见）；
;;   第二步：把这张纹理当普通图片整屏贴回窗口——屏幕的每个像素 = 采样
;;   离屏纹理。于是在片元着色器里可以随意改每个像素 → 滤镜。
;;   "画到纹理再贴回去"是渲染到阴影贴图、镜面反射、屏幕后期等一切
;;   高级技巧的公共地基。
;;
;; 窗口大小一变 → 离屏缓冲要删掉重建（尺寸变了）。
;; =========================================================

(require racket/gui opengl)
(require "lib.rkt")

(define PI (acos -1.0))
(define start-ms (current-inexact-milliseconds))
(define mode (box 0))

(define scene-vert
  (glsl-pretty
   (glsl
    (version 330 core)
    (layout (location 0) in vec3 aPos)
    (layout (location 1) in vec3 aColor)
    (uniform mat4 uMVP)
    (out vec3 vColor)
    (define (main) void
      (set! vColor aColor)
      (set! gl_Position (* uMVP (vec4 aPos 1.0)))))))
(define scene-frag
  (glsl-pretty
   (glsl
    (version 330 core)
    (in vec3 vColor)
    (out vec4 FragColor)
    (define (main) void
      (set! FragColor (vec4 vColor 1.0))))))
(define screen-vert
  (glsl-pretty
   (glsl
    (version 330 core)
    (layout (location 0) in vec2 aPos)
    (layout (location 1) in vec2 aUV)
    (out vec2 vUV)
    (define (main) void
      (set! vUV aUV)
      (set! gl_Position (vec4 aPos 0.0 1.0))))))
;; 后处理片元：uMode 切滤镜——0 原图 1 反色 2 灰度 3 暗角。
;; 每个分支都是"对单个像素颜色 c 做一次改写"：把离屏纹理当普通图片
;; 贴回屏幕时，顺手在每个像素上改色，就是滤镜。
(define screen-frag
  (glsl-pretty
   (glsl
    (version 330 core)
    (in vec2 vUV)
    (uniform sampler2D uScreen)
    (uniform int uMode)
    (out vec4 FragColor)
    (define (main) void
      (vec3 c (rgb (texture uScreen vUV)))
      (cond [(= uMode 1) (set! c (- 1.0 c))]
            [(= uMode 2)
             (float g (dot c (vec3 0.299 0.587 0.114)))
             (set! c (vec3 g))]
            [(= uMode 3)
             (*= c (- 1.0 (* 0.55 (length (- vUV 0.5)))))])
      (set! FragColor (vec4 c 1.0))))))

(define cfg (new gl-config%))
(send cfg set-legacy? #f)
(send cfg set-double-buffered #t)
(send cfg set-depth-size 1)
(define frame
  (new (class frame%
         (augment* [on-close (lambda () (exit 0))])
         (super-new))
       (label "12 FBO 后处理") (width 800) (height 600)))

(define fw (box 800)) (define fh (box 600))
(define init? (box #f))
(define prog-scene #f) (define prog-screen #f)
(define vao-cube 0) (define vao-quad 0)
(define loc-mvp 0) (define loc-tex 0) (define loc-mode 0)
(define fbo (box 0)) (define tex-color (box 0)) (define rbo-depth (box 0))

(define canvas
  (new (class canvas%
         (inherit with-gl-context swap-gl-buffers)
         (define (make-offscreen! w h)
           ;; 旧的先删（否则窗口尺寸变化不生效）
           (when (> (unbox fbo) 0)
             (glDeleteFramebuffers 1 (u32vector (unbox fbo)))
             (glDeleteTextures 1 (u32vector (unbox tex-color)))
             (glDeleteRenderbuffers 1 (u32vector (unbox rbo-depth))))
           ;; ① 颜色纹理（先不填数据，画布会往里面画）
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
           ;; ③ 组装 FBO 并检查
           (define fb (u32vector-ref (glGenFramebuffers 1) 0))
           (glBindFramebuffer GL_FRAMEBUFFER fb)
           (glFramebufferTexture2D GL_FRAMEBUFFER GL_COLOR_ATTACHMENT0 GL_TEXTURE_2D tex 0)
           (glFramebufferRenderbuffer GL_FRAMEBUFFER GL_DEPTH_ATTACHMENT GL_RENDERBUFFER rbo)
           (define st (glCheckFramebufferStatus GL_FRAMEBUFFER))
           (glBindFramebuffer GL_FRAMEBUFFER 0)
           (when (not (= st GL_FRAMEBUFFER_COMPLETE))
             (printf "FBO 不完整，状态码 ~a~%" st))
           (set-box! fbo fb) (set-box! tex-color tex) (set-box! rbo-depth rbo))
         (define/override (on-size w h)
           (with-gl-context
            (lambda ()
              (define-values (gw gh) (send this get-gl-client-size))
              (set-box! fw gw) (set-box! fh gh)
              (glViewport 0 0 gw gh)
              (make-offscreen! gw gh))))
         (define/override (on-char e)
           (define code (send e get-key-code))
           (when (not (eq? code 'release))
             (cond
               [(eq? code 'escape) (exit 0)]
               [(and (char? code) (char<=? #\1 code #\4))
                (set-box! mode (- (char->integer code) (char->integer #\1)))
                (printf "滤镜 ~a~%" (+ (unbox mode) 1))])))
         (define/override (on-paint)
           (with-gl-context
            (lambda ()
              (unless (unbox init?)
                (set-box! init? #t)
                (set! prog-scene  (build-program scene-vert scene-frag))
                (set! prog-screen (build-program screen-vert screen-frag))
                (set! loc-mvp  (glGetUniformLocation prog-scene "uMVP"))
                (set! loc-tex  (glGetUniformLocation prog-screen "uScreen"))
                (set! loc-mode (glGetUniformLocation prog-screen "uMode"))
                ;; 立方体（同 05 数据）
                (define pos8
                  '((-1.0 -1.0  1.0) ( 1.0 -1.0  1.0) ( 1.0  1.0  1.0) (-1.0  1.0  1.0)
                    (-1.0 -1.0 -1.0) ( 1.0 -1.0 -1.0) ( 1.0  1.0 -1.0) (-1.0  1.0 -1.0)))
                (define faces
                  (list (list 0.85 0.20 0.20 '(0 1 2 3)) (list 0.20 0.80 0.25 '(5 4 7 6))
                        (list 0.95 0.60 0.10 '(1 5 6 2)) (list 0.95 0.85 0.15 '(4 0 3 7))
                        (list 0.20 0.60 0.95 '(3 2 6 7)) (list 0.75 0.30 0.90 '(4 5 1 0))))
                (define verts
                  (apply f32vector
                         (apply append
                                (for/list ([f faces])
                                  (apply append
                                         (for/list ([j (in-range 4)])
                                           (define p (list-ref pos8 (list-ref (list-ref f 3) j)))
                                           (list (car p) (cadr p) (caddr p)
                                                 (car f) (cadr f) (caddr f))))))))
                (define idx
                  (apply u16vector
                         (apply append
                                (for/list ([i (in-range 6)])
                                  (define b (* i 4))
                                  (list b (+ b 1) (+ b 2) b (+ b 2) (+ b 3))))))
                (define cv (u32vector-ref (glGenVertexArrays 1) 0))
                (glBindVertexArray cv)
                (define cb (u32vector-ref (glGenBuffers 1) 0))
                (glBindBuffer GL_ARRAY_BUFFER cb)
                (glBufferData GL_ARRAY_BUFFER (gl-vector-sizeof verts) verts GL_STATIC_DRAW)
                (define s6 (* 6 4))
                (glVertexAttribPointer 0 3 GL_FLOAT #f s6 0)
                (glEnableVertexAttribArray 0)
                (glVertexAttribPointer 1 3 GL_FLOAT #f s6 (* 3 4))
                (glEnableVertexAttribArray 1)
                (define ce (u32vector-ref (glGenBuffers 1) 0))
                (glBindBuffer GL_ELEMENT_ARRAY_BUFFER ce)
                (glBufferData GL_ELEMENT_ARRAY_BUFFER (gl-vector-sizeof idx) idx GL_STATIC_DRAW)
                (set! vao-cube cv)
                ;; 整屏四边形（贴回用）：pos(2)+uv(2)
                (define qv (u32vector-ref (glGenVertexArrays 1) 0))
                (define qverts (f32vector -1.0 -1.0  0.0 0.0
                                           1.0 -1.0  1.0 0.0
                                           1.0  1.0  1.0 1.0
                                          -1.0  1.0  0.0 1.0))
                (glBindVertexArray qv)
                (define qb (u32vector-ref (glGenBuffers 1) 0))
                (glBindBuffer GL_ARRAY_BUFFER qb)
                (glBufferData GL_ARRAY_BUFFER (gl-vector-sizeof qverts) qverts GL_STATIC_DRAW)
                (define s4 (* 4 4))
                (glVertexAttribPointer 0 2 GL_FLOAT #f s4 0)
                (glEnableVertexAttribArray 0)
                (glVertexAttribPointer 1 2 GL_FLOAT #f s4 (* 2 4))
                (glEnableVertexAttribArray 1)
                (define qidx (u16vector 0 1 2 0 2 3))
                (define qe (u32vector-ref (glGenBuffers 1) 0))
                (glBindBuffer GL_ELEMENT_ARRAY_BUFFER qe)
                (glBufferData GL_ELEMENT_ARRAY_BUFFER (gl-vector-sizeof qidx) qidx GL_STATIC_DRAW)
                (set! vao-quad qv)
                (when (zero? (unbox fbo))            ; 万一 on-size 还没跑过
                  (define-values (gw0 gh0) (send this get-gl-client-size))
                  (make-offscreen! gw0 gh0)))

              (define t (/ (- (current-inexact-milliseconds) start-ms) 1000.0))
              (define gw (unbox fw)) (define gh (unbox fh))
              (define aspect (/ (exact->inexact gw) (exact->inexact gh)))
              (define P (m4-perspective 45.0 aspect 0.1 100.0))
              (define V (m4-translate 0.0 0.0 -6.0))

              ;; ========== 第 1 遍：画进离屏纹理 ==========
              (glBindFramebuffer GL_FRAMEBUFFER (unbox fbo))
              (glViewport 0 0 gw gh)
              (glEnable GL_DEPTH_TEST)
              (glClearColor 0.30 0.25 0.35 1.0)      ; 中间色：滤镜效果更明显
              (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
              (glUseProgram prog-scene)
              (define (draw-cube m)
                (glUniformMatrix4fv loc-mvp 1 #f (mat4 (m4-mult (m4-mult P V) m)))
                (glBindVertexArray vao-cube)
                (glDrawElements GL_TRIANGLES 36 GL_UNSIGNED_SHORT 0))
              ;; 中央缩到 0.6、公转小立方体 0.4、半径 2.2 → 旋转时不互穿
              (draw-cube (m4-mult (m4-mult (m4-rot-y (* t 60.0)) (m4-rot-x (* t 40.0)))
                                  (m4-scale 0.6 0.6 0.6)))
              (for ([k (in-range 2)])
                (define a (+ (* k 180.0) (* t 80.0)))
                (define rad (* (/ PI 180.0) a))
                (draw-cube (m4-mult (m4-translate (* 2.2 (cos rad)) 0.0 (- (* 2.2 (sin rad))))
                                    (m4-mult (m4-rot-y (* t -90.0))
                                             (m4-scale 0.4 0.4 0.4)))))

              ;; ========== 第 2 遍：离屏纹理整屏贴回窗口 ==========
              (glBindFramebuffer GL_FRAMEBUFFER 0)
              (glViewport 0 0 gw gh)
              (glDisable GL_DEPTH_TEST)
              (glUseProgram prog-screen)
              (glActiveTexture GL_TEXTURE0)
              (glBindTexture GL_TEXTURE_2D (unbox tex-color))
              (glUniform1i loc-tex 0)
              (glUniform1i loc-mode (unbox mode))
              (glBindVertexArray vao-quad)
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
