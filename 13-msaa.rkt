#lang racket/base
;; =========================================================
;; 13-msaa.rkt —— 多重采样抗锯齿（MSAA）
;; 运行：racket 13-msaa.rkt   M = MSAA 开/关   ESC = 退出
;; =========================================================
;; 锯齿的成因：屏幕像素是"格子"，几何斜边却穿过格子中间。片元着色器
;; 每个格子只算一次 → 整格染成半边颜色，就出"狗牙"。
;;
;; MSAA 的思路：颜色只算一次，但**覆盖率用多个采样点**投票——斜边经过
;; 的格子，4 个采样点里有几个落在三角形内，颜色就按比例混合，边缘立刻
;; 平滑。开销只花在"边缘上的格子"，比超分辨率便宜得多。
;;
;; 新 API：
;;   glRenderbufferStorageMultisample(target, samples, fmt, w, h)
;;       —— 让渲染缓冲带 4×/8× 采样（普通窗口默认只有 1）
;;   glBlitFramebuffer(...) 完成"解析 resolve"
;;       —— 多采样缓冲不能直接贴图/上屏，要拷到单采样目标：
;;          glBindFramebuffer(GL_READ_FRAMEBUFFER, 多采样FBO)
;;          glBindFramebuffer(GL_DRAW_FRAMEBUFFER, 0)      ← 窗口
;;          glBlitFramebuffer(0,0,w,h, 0,0,w,h, GL_COLOR_BUFFER_BIT, GL_NEAREST)
;;          → 拷的过程自动把 4 个子样本平均成 1 个像素
;;
;; 演示：同样的场景两遍对比——关 MSAA 直接画到窗口（看斜边锯齿），
;; 开 MSAA 先进 4× 缓冲再解析上屏（边缘平滑）。M 切换。
;; 场景特意：立方体缩小（旋转占√3 体积，避免互穿）+ 每个立方体描黑边
;; ——黑边与面色的高对比，让"有无抗锯齿"一眼可辨。
;; =========================================================

(require racket/gui opengl)
(require "lib.rkt")

(define PI (acos -1.0))
(define start-ms (current-inexact-milliseconds))
(define msaa-on? (box #t))
(define SAMPLES 4)

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

(define cfg (new gl-config%))
(send cfg set-legacy? #f)
(send cfg set-double-buffered #t)
(send cfg set-depth-size 1)
(define frame
  (new (class frame%
         (augment* [on-close (lambda () (exit 0))])
         (super-new))
       (label "13 MSAA 抗锯齿") (width 800) (height 600)))

(define fw (box 800)) (define fh (box 600))
(define init? (box #f))
(define prog #f) (define vao-cube 0) (define vao-lines 0)
(define loc-mvp 0)
(define fbo-ms (box 0)) (define rbo-color (box 0)) (define rbo-depth-ms (box 0))

(define canvas
  (new (class canvas%
         (inherit with-gl-context swap-gl-buffers)
         (define (make-msaa! w h)
           (when (> (unbox fbo-ms) 0)
             (glDeleteFramebuffers 1 (u32vector (unbox fbo-ms)))
             (glDeleteRenderbuffers 1 (u32vector (unbox rbo-color)))
             (glDeleteRenderbuffers 1 (u32vector (unbox rbo-depth-ms))))
           ;; 颜色 + 深度都用"多重采样渲染缓冲"
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
           (set-box! fbo-ms fb) (set-box! rbo-color rc) (set-box! rbo-depth-ms rd))
         (define/override (on-size w h)
           (with-gl-context
            (lambda ()
              (define-values (gw gh) (send this get-gl-client-size))
              (set-box! fw gw) (set-box! fh gh)
              (glViewport 0 0 gw gh)
              (glEnable GL_DEPTH_TEST)
              (make-msaa! gw gh))))
         (define/override (on-char e)
           (define code (send e get-key-code))
           (when (not (eq? code 'release))
             (cond
               [(eq? code 'escape) (exit 0)]
               [(or (eq? code #\m) (eq? code #\M))
                (set-box! msaa-on? (not (unbox msaa-on?)))
                (printf (if (unbox msaa-on?) "MSAA 开（4× 平滑）~%" "MSAA 关（看锯齿）~%"))])))
         (define/override (on-paint)
           (with-gl-context
            (lambda ()
              (unless (unbox init?)
                (set-box! init? #t)
                (set! prog (build-program scene-vert scene-frag))
                (set! loc-mvp (glGetUniformLocation prog "uMVP"))
                ;; 立方体（05 同款）
                (define pos8
                  '((-1.0 -1.0  1.0) ( 1.0 -1.0  1.0) ( 1.0  1.0  1.0) (-1.0  1.0  1.0)
                    (-1.0 -1.0 -1.0) ( 1.0 -1.0 -1.0) ( 1.0  1.0 -1.0) (-1.0  1.0 -1.0)))
                (define faces
                  (list (list 0.90 0.25 0.25 '(0 1 2 3)) (list 0.25 0.85 0.30 '(5 4 7 6))
                        (list 0.95 0.60 0.10 '(1 5 6 2)) (list 0.95 0.85 0.15 '(4 0 3 7))
                        (list 0.25 0.60 0.95 '(3 2 6 7)) (list 0.75 0.35 0.90 '(4 5 1 0))))
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
                (define v (u32vector-ref (glGenVertexArrays 1) 0))
                (glBindVertexArray v)
                (define vb (u32vector-ref (glGenBuffers 1) 0))
                (glBindBuffer GL_ARRAY_BUFFER vb)
                (glBufferData GL_ARRAY_BUFFER (gl-vector-sizeof verts) verts GL_STATIC_DRAW)
                (define s6 (* 6 4))
                (glVertexAttribPointer 0 3 GL_FLOAT #f s6 0)
                (glEnableVertexAttribArray 0)
                (glVertexAttribPointer 1 3 GL_FLOAT #f s6 (* 3 4))
                (glEnableVertexAttribArray 1)
                (define eb (u32vector-ref (glGenBuffers 1) 0))
                (glBindBuffer GL_ELEMENT_ARRAY_BUFFER eb)
                (glBufferData GL_ELEMENT_ARRAY_BUFFER (gl-vector-sizeof idx) idx GL_STATIC_DRAW)
                (set! vao-cube v)

                ;; ② 黑边网格：立方体 12 条棱（pos + 黑色），放大锯齿观感
                (define edge-verts
                  (apply f32vector
                         (apply append
                                (for/list ([e (list (list 0 1) (list 1 2) (list 2 3) (list 3 0)
                                                    (list 4 5) (list 5 6) (list 6 7) (list 7 4)
                                                    (list 0 4) (list 1 5) (list 2 6) (list 3 7))])
                                  (define a (list-ref pos8 (car e)))
                                  (define b (list-ref pos8 (cadr e)))
                                  (append a '(0.0 0.0 0.0) b '(0.0 0.0 0.0))))))
                (define ve (u32vector-ref (glGenVertexArrays 1) 0))
                (glBindVertexArray ve)
                (define vb2 (u32vector-ref (glGenBuffers 1) 0))
                (glBindBuffer GL_ARRAY_BUFFER vb2)
                (glBufferData GL_ARRAY_BUFFER (gl-vector-sizeof edge-verts) edge-verts GL_STATIC_DRAW)
                (glVertexAttribPointer 0 3 GL_FLOAT #f s6 0)
                (glEnableVertexAttribArray 0)
                (glVertexAttribPointer 1 3 GL_FLOAT #f s6 (* 3 4))
                (glEnableVertexAttribArray 1)
                (set! vao-lines ve)
                (when (zero? (unbox fbo-ms))
                  (define-values (gw0 gh0) (send this get-gl-client-size))
                  (make-msaa! gw0 gh0)))

              (define t (/ (- (current-inexact-milliseconds) start-ms) 1000.0))
              (define gw (unbox fw)) (define gh (unbox fh))
              (define aspect (/ (exact->inexact gw) (exact->inexact gh)))
              (define P (m4-perspective 45.0 aspect 0.1 100.0))
              (define V (m4-mult (m4-translate 0.0 0.0 -6.0) (m4-rot-y 25.0)))

              ;; 画一个"缩小的立方体 + 黑边"：
              ;;   m = 位置/朝向；s = 棱长系数（旋转占√3 体积，s<1 才不会互穿）
              ;; 黑边用放大 4% 的同一矩阵画 GL_LINES，叠在实心面外侧 → 无 z-fight
              (define (draw-scene)
                (glClearColor 0.05 0.06 0.11 1.0)
                (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
                (glUseProgram prog)
                (define (cube m s)
                  (define Ms (m4-mult m (m4-scale s s s)))
                  (glBindVertexArray vao-cube)
                  (glUniformMatrix4fv loc-mvp 1 #f (mat4 (m4-mult (m4-mult P V) Ms)))
                  (glDrawElements GL_TRIANGLES 36 GL_UNSIGNED_SHORT 0)
                  (glBindVertexArray vao-lines)
                  (glUniformMatrix4fv loc-mvp 1 #f
                                      (mat4 (m4-mult (m4-mult P V)
                                                        (m4-mult Ms (m4-scale 1.04 1.04 1.04)))))
                  (glDrawArrays GL_LINES 0 24))
                (cube (m4-mult (m4-rot-y (* t 55.0)) (m4-rot-x (* t 35.0))) 0.6)
                (for ([k (in-range 3)])
                  (define a (+ (* k 120.0) (* t 100.0)))
                  (define rad (* (/ PI 180.0) a))
                  (cube (m4-mult (m4-translate (* 1.7 (cos rad)) 0.0 (- (* 1.7 (sin rad))))
                                 (m4-rot-y (* t -80.0)))
                        0.26)))

              (if (unbox msaa-on?)
                  ;; ---- MSAA：画进 4× 缓冲，再解析(blit)到窗口 ----
                  (begin
                    (glBindFramebuffer GL_FRAMEBUFFER (unbox fbo-ms))
                    (glViewport 0 0 gw gh)
                    (draw-scene)
                    (glBindFramebuffer GL_READ_FRAMEBUFFER (unbox fbo-ms))
                    (glBindFramebuffer GL_DRAW_FRAMEBUFFER 0)
                    (glBlitFramebuffer 0 0 gw gh  0 0 gw gh
                                       GL_COLOR_BUFFER_BIT GL_NEAREST)
                    (glBindFramebuffer GL_FRAMEBUFFER 0))
                  ;; ---- 关：直接画窗口（单采样）----
                  (begin
                    (glBindFramebuffer GL_FRAMEBUFFER 0)
                    (glViewport 0 0 gw gh)
                    (draw-scene)))

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
