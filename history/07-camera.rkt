#lang racket/base
;; =========================================================
;; 07-camera.rkt —— 视图矩阵：轨道相机（lookAt 三行）
;; 运行：racket 07-camera.rkt
;; 操作：左键拖 = 环绕(经度/纬度)   滚轮 = 拉近拉远   R = 复位   ESC = 退出
;; =========================================================
;; 上一课(06)：3D + 透视，相机是写死的"往后退 6 格"(V 固定)。
;; 本课解决"相机怎么动"。核心理解只有一句话：
;;
;;   ★相机 ≠ 世界动了，而是"把整个世界搬到相机面前"——
;;     视图矩阵 V 就是一个把世界坐标换成"以相机为原点的坐标"的矩阵。
;;
;; 新 API（其实是老 API 的用法）：m4-look-at(eye, center, up) 生成 V。
;; 实现三行：先算出相机位置 eye（本课从球坐标 经度/纬度/距离 反推），
;; 再 V = lookAt(eye, 目标原点, (0,1,0))，然后每帧：
;;     gl_Position = uMVP · v，uMVP = P · V · M
;;
;; 所以"相机在转"不过是把 V 每帧重新算一次再上传——
;; 场景代码（网格、立方体）完全不用知道相机在哪。这正是矩阵的好处：
;; CPU 只管改 V，GPU 无脑乘。
;;
;; 演示：网格地面 + 中央翻滚立方体 + 几颗散落小立方体。
;; 拖一下就知道：物体没动，是你的"眼睛"在绕。
;; =========================================================

(require racket/gui opengl)
(require "lib.rkt")

(define PI (acos -1.0))
(define start-ms (current-inexact-milliseconds))

(define vert-src
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

(define frag-src
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
       (label "07 轨道相机") (width 800) (height 600)))

;; ---- 轨道参数（球坐标，目标在原点）----
(define yaw (box 30.0)) (define pitch (box 20.0)) (define dist (box 9.0))
(define drag? (box #f)) (define last-px (box 0.0)) (define last-py (box 0.0))
(define fw (box 800)) (define fh (box 600))
(define init? (box #f))
(define prog #f) (define vao-cube 0) (define vao-lines 0) (define line-count 0)
(define loc-mvp 0)

(define canvas
  (new (class canvas%
         (inherit with-gl-context swap-gl-buffers)
         (define/override (on-size w h)
           (with-gl-context
            (lambda ()
              (define-values (gw gh) (send this get-gl-client-size))
              (set-box! fw gw) (set-box! fh gh)
              (glViewport 0 0 gw gh)
              (glEnable GL_DEPTH_TEST)
              (glClearColor 0.07 0.08 0.14 1.0))))
         (define/override (on-char e)
           (define code (send e get-key-code))
           (when (not (eq? code 'release))
             (cond
               [(or (eq? code #\r) (eq? code #\R))
                (set-box! yaw 30.0) (set-box! pitch 20.0) (set-box! dist 9.0)
                (printf "相机复位~%")]
               [(eq? code 'escape) (exit 0)])))
         (define/override (on-event e)
           (define et (send e get-event-type))
           (cond
             [(send e button-down? 'left)
              (set-box! drag? #t)
              (set-box! last-px (send e get-x))
              (set-box! last-py (send e get-y))]
             [(send e button-up? 'left) (set-box! drag? #f)]
             [(eq? et 'motion)
              (when (unbox drag?)
                (define px (send e get-x))
                (define py (send e get-y))
                (define sens 0.25)
                (set-box! yaw (+ (unbox yaw) (* (- px (unbox last-px)) sens)))
                (set-box! pitch (max -85.0 (min 85.0
                                                 (+ (unbox pitch)
                                                    (* (- (unbox last-py) py) sens)))))
                (set-box! last-px px) (set-box! last-py py))]
             [(eq? et 'wheel-up)   (set-box! dist (max 2.5 (min 20.0 (* (unbox dist) 0.88))))]
             [(eq? et 'wheel-down) (set-box! dist (max 2.5 (min 20.0 (* (unbox dist) 1.12))))]))
         (define/override (on-paint)
           (with-gl-context
            (lambda ()
              (unless (unbox init?)
                (set-box! init? #t)
                (set! prog (build-program vert-src frag-src))
                (set! loc-mvp (glGetUniformLocation prog "uMVP"))

                ;; ---- 网格地面：XZ 平面上一堆 GL_LINES（复用立方体着色器）----
                (define span 5.0) (define step 0.5)
                (define grid '())
                (for ([s (in-range (- span) (+ span step) step)])
                  (define dim '(0.22 0.24 0.38))
                  (set! grid (append grid
                                     (list (list (- span) 0.0 s) (list span 0.0 s)
                                           (list s 0.0 (- span)) (list s 0.0 span)))))
                (define lg (apply f32vector
                                  (apply append
                                         (for/list ([p grid])
                                           (append p '(0.30 0.32 0.50))))))
                (define vl (u32vector-ref (glGenVertexArrays 1) 0))
                (glBindVertexArray vl)
                (define bl (u32vector-ref (glGenBuffers 1) 0))
                (glBindBuffer GL_ARRAY_BUFFER bl)
                (glBufferData GL_ARRAY_BUFFER (gl-vector-sizeof lg) lg GL_STATIC_DRAW)
                (define s6 (* 6 4))
                (glVertexAttribPointer 0 3 GL_FLOAT #f s6 0)
                (glEnableVertexAttribArray 0)
                (glVertexAttribPointer 1 3 GL_FLOAT #f s6 (* 3 4))
                (glEnableVertexAttribArray 1)
                (set! vao-lines vl)
                (set! line-count (quotient (f32vector-length lg) 6))

                ;; ---- 立方体 VAO（05 同款生成法）----
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
                (define v (u32vector-ref (glGenVertexArrays 1) 0))
                (glBindVertexArray v)
                (define vb (u32vector-ref (glGenBuffers 1) 0))
                (glBindBuffer GL_ARRAY_BUFFER vb)
                (glBufferData GL_ARRAY_BUFFER (gl-vector-sizeof verts) verts GL_STATIC_DRAW)
                (glVertexAttribPointer 0 3 GL_FLOAT #f s6 0)
                (glEnableVertexAttribArray 0)
                (glVertexAttribPointer 1 3 GL_FLOAT #f s6 (* 3 4))
                (glEnableVertexAttribArray 1)
                (define eb (u32vector-ref (glGenBuffers 1) 0))
                (glBindBuffer GL_ELEMENT_ARRAY_BUFFER eb)
                (glBufferData GL_ELEMENT_ARRAY_BUFFER (gl-vector-sizeof idx) idx GL_STATIC_DRAW)
                (glBindVertexArray 0)
                (set! vao-cube v))

              (define t (/ (- (current-inexact-milliseconds) start-ms) 1000.0))
              (define gw (unbox fw)) (define gh (unbox fh))
              (define aspect (/ (exact->inexact gw) (exact->inexact gh)))
              (define P (m4-perspective 45.0 aspect 0.1 100.0))

              ;; ★视图矩阵三行：球坐标 → 眼睛位置 → lookAt
              (define ph (* (/ PI 180.0) (unbox pitch)))
              (define ya (* (/ PI 180.0) (unbox yaw)))
              (define ex (* (unbox dist) (cos ph) (sin ya)))
              (define ey (* (unbox dist) (sin ph)))
              (define ez (* (unbox dist) (cos ph) (cos ya)))
              (define V (m4-look-at ex ey ez  0.0 0.0 0.0  0.0 1.0 0.0))

              (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
              (glUseProgram prog)

              ;; 统一入口：给定模型矩阵 m，画立方体（36 索引）
              (define (draw-cube m)
                (glUniformMatrix4fv loc-mvp 1 #f (mat4 (m4-mult (m4-mult P V) m)))
                (glBindVertexArray vao-cube)
                (glDrawElements GL_TRIANGLES 36 GL_UNSIGNED_SHORT 0))

              ;; 地面网格（GL_LINES）
              (glUniformMatrix4fv loc-mvp 1 #f (mat4 (m4-mult P V)))
              (glBindVertexArray vao-lines)
              (glDrawArrays GL_LINES 0 line-count)

              ;; 中央翻滚立方体：缩到 0.8，避免旋转时(占√3体积)吞掉旁边小立方体
              (draw-cube (m4-mult (m4-translate 0.0 1.0 0.0)
                                  (m4-mult (m4-mult (m4-rot-y (* t 60.0)) (m4-rot-x (* t 40.0)))
                                           (m4-scale 0.8 0.8 0.8))))
              ;; 几颗散落的小立方体（挪远 + 缩小到 0.5）
              (for ([p (list (list 2.4 0.5 -1.7) (list -2.5 0.6 -1.3) (list 1.7 0.4 2.1))])
                (draw-cube (m4-mult (m4-translate (car p) (cadr p) (caddr p))
                                    (m4-mult (m4-rot-y (* t 90.0)) (m4-scale 0.5 0.5 0.5)))))

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
