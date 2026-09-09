#lang racket/base
;; =========================================================
;; 08-texture.rkt —— 纹理：uv 坐标 + sampler2D + 采样参数
;; 运行：racket 08-texture.rkt    T = 纹理开/关   F = 过滤切换   ESC = 退出
;; 素材：assets/cube.png、assets/floor.png（gen-textures.rkt 可重新生成）
;; =========================================================
;; 前面几课每个顶点都亲手填颜色。本课换成"贴图"——照片级细节不可能
;; 逐顶点手填，而是把整张图片作为 **纹理** 贴到表面上。新概念三个：
;;
;;   ① uv 坐标（每顶点 2 个 float 的 attribute）
;;      —— 顶点除了位置，还带"在这张图上处于哪个点 (u,v)"。
;;         u/v 都是 0..1（图左/下→右/上）。面上两个三角形内部的 uv
;;         由顶点 uv 自动插值，片元着色器据此去图上取色。
;;   ② sampler2D + texture()
;;      —— 片元着色器里：uniform sampler2D uTex; texture(uTex, uv)。
;;         sampler = 一个"纹理单元"的编号，纹理对象绑到单元上。
;;   ③ 采样参数（为什么贴图会花/会糊/会边缘拉伸）
;;      —— GL_TEXTURE_MIN/MAG_FILTER：放大/缩小时怎么取色。
;;         NEAREST=取最近像素（马赛克）；LINEAR=周围平均（平滑）。
;;      —— GL_TEXTURE_WRAP_S/T：uv 超出 0..1 怎么办。
;;         REPEAT=平铺（地板用）；CLAMP_TO_EDGE=边缘拉伸。
;;      —— 缩小用 mipmap（多级预缩小的图）避免远处闪烁。
;;
;; 新 API（Racket 侧）：
;;   opengl/util 的 load-texture：读图 → 位图转 RGBA 字节 → glTexImage2D
;;   （它内部把 Racket 位图转成 GL 要的字节序，本课直接用，不必手写转换）
;;   glActiveTexture + glBindTexture + glUniform1i = 纹理接上采样器
;;
;; 演示：地板铺棋盘（平铺）+ 立方体贴圆环图。T 关纹理看原色，F 看过滤差异。
;; =========================================================

(require racket/gui opengl)
(require racket/runtime-path)
(require "lib.rkt")

;; 纹理文件：相对"本脚本所在目录"解析（而非运行时的当前目录），
;; 这样在任意目录下 racket 08-texture.rkt 都能找到 assets/。
(define-runtime-path cube-png-path  "assets/cube.png")
(define-runtime-path floor-png-path "assets/floor.png")

(define start-ms (current-inexact-milliseconds))

(define vert-src
  (glsl-pretty
   (glsl
    (version 330 core)
    (layout (location 0) in vec3 aPos)
    (layout (location 1) in vec2 aUV)
    (uniform mat4 uMVP)
    (out vec2 vUV)
    (define (main) void
      (set! vUV aUV)
      (set! gl_Position (* uMVP (vec4 aPos 1.0)))))))

;; 片元着色器：mix(uTint, base, uUseTex) —— uUseTex=0 用纯色 uTint，
;; uUseTex=1 用贴图采样色；本课用 T 键在 0/1 间切，对比"有没有纹理"。
(define frag-src
  (glsl-pretty
   (glsl
    (version 330 core)
    (in vec2 vUV)
    (uniform sampler2D uTex)
    (uniform vec3 uTint)
    (uniform float uUseTex)
    (out vec4 FragColor)
    (define (main) void
      (vec3 base (rgb (texture uTex vUV)))
      (set! FragColor (vec4 (mix uTint base uUseTex) 1.0))))))

;; 用法小抄（GLSL 内建，出现即记住）：
;;   texture(uTex, uv) = 在 uv 处采样贴图 → vec4（.rgb 取前三个分量）
;;   mix(a, b, t)      = a*(1-t)+b*t；t∈[0,1] 线性混色（本课的 uUseTex 0/1 就是它）
;;   GL_TEXTURE0/glUniform1i：采样器的值 = "纹理单元号"。纹理先绑到某个
;;   单元，再把单元号告诉采样器——这样一次 draw 可以采样多张贴图。

(define cfg (new gl-config%))
(send cfg set-legacy? #f)
(send cfg set-double-buffered #t)
(send cfg set-depth-size 1)
(define frame
  (new (class frame%
         (augment* [on-close (lambda () (exit 0))])
         (super-new))
       (label "08 纹理贴图") (width 800) (height 600)))

(define tex-on? (box #t))
(define linear? (box #t))
(define fw (box 800)) (define fh (box 600))
(define init? (box #f))
(define prog #f)
(define loc-mvp 0) (define loc-tex 0) (define loc-tint 0) (define loc-use 0)
(define tex-cube 0) (define tex-floor 0)
(define vao-cube 0) (define vao-floor 0)

;; 自己写一个极小的纹理加载器（opengl/util 在 core 下会用被删除的旧常量
;; GL_CLAMP；换成新写法，也顺便看清上传的每一步）：
;; 读位图 → ARGB 字节 → 重排成 RGBA → glTexImage2D → 设采样参数
(define (load-tex path wrap-mode mipmap?)
  (define bm (read-bitmap path))
  (define w (send bm get-width)) (define h (send bm get-height))
  (define argb (make-bytes (* w h 4)))
  (send bm get-argb-pixels 0 0 w h argb)
  (define rgba (make-bytes (* w h 4)))          ; A,R,G,B → R,G,B,A
  (for ([i (in-range (* w h))])
    (bytes-set! rgba (* i 4)      (bytes-ref argb (+ (* i 4) 1)))
    (bytes-set! rgba (+ (* i 4) 1) (bytes-ref argb (+ (* i 4) 2)))
    (bytes-set! rgba (+ (* i 4) 2) (bytes-ref argb (+ (* i 4) 3)))
    (bytes-set! rgba (+ (* i 4) 3) (bytes-ref argb (* i 4))))
  (define tex (u32vector-ref (glGenTextures 1) 0))
  (glBindTexture GL_TEXTURE_2D tex)
  (glPixelStorei GL_UNPACK_ALIGNMENT 1)
  (glTexImage2D GL_TEXTURE_2D 0 GL_RGBA w h 0 GL_RGBA GL_UNSIGNED_BYTE rgba)
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S
                   (if (eq? wrap-mode 'repeat) GL_REPEAT GL_CLAMP_TO_EDGE))
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T
                   (if (eq? wrap-mode 'repeat) GL_REPEAT GL_CLAMP_TO_EDGE))
  (if mipmap?
      (begin (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR_MIPMAP_LINEAR)
             (glGenerateMipmap GL_TEXTURE_2D))  ; 自动生成多级缩小图
      (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR))
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
  tex)

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
              (glClearColor 0.10 0.11 0.17 1.0))))
         (define/override (on-char e)
           (define code (send e get-key-code))
           (when (not (eq? code 'release))
             (cond
               [(eq? code 'escape) (exit 0)]
               [(or (eq? code #\t) (eq? code #\T))
                (set-box! tex-on? (not (unbox tex-on?)))
                (printf (if (unbox tex-on?) "纹理 开~%" "纹理 关（纯色）~%"))]
               [(or (eq? code #\f) (eq? code #\F))
                (with-gl-context
                 (lambda ()
                   (define (set-one! id)
                     (glBindTexture GL_TEXTURE_2D id)
                     (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER
                                      (if (unbox linear?) GL_NEAREST GL_LINEAR_MIPMAP_LINEAR))
                     (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER
                                      (if (unbox linear?) GL_NEAREST GL_LINEAR)))
                   (set-one! tex-cube) (set-one! tex-floor)
                   (set-box! linear? (not (unbox linear?)))
                   (printf (if (unbox linear?) "过滤 LINEAR（平滑）~%" "过滤 NEAREST（马赛克）~%"))))])))
         (define/override (on-paint)
           (with-gl-context
            (lambda ()
              (unless (unbox init?)
                (set-box! init? #t)
                (set! tex-cube  (load-tex cube-png-path 'clamp #t))
                (set! tex-floor (load-tex floor-png-path 'repeat #t))
                (set! prog (build-program vert-src frag-src))
                (set! loc-mvp  (glGetUniformLocation prog "uMVP"))
                (set! loc-tex  (glGetUniformLocation prog "uTex"))
                (set! loc-tint (glGetUniformLocation prog "uTint"))
                (set! loc-use  (glGetUniformLocation prog "uUseTex"))

                ;; VAO 工厂：[pos(3)+uv(2)]×N
                (define (make-vao verts indices)
                  (define vao (u32vector-ref (glGenVertexArrays 1) 0))
                  (glBindVertexArray vao)
                  (define vbo (u32vector-ref (glGenBuffers 1) 0))
                  (glBindBuffer GL_ARRAY_BUFFER vbo)
                  (glBufferData GL_ARRAY_BUFFER (gl-vector-sizeof verts) verts GL_STATIC_DRAW)
                  (define s5 (* 5 4))
                  (glVertexAttribPointer 0 3 GL_FLOAT #f s5 0)
                  (glEnableVertexAttribArray 0)
                  (glVertexAttribPointer 1 2 GL_FLOAT #f s5 (* 3 4))
                  (glEnableVertexAttribArray 1)
                  (define ebo (u32vector-ref (glGenBuffers 1) 0))
                  (glBindBuffer GL_ELEMENT_ARRAY_BUFFER ebo)
                  (glBufferData GL_ELEMENT_ARRAY_BUFFER (gl-vector-sizeof indices) indices GL_STATIC_DRAW)
                  (glBindVertexArray 0)
                  vao)

                ;; 立方体：6 面 × 4 顶点 [x,y,z,u,v]
                (define pos8
                  '((-1.0 -1.0  1.0) ( 1.0 -1.0  1.0) ( 1.0  1.0  1.0) (-1.0  1.0  1.0)
                    (-1.0 -1.0 -1.0) ( 1.0 -1.0 -1.0) ( 1.0  1.0 -1.0) (-1.0  1.0 -1.0)))
                (define cube-faces '((0 1 2 3) (5 4 7 6) (1 5 6 2) (4 0 3 7) (3 2 6 7) (4 5 1 0)))
                (define uv4 '((0.0 0.0) (1.0 0.0) (1.0 1.0) (0.0 1.0)))
                (define cube-verts
                  (apply f32vector
                         (apply append
                                (for/list ([f cube-faces])
                                  (apply append
                                         (for/list ([j (in-range 4)])
                                           (define p (list-ref pos8 (list-ref f j)))
                                           (define u (list-ref uv4 j))
                                           (list (car p) (cadr p) (caddr p) (car u) (cadr u))))))))
                (define cube-idx
                  (apply u16vector
                         (apply append
                                (for/list ([i (in-range 6)])
                                  (define b (* i 4))
                                  (list b (+ b 1) (+ b 2) b (+ b 2) (+ b 3))))))
                (set! vao-cube (make-vao cube-verts cube-idx))
                ;; 地板：向远处铺开（uv 放大 → 棋盘平铺多块）
                (set! vao-floor
                      (make-vao (f32vector -9.0 -1.6 -7.0   0.0 0.0
                                           9.0 -1.6 -7.0   4.0 0.0
                                           9.0 -1.6 -24.0  4.0 8.0
                                          -9.0 -1.6 -24.0  0.0 8.0)
                                (u16vector 0 1 2 0 2 3))))

              (define t (/ (- (current-inexact-milliseconds) start-ms) 1000.0))
              (define gw (unbox fw)) (define gh (unbox fh))
              (define aspect (/ (exact->inexact gw) (exact->inexact gh)))
              (define P (m4-perspective 50.0 aspect 0.1 100.0))
              (define V (m4-look-at 4.5 5.0 9.0  0.0 0.0 -10.0  0.0 1.0 0.0))

              (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
              (glUseProgram prog)
              (glUniform1i loc-tex 0)                 ; 采样器接单元 0
              (glUniform1f loc-use (if (unbox tex-on?) 1.0 0.0))
              (glActiveTexture GL_TEXTURE0)

              (define (draw m vao)
                (glUniformMatrix4fv loc-mvp 1 #f (mat4 (m4-mult (m4-mult P V) m)))
                (glBindVertexArray vao)
                (glDrawElements GL_TRIANGLES 6 GL_UNSIGNED_SHORT 0))

              ;; 地板（棋盘图，REPEAT 平铺）
              (glBindTexture GL_TEXTURE_2D tex-floor)
              (glUniform3f loc-tint 0.4 0.4 0.4)
              (draw (m4-identity) vao-floor)

              ;; 旋转立方体（贴圆环图）
              (glBindTexture GL_TEXTURE_2D tex-cube)
              (glUniform3f loc-tint 0.6 0.7 1.0)
              (glUniformMatrix4fv loc-mvp 1 #f
                                  (mat4 (m4-mult (m4-mult P V)
                                                    (m4-mult (m4-translate 0.0 0.8 0.0)
                                                             (m4-mult (m4-rot-y (* t 60.0))
                                                                      (m4-rot-x (* t 30.0)))))))
              (glBindVertexArray vao-cube)
              (glDrawElements GL_TRIANGLES 36 GL_UNSIGNED_SHORT 0)

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
