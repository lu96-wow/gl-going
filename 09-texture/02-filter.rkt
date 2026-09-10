#lang racket/base
;; =========================================================
;; 09-texture/02-filter.rkt —— 第二步：采样参数（过滤 + 环绕）
;; 运行：racket 09-texture/02-filter.rkt
;; 操作：F = 切换过滤(NEAREST/LINEAR)   W = 切换环绕(REPEAT/CLAMP)   点X = 退出
;; =========================================================
;; 上一步：图贴上去了。本步回答"为什么贴图会糊/会花/边缘会拉伸"——采样参数。
;;
;; 本步新增（2 组，同属"采样参数"这一件事）：
;;   ① 过滤 GL_TEXTURE_MIN/MAG_FILTER —— 放大/缩小时怎么取色
;;        NEAREST = 取最近像素（马赛克）；LINEAR = 周围平均（平滑）
;;   ② 环绕 GL_TEXTURE_WRAP_S/T —— uv 超出 0..1 怎么办
;;        REPEAT = 平铺重复；CLAMP_TO_EDGE = 边缘颜色拉伸
;;
;; ★为什么需要这些参数：纹理是一张有限像素的图，但采样点（uv）是连续的，
;;   而且可能落在像素之间、甚至 0..1 之外。GPU 必须有个规则"取哪个颜色"：
;;   过滤管"像素之间怎么插值"，环绕管"越界了怎么办"。本步用 F/W 两个键
;;   当场切换，看同一张贴图的不同表现。
;;
;; 本步视觉：floor.png（棋盘格）贴在一个大四边形上，uv 放大到 0..4：
;;   REPEAT → 4×4 棋盘平铺；CLAMP → 边缘拉伸成一整片
;;   LINEAR → 放大后平滑过渡；NEAREST → 放大后一格一格的马赛克
;; =========================================================

(require "lib-gui.rkt")
(require "lib.rkt")
(require racket/runtime-path)

(define-runtime-path floor-png "assets/floor.png")

(define vert-src
  (glsl (version 330 core)
        (layout (location 0) in vec3 aPos)
        (layout (location 1) in vec2 aUV)
        (uniform mat4 uMVP)
        (out vec2 vUV)
        (define (main) void
          (set! vUV aUV)
          (set! gl_Position (* uMVP (vec4 aPos 1.0))))))

(define frag-src
  (glsl (version 330 core)
        (in vec2 vUV)
        (uniform sampler2D uTex)
        (out vec4 FragColor)
        (define (main) void
          (set! FragColor (texture uTex vUV)))))

(define (load-tex path wrap-mode mipmap?)
  (define bm (read-bitmap path))
  (define w (send bm get-width)) (define h (send bm get-height))
  (define argb (make-bytes (* w h 4)))
  (send bm get-argb-pixels 0 0 w h argb)
  (define rgba (make-bytes (* w h 4)))
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
             (glGenerateMipmap GL_TEXTURE_2D))
      (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR))
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
  tex)

;; 大四边形，uv 放大到 0..4（这样 REPEAT 会平铺 4×4 块）
(define verts
  (concat-vecs (vec3 -2.0 -2.0 0.0) (vec2 0.0 0.0)
               (vec3  2.0 -2.0 0.0) (vec2 4.0 0.0)
               (vec3  2.0  2.0 0.0) (vec2 4.0 4.0)
               (vec3 -2.0  2.0 0.0) (vec2 0.0 4.0)))
(define idx (u16vector 0 1 2  0 2 3))

;; 采样参数状态
(define linear? (box #t))   ; #t = LINEAR，#f = NEAREST
(define repeat? (box #t))   ; #t = REPEAT，#f = CLAMP_TO_EDGE

;; 把当前参数应用到纹理上（绑定纹理 → 设 4 个参数）
(define (apply-params!)
  (send canvas with-gl-context
        (lambda ()
          (glBindTexture GL_TEXTURE_2D tex)
          (define wrap (if (unbox repeat?) GL_REPEAT GL_CLAMP_TO_EDGE))
          (define filt (if (unbox linear?) GL_LINEAR GL_NEAREST))
          (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S wrap)
          (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T wrap)
          (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER filt)
          (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER filt))))

(define (on-char e)
  (define code (send e get-key-code))
  (when (not (eq? code 'release))
    (cond
      [(or (eq? code #\f) (eq? code #\F))
       (set-box! linear? (not (unbox linear?)))
       (apply-params!)
       (printf (if (unbox linear?) "过滤 LINEAR（平滑）~%" "过滤 NEAREST（马赛克）~%"))]
      [(or (eq? code #\w) (eq? code #\W))
       (set-box! repeat? (not (unbox repeat?)))
       (apply-params!)
       (printf (if (unbox repeat?) "环绕 REPEAT（平铺）~%" "环绕 CLAMP_TO_EDGE（拉伸）~%"))]
      [(eq? code 'escape) (exit 0)])))

(define (draw)
  (define-values (w h) (send canvas get-gl-client-size))
  (define aspect (/ (exact->inexact w) (exact->inexact h)))
  (define P (m4-perspective 45.0 aspect 0.1 100.0))
  (define V (m4-look-at 0.0 0.0 5.0  0.0 0.0 0.0  0.0 1.0 0.0))

  (glClearColor 0.10 0.11 0.17 1.0)
  (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
  (glUseProgram prog)
  (glUniform1i loc-tex 0)
  (glActiveTexture GL_TEXTURE0)
  (glBindTexture GL_TEXTURE_2D tex)
  (glUniformMatrix4fv loc-mvp 1 #f (mat4 (m4-mult P V)))
  (glBindVertexArray vao)
  (glDrawElements GL_TRIANGLES 6 GL_UNSIGNED_SHORT 0))

(define-values (frame canvas)
  (make-window #:title "09-02 过滤与环绕（F/W 切换）"
               #:width 600 #:height 600 #:draw draw #:on-char on-char))

(define prog (send canvas with-gl-context (lambda () (build-program vert-src frag-src))))
(define loc-mvp (send canvas with-gl-context (lambda () (glGetUniformLocation prog "uMVP"))))
(define loc-tex (send canvas with-gl-context (lambda () (glGetUniformLocation prog "uTex"))))
(define tex (send canvas with-gl-context (lambda () (load-tex floor-png 'repeat #f))))
(define vao
  (send canvas with-gl-context
        (lambda ()
          (glEnable GL_DEPTH_TEST)
          (define vbo (u32vector-ref (glGenBuffers 1) 0))
          (glBindBuffer GL_ARRAY_BUFFER vbo)
          (glBufferData GL_ARRAY_BUFFER (gl-vector-sizeof verts) verts GL_STATIC_DRAW)
          (define v (u32vector-ref (glGenVertexArrays 1) 0))
          (glBindVertexArray v)
          (glVertexAttribPointer 0 (glsl-size 'vec3) GL_FLOAT #f (glsl-stride-bytes 'vec3 'vec2) 0)
          (glEnableVertexAttribArray 0)
          (glVertexAttribPointer 1 (glsl-size 'vec2) GL_FLOAT #f (glsl-stride-bytes 'vec3 'vec2) (glsl-stride-bytes 'vec3))
          (glEnableVertexAttribArray 1)
          (define ebo (u32vector-ref (glGenBuffers 1) 0))
          (glBindBuffer GL_ELEMENT_ARRAY_BUFFER ebo)
          (glBufferData GL_ELEMENT_ARRAY_BUFFER (gl-vector-sizeof idx) idx GL_STATIC_DRAW)
          (glBindVertexArray 0)
          v)))

(send frame show #t)
(send canvas focus)
