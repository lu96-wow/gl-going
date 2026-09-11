#lang racket/base
;; =========================================================
;; 09-texture/03-mipmap.rkt —— 第三步：mipmap 多级缩小图
;; 运行：racket 09-texture/03-mipmap.rkt     M = 切换 mipmap   点X = 退出
;; =========================================================
;; 上一步：会设过滤和环绕。但还有一个问题没解决：纹理**缩小**到远处时会闪。
;;
;; 本步新增（1 个）：
;;   mipmap —— 预先生成一组逐级缩小的图，缩小采样时按距离挑合适的那级
;;
;; ★问题：地板向远处延伸，远处的棋盘格被压到比一个像素还小。如果直接对
;;   256×256 的大图做"采样"，相邻像素可能跳到完全不同的格子上 → 远处闪烁、
;;   摩尔纹。mipmap = 提前生成 128×128、64×64… 一串缩小图，远处自动用
;;   小图采样，画面稳定。
;;   GLSL/GL 侧：glGenerateMipmap 一键生成全部级；MIN_FILTER 设成
;;   GL_LINEAR_MIPMAP_LINEAR（在两级之间还做插值，最平滑）。
;;
;; 本步视觉：一条伸向远方的棋盘地板。按 M 在"GL_LINEAR（无 mipmap，远处闪）"
;;   和"GL_LINEAR_MIPMAP_LINEAR（有 mipmap，远处稳）"之间切换，盯住远处看差别。
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
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_REPEAT)
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_REPEAT)
  (if mipmap?
      (begin (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR_MIPMAP_LINEAR)
             (glGenerateMipmap GL_TEXTURE_2D))          ; ★一键生成所有缩小级
      (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR))
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
  tex)

;; 伸向远方的地板（z 从 -7 到 -24），uv 放大到 4×8（棋盘平铺很多块）
(define verts
  (concat-vecs (vec3 -9.0 -1.6 -7.0)  (vec2 0.0 0.0)
               (vec3  9.0 -1.6 -7.0)  (vec2 4.0 0.0)
               (vec3  9.0 -1.6 -24.0) (vec2 4.0 8.0)
               (vec3 -9.0 -1.6 -24.0) (vec2 0.0 8.0)))
(define idx (u16vector 0 1 2  0 2 3))

(define mipmap? (box #t))

;; M 键切换：MIN_FILTER 在有 mipmap / 无 mipmap 之间切
(define (on-char e)
  (define code (send e get-key-code))
  (when (not (eq? code 'release))
    (cond
      [(or (eq? code #\m) (eq? code #\M))
       (set-box! mipmap? (not (unbox mipmap?)))
       (send canvas with-gl-context
             (lambda ()
               (glBindTexture GL_TEXTURE_2D tex)
               (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER
                                (if (unbox mipmap?) GL_LINEAR_MIPMAP_LINEAR GL_LINEAR))))
       (printf (if (unbox mipmap?) "mipmap 开（远处稳定）~%" "mipmap 关（远处闪烁）~%"))]
      [(eq? code 'escape) (exit 0)])))

(define (draw)
  (define-values (w h) (send canvas get-gl-client-size))
  (define aspect (/ (exact->inexact w) (exact->inexact h)))
  (define P (mat4-perspective 50.0 aspect 0.1 100.0))
  (define V (mat4-look-at 4.5 5.0 9.0  0.0 0.0 -10.0  0.0 1.0 0.0))

  (glClearColor 0.10 0.11 0.17 1.0)
  (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
  (glUseProgram prog)
  (glUniform1i loc-tex 0)
  (glActiveTexture GL_TEXTURE0)
  (glBindTexture GL_TEXTURE_2D tex)
  (glUniformMatrix4fv loc-mvp 1 #f (mat4-mult P V))
  (glBindVertexArray vao)
  (glDrawElements GL_TRIANGLES 6 GL_UNSIGNED_SHORT 0))

(define-values (frame canvas)
  (make-window #:title "09-03 mipmap（M 切换）"
               #:width 800 #:height 600 #:draw draw #:on-char on-char))

(define prog (send canvas with-gl-context (lambda () (build-program vert-src frag-src))))
(define loc-mvp (send canvas with-gl-context (lambda () (glGetUniformLocation prog "uMVP"))))
(define loc-tex (send canvas with-gl-context (lambda () (glGetUniformLocation prog "uTex"))))
(define tex (send canvas with-gl-context (lambda () (load-tex floor-png 'repeat #t))))
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
