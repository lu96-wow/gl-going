#lang racket/base
;; =========================================================
;; 10-texture/05-demo.rkt —— 第五步：综合，地板 + 旋转贴图立方体
;; 运行：racket 10-texture/05-demo.rkt    点 X = 退出
;; =========================================================
;; 本课前四步：上传(01)、过滤环绕(02)、mipmap(03)、立方体贴图(04)。
;; 本步**不引入新语法**，把老教程 08-texture 的成品拼出来。
;;
;; 场景：向远方铺开的棋盘地板（REPEAT 平铺 + mipmap）+ 悬在空中的旋转立方体
;;   （cube.png 贴六面）。一块纹理 = 棋盘地板；一张图 = 立方体六个面。
;;   你看到的是"贴图"在完整场景里的样子。
;; =========================================================

(require "lib-gui.rkt")
(require "lib.rkt")
(require racket/runtime-path)

(define-runtime-path cube-png "assets/cube.png")
(define-runtime-path floor-png "assets/floor.png")
(define start-ms (current-inexact-milliseconds))

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

;; 地板：向远处铺开（z -7 → -24），uv 放大 4×8 → 棋盘平铺很多块
(define floor-verts
  (concat-vecs (vec3 -9.0 -1.6 -7.0)  (vec2 0.0 0.0)
               (vec3  9.0 -1.6 -7.0)  (vec2 4.0 0.0)
               (vec3  9.0 -1.6 -24.0) (vec2 4.0 8.0)
               (vec3 -9.0 -1.6 -24.0) (vec2 0.0 8.0)))
(define floor-idx (u16vector 0 1 2  0 2 3))

(define (draw)
  (define t (/ (- (current-inexact-milliseconds) start-ms) 1000.0))
  (define-values (w h) (send canvas get-gl-client-size))
  (define aspect (/ (exact->inexact w) (exact->inexact h)))
  (define P (mat4-perspective 50.0 aspect 0.1 100.0))
  (define V (mat4-look-at 4.5 5.0 9.0  0.0 0.0 -10.0  0.0 1.0 0.0))

  (glClearColor 0.10 0.11 0.17 1.0)
  (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
  (glUseProgram prog)
  (glUniform1i loc-tex 0)
  (glActiveTexture GL_TEXTURE0)

  ;; 地板（棋盘图，REPEAT 平铺）
  (glBindTexture GL_TEXTURE_2D tex-floor)
  (glUniformMatrix4fv loc-mvp 1 #f (mat4-mult P V))
  (glBindVertexArray vao-floor)
  (glDrawElements GL_TRIANGLES 6 GL_UNSIGNED_SHORT 0)

  ;; 旋转立方体（cube.png 贴六面）
  (glBindTexture GL_TEXTURE_2D tex-cube)
  (define M (mat4-mult (mat4-translate 0.0 0.8 0.0)
                     (mat4-mult (mat4-rot-y (* t 60.0)) (mat4-rot-x (* t 30.0)))))
  (glUniformMatrix4fv loc-mvp 1 #f (mat4-mult (mat4-mult P V) M))
  (glBindVertexArray vao-cube)
  (glDrawElements GL_TRIANGLES 36 GL_UNSIGNED_SHORT 0))

(define-values (frame canvas)
  (make-window #:title "10-05 地板 + 贴图立方体" #:width 800 #:height 600 #:draw draw))

(define prog (send canvas with-gl-context (lambda () (build-program (GL_VERTEX_SHADER vert-src) (GL_FRAGMENT_SHADER frag-src)))))
(define loc-mvp (send canvas with-gl-context (lambda () (glGetUniformLocation prog "uMVP"))))
(define loc-tex (send canvas with-gl-context (lambda () (glGetUniformLocation prog "uTex"))))
(define tex-floor (send canvas with-gl-context (lambda () (load-tex floor-png 'repeat #t))))
(define tex-cube  (send canvas with-gl-context (lambda () (load-tex cube-png 'clamp #t))))

(define (make-vao verts indices)
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
          (glBufferData GL_ELEMENT_ARRAY_BUFFER (gl-vector-sizeof indices) indices GL_STATIC_DRAW)
          (glBindVertexArray 0)
          v)))
(define vao-floor (make-vao floor-verts floor-idx))
(define vao-cube  (make-vao cube-uv-verts cube-uv-idx))

(define ticker
  (new timer% (interval 16)
       (notify-callback (lambda () (send canvas refresh)))))

(send frame show #t)
(send canvas focus)
