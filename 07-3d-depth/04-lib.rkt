#lang racket/base
;; =========================================================
;; 07-3d-depth/04-lib.rkt —— 第四步：收进 lib.rkt
;; 运行：racket 07-3d-depth/04-lib.rkt    点 X = 退出
;; =========================================================
;; 03 步裸写的 mat4-perspective、02 步裸写的立方体数据，后面每课都要用——纯重复。
;; 本步把它们收进 lib.rkt（见同文件夹 lib.rkt）：mat4-perspective + cube-verts +
;; cube-idx。之后画立方体只需两行数据、一个矩阵函数。
;;
;; ★节奏：裸写(02/03) → 收进 lib(本步) → 复用(下一步)。
;;   矩阵工具至此齐了：translate/rot-x/rot-y/rot-z/scale/mult/ortho/perspective。
;;
;; 本步演示和 03 步完全一样（翻滚的透视立方体），代码从"自己写透视矩阵 +
;; 30 行立方体数据"缩成"两行 lib 数据 + 一个 mat4-perspective 调用"。
;; =========================================================

(require "lib-gui.rkt")
(require "lib.rkt")     ; mat4-perspective、cube-verts、cube-idx 现在都在这里

(define start-ms (current-inexact-milliseconds))

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

(define (draw)
  (define t (/ (- (current-inexact-milliseconds) start-ms) 1000.0))
  (define-values (w h) (send canvas get-gl-client-size))
  (define aspect (/ (exact->inexact w) (exact->inexact h)))
  (define V (mat4-translate 0.0 0.0 -6.0))
  (define P (mat4-perspective 45.0 aspect 0.1 100.0))
  (define M (mat4-mult (mat4-rot-y (* t 40.0)) (mat4-rot-x (* t 30.0))))
  (glClearColor 0.07 0.08 0.14 1.0)
  (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
  (glUseProgram prog)
  (glUniformMatrix4fv loc-mvp 1 #f (mat4-mult (mat4-mult P V) M))
  (glBindVertexArray vao)
  (glDrawElements GL_TRIANGLES 36 GL_UNSIGNED_SHORT 0))

(define-values (frame canvas)
  (make-window #:title "07-04 用 lib 画立方体" #:width 400 #:height 400 #:draw draw))

(define prog (send canvas with-gl-context (lambda () (build-program vert-src frag-src))))
(define loc-mvp (send canvas with-gl-context (lambda () (glGetUniformLocation prog "uMVP"))))
(define vao
  (send canvas with-gl-context
        (lambda ()
          (glEnable GL_DEPTH_TEST)
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

(define ticker
  (new timer% (interval 16)
       (notify-callback (lambda () (send canvas refresh)))))

(send frame show #t)
