#lang racket/base
;; =========================================================
;; 09-texture/04-cube.rkt —— 第四步：立方体贴图 + 收 load-tex 进 lib
;; 运行：racket 09-texture/04-cube.rkt    点 X = 退出
;; =========================================================
;; 前 3 步：纹理上传(01)、过滤环绕(02)、mipmap(03)，都在平面四边形上。
;; 本步把纹理贴到 3D 物体上，并顺带把 load-tex 收进 lib.rkt。
;;
;; 本步新增（1 个）：
;;   立方体 uv —— 每个面 4 个顶点各自带 uv，把整张图 (0..1) 铺到每个面上
;;
;; ★3D 贴图的思路：uv 是"顶点属性"，和位置一样每顶点一份。立方体 6 个面，
;;   每面 4 个顶点（因为每面的 uv 排列不同，角不能共享，和 07 课"每面颜色
;;   不同要拆面"同理）。每个面把 (0,0)-(1,1) 整张图铺满 → 六面都是同一张图。
;;
;; 本步视觉：旋转立方体，六个面都贴着 cube.png。你能看到纹理随面转动——
;;   这就是"贴图"在 3D 上的样子。
;; =========================================================

(require "lib-gui.rkt")
(require "lib.rkt")     ; load-tex、cube-uv-verts、cube-uv-idx 现在都在 lib 里
(require racket/runtime-path)

(define-runtime-path cube-png "assets/cube.png")
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

(define (draw)
  (define t (/ (- (current-inexact-milliseconds) start-ms) 1000.0))
  (define-values (w h) (send canvas get-gl-client-size))
  (define aspect (/ (exact->inexact w) (exact->inexact h)))
  (define P (m4-perspective 45.0 aspect 0.1 100.0))
  (define V (m4-look-at 0.0 1.0 5.0  0.0 0.0 0.0  0.0 1.0 0.0))
  (define M (m4-mult (m4-rot-y (* t 60.0)) (m4-rot-x (* t 30.0))))

  (glClearColor 0.10 0.11 0.17 1.0)
  (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
  (glUseProgram prog)
  (glUniform1i loc-tex 0)
  (glActiveTexture GL_TEXTURE0)
  (glBindTexture GL_TEXTURE_2D tex)
  (glUniformMatrix4fv loc-mvp 1 #f (mat4 (m4-mult (m4-mult P V) M)))
  (glBindVertexArray vao)
  (glDrawElements GL_TRIANGLES 36 GL_UNSIGNED_SHORT 0))

(define-values (frame canvas)
  (make-window #:title "09-04 立方体贴图" #:width 600 #:height 600 #:draw draw))

(define prog (send canvas with-gl-context (lambda () (build-program vert-src frag-src))))
(define loc-mvp (send canvas with-gl-context (lambda () (glGetUniformLocation prog "uMVP"))))
(define loc-tex (send canvas with-gl-context (lambda () (glGetUniformLocation prog "uTex"))))
(define tex (send canvas with-gl-context (lambda () (load-tex cube-png 'clamp #t))))
(define vao
  (send canvas with-gl-context
        (lambda ()
          (glEnable GL_DEPTH_TEST)
          (define vbo (u32vector-ref (glGenBuffers 1) 0))
          (glBindBuffer GL_ARRAY_BUFFER vbo)
          (glBufferData GL_ARRAY_BUFFER (gl-vector-sizeof cube-uv-verts) cube-uv-verts GL_STATIC_DRAW)
          (define v (u32vector-ref (glGenVertexArrays 1) 0))
          (glBindVertexArray v)
          (glVertexAttribPointer 0 (glsl-size 'vec3) GL_FLOAT #f (glsl-stride-bytes 'vec3 'vec2) 0)
          (glEnableVertexAttribArray 0)
          (glVertexAttribPointer 1 (glsl-size 'vec2) GL_FLOAT #f (glsl-stride-bytes 'vec3 'vec2) (glsl-stride-bytes 'vec3))
          (glEnableVertexAttribArray 1)
          (define ebo (u32vector-ref (glGenBuffers 1) 0))
          (glBindBuffer GL_ELEMENT_ARRAY_BUFFER ebo)
          (glBufferData GL_ELEMENT_ARRAY_BUFFER (gl-vector-sizeof cube-uv-idx) cube-uv-idx GL_STATIC_DRAW)
          (glBindVertexArray 0)
          v)))

(define ticker
  (new timer% (interval 16)
       (notify-callback (lambda () (send canvas refresh)))))

(send frame show #t)
