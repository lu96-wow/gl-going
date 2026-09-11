#lang racket/base
;; =========================================================
;; 07-3d-depth/05-demo.rkt —— 第五步：综合，三颗立方体
;; 运行：racket 07-3d-depth/05-demo.rkt    点 X = 退出
;; =========================================================
;; 本课前四步：3D 顶点与线框(01)、实心与深度(02)、透视(03)、收进 lib(04)。
;; 本步**不引入新语法**，把老教程 06-3d-depth 的成品拼出来。
;;
;; 画面：三颗立方体并排（左中右），前后拉开 z 距离，各自以不同速度翻滚。
;;   你能同时看到三件事：
;;     ① 透视：近处（更靠前那颗）更大，远处更小 → 近大远小
;;     ② 深度：立方体转过某个角度时前后两个面遮挡正确
;;     ③ 模型矩阵：每颗都有自己的 M = T(位置) · R(各自旋转) · S(缩放 0.6)
;;
;; 每颗立方体 = 同一份 cube-verts/cube-idx，只换 uMVP 矩阵——这就是"数据
;; 复用 + 变换区分"：一份几何，画三遍，每遍一个矩阵。
;; =========================================================

(require "lib-gui.rkt")
(require "lib.rkt")

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
  (define V (mat4-translate 0.0 0.0 -6.0))              ; 假相机
  (define P (mat4-perspective 45.0 aspect 0.1 100.0))   ; 透视

  (glClearColor 0.07 0.08 0.14 1.0)
  (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
  (glUseProgram prog)
  (glBindVertexArray vao)

  ;; 画一颗立方体：给定模型矩阵 M
  (define (cube-at M)
    (glUniformMatrix4fv loc-mvp 1 #f (mat4-mult (mat4-mult P V) M))
    (glDrawElements GL_TRIANGLES 36 GL_UNSIGNED_SHORT 0))

  ;; 三颗并排：x 拉开 2.6，z 依次 -1.4 / -0.2 / +1.0（前后拉开），各自翻滚
  (for ([k (in-range 3)])
    (define x (* 2.6 (- k 1)))
    (define z (- 1.0 (* 1.2 k)))
    (define M (mat4-mult (mat4-translate x 0.0 z)
                       (mat4-mult (mat4-mult (mat4-rot-y (* t (+ 40.0 (* k 30.0))))
                                         (mat4-rot-x (* t 30.0)))
                                (mat4-scale 0.6 0.6 0.6))))
    (cube-at M)))

(define-values (frame canvas)
  (make-window #:title "07-05 三颗立方体（综合）" #:width 800 #:height 600 #:draw draw))

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
