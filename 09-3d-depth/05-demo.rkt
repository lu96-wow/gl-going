#lang racket/base
;; =========================================================
;; 09-3d-depth/05-demo.rkt —— 第五步：综合，三颗立方体
;; 运行：racket 09-3d-depth/05-demo.rkt    点 X = 退出
;; =========================================================

;; 本课前四步：3D 顶点与线框(01)、实心与深度(02)、透视(03)、收进 lib(04)。
;; 本步**不引入新语法**，把"3D + 深度 + 透视"拼成成品。
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

(require "gui-tool.rkt")              ; make-window（带深度缓冲）+ start-animation
(require "../racket-glsl/rewrite.rkt")
(require "../racket-glsl/tool.rkt")
(require "lib.rkt")                   ; mat4-* + cube-verts/cube-idx

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

  (gl-clear-color 0.10 0.12 0.20 1.0)
  (gl-clear (bitwise-ior gl-color-buffer-bit gl-depth-buffer-bit))
  (use-program prog)
  (gl-bind-vertex-array vao)

  ;; 画一颗立方体：给定模型矩阵 M
  (define (cube-at M)
    (gl-uniform-matrix-4fv loc-mvp 1 #f (mat4-mult (mat4-mult P V) M))
    (gl-draw-elements gl-triangles 36 gl-unsigned-short 0))

  ;; 三颗并排：x 拉开 2.6，z 依次 -1.4 / -0.2 / +1.0（前后拉开），各自翻滚
  (for ([k (in-range 3)])
    (define kk (exact->inexact k))   ; ★转 flonum：否则 (* 2.6 (- k 1)) 在 k=1 时得精确 0，mat4 会报错
    (define x (* 2.6 (- kk 1.0)))
    (define z (- 1.0 (* 1.2 kk)))
    (define M (mat4-mult (mat4-translate x 0.0 z)
                         (mat4-mult (mat4-mult (mat4-rot-y (* t (+ 40.0 (* kk 30.0))))
                                               (mat4-rot-x (* t 30.0)))
                                    (mat4-scale 0.6 0.6 0.6))))
    (cube-at M)))

(define-values (frame canvas)
  (make-window #:title "09-05 三颗立方体（综合）" #:width 800 #:height 600 #:draw draw))

(define prog
  (send canvas with-gl-context
    (lambda () (build-program (gl-vertex-shader vert-src) (gl-fragment-shader frag-src)))))
(define loc-mvp
  (send canvas with-gl-context (lambda () (uniform-location prog "uMVP"))))
(define vao
  (send canvas with-gl-context
    (lambda ()
      (gl-enable gl-depth-test)
      (define vbo (u32vector-ref (gl-gen-buffers 1) 0))
      (gl-bind-buffer gl-array-buffer vbo)
      (gl-buffer-data gl-array-buffer (gl-vector-sizeof cube-verts) cube-verts gl-static-draw)
      (define v (u32vector-ref (gl-gen-vertex-arrays 1) 0))
      (gl-bind-vertex-array v)
      (gl-vertex-attrib-pointer 0 (glsl-size 'vec3) gl-float #f (glsl-stride-bytes 'vec3 'vec3) 0)
      (gl-enable-vertex-attrib-array 0)
      (gl-vertex-attrib-pointer 1 (glsl-size 'vec3) gl-float #f
                                (glsl-stride-bytes 'vec3 'vec3)
                                (glsl-stride-bytes 'vec3))
      (gl-enable-vertex-attrib-array 1)
      (define ebo (u32vector-ref (gl-gen-buffers 1) 0))
      (gl-bind-buffer gl-element-array-buffer ebo)
      (gl-buffer-data gl-element-array-buffer (gl-vector-sizeof cube-idx) cube-idx gl-static-draw)
      (gl-bind-vertex-array 0)
      v)))

(define ticker (start-animation canvas 16))

(send frame show #t)
