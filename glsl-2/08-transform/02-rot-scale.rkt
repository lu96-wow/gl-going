#lang racket/base
;; =========================================================
;; 08-transform/02-rot-scale.rkt —— 第二步：旋转 + 缩放矩阵
;; 运行：racket 08-transform/02-rot-scale.rkt    点 X = 退出
;; =========================================================

;; 上一步：会用矩阵平移。本步补上另外两个基本变换：旋转、缩放。
;;
;; 本步新增（2 个）：
;;   旋转矩阵 —— 对角放 cos、反对角放 ±sin
;;   缩放矩阵 —— 对角线上放缩放倍数（单位阵的推广）
;;
;; ★旋转（绕 z 轴，2D 就是它）：转 θ 度，
;;     [cosθ  -sinθ  0 0]
;;     [sinθ   cosθ  0 0]
;;     [0      0     1 0]
;;     [0      0     0 1]
;;   为什么是 sin/cos：旋转 = 把 x 轴"掰"到 (cosθ, sinθ) 方向。点 (1,0)
;;   转 θ 后落在 (cosθ, sinθ)，点 (0,1) 落在 (-sinθ, cosθ)——这两列正好是
;;   旋转矩阵的前两列。列主序存储时第 0 列 = (c, s)，第 1 列 = (-s, c)。
;;
;; ★缩放：单位阵的对角线不再是 1，而是各轴的倍数：
;;     [sx 0  0 0]
;;     [0  sy 0 0]
;;     [0  0  1 0]     （z 保持 1；2D 用不到）
;;     [0  0  0 1]
;;   乘上 (x,y,0,1) 得到 (sx·x, sy·y, 0, 1) = 沿各轴拉伸。
;;
;; 本步视觉：方块绕自己中心匀速旋转。★动手试：把下面 draw 里的
;;   (mat4-rot-z ...) 换成 (mat4-scale 1.3 1.3)，方块就会原地"呼吸"缩放。
;;   两个变换同时做（旋转+缩放）需要矩阵乘法——下一步。
;; =========================================================

(require "../04-animate/05-gui-tool.rkt")   ; make-window + start-animation
(require "../racket-glsl/rewrite.rkt")        ; (glsl ...) 宏
(require "../racket-glsl/rename-vector.rkt")  ; mat4 / vec2 / u16vector / glsl-size / glsl-stride-bytes
(require "../racket-glsl/tool.rkt")           ; build-program / use-program / uniform-location

(define start-ms (current-inexact-milliseconds))

(define vert-src
  (glsl (version 330 core)
        (layout (location 0) in vec2 aPos)
        (uniform mat4 uMVP)
        (define (main) void
          (set! gl_Position (* uMVP (vec4 aPos 0.0 1.0))))))

(define frag-src
  (glsl (version 330 core)
        (uniform vec3 uColor)
        (out vec4 FragColor)
        (define (main) void
          (set! FragColor (vec4 uColor 1.0)))))

;; 旋转矩阵（绕 z，角度制）—— 裸写，本步主角
(define (mat4-rot-z deg)
  (define r (* (/ pi 180.0) deg))
  (define c (cos r))
  (define s (sin r))
  (mat4 c     s     0.0 0.0    ; 第 0 列 = (cosθ, sinθ)
             (- s) c     0.0 0.0    ; 第 1 列 = (-sinθ, cosθ)
             0.0   0.0   1.0 0.0
             0.0   0.0   0.0 1.0))

;; 缩放矩阵 —— 对角线放倍数（本步先定义，动手试时换上去）
(define (mat4-scale sx sy)
  (mat4 sx  0.0 0.0 0.0
             0.0 sy  0.0 0.0
             0.0 0.0 1.0 0.0
             0.0 0.0 0.0 1.0))

(define verts (vec (vec2 -0.5 -0.5) (vec2 0.5 -0.5) (vec2 0.5 0.5) (vec2 -0.5 0.5)))
(define idx   (u16vector 0 1 2  0 2 3))

(define (draw)
  (define t (/ (- (current-inexact-milliseconds) start-ms) 1000.0))
  (define M (mat4-rot-z (* t 90.0)))    ; 每秒转 90°
  (gl-clear-color 0.10 0.12 0.20 1.0)
  (gl-clear gl-color-buffer-bit)
  (use-program prog)
  (gl-uniform-matrix-4fv loc-mvp 1 #f M)
  (gl-uniform-3f loc-color 0.30 0.65 0.95)
  (gl-bind-vertex-array vao)
  (gl-draw-elements gl-triangles 6 gl-unsigned-short 0))

(define-values (frame canvas)
  (make-window #:title "08-02 旋转与缩放" #:width 400 #:height 400 #:draw draw))

(define prog
  (send canvas with-gl-context
    (lambda () (build-program (gl-vertex-shader vert-src) (gl-fragment-shader frag-src)))))
(define loc-mvp
  (send canvas with-gl-context (lambda () (uniform-location prog "uMVP"))))
(define loc-color
  (send canvas with-gl-context (lambda () (uniform-location prog "uColor"))))
(define vao
  (send canvas with-gl-context
    (lambda ()
      (define data (vec->f32vector verts))
      (define vbo (u32vector-ref (gl-gen-buffers 1) 0))
      (gl-bind-buffer gl-array-buffer vbo)
      (gl-buffer-data gl-array-buffer (gl-vector-sizeof data) data gl-static-draw)
      (define v (u32vector-ref (gl-gen-vertex-arrays 1) 0))
      (gl-bind-vertex-array v)
      (gl-vertex-attrib-pointer 0 (glsl-size 'vec2) gl-float #f (glsl-stride-bytes 'vec2) 0)
      (gl-enable-vertex-attrib-array 0)
      (define ebo (u32vector-ref (gl-gen-buffers 1) 0))
      (gl-bind-buffer gl-element-array-buffer ebo)
      (gl-buffer-data gl-element-array-buffer (gl-vector-sizeof idx) idx gl-static-draw)
      (gl-bind-vertex-array 0)
      v)))

(define ticker (start-animation canvas 16))

(send frame show #t)
