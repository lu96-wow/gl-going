#lang racket/base
;; =========================================================
;; 08-transform/05-ortho.rkt —— 第五步：正交投影（像素世界 → NDC）
;; 运行：racket 08-transform/05-ortho.rkt    点 X = 退出
;; =========================================================

;; 前四步：方块在 NDC（-1..1）里转，坐标很不直观（"0.5 是多长？"）。本步
;; 引入**投影矩阵**，让你能用"像素"来定位物体。
;;
;; 本步新增（1 个）：
;;   正交投影矩阵 —— 把像素矩形映射到 NDC，从此用像素坐标写位置
;;
;; ★为什么需要投影：顶点着色器必须输出 NDC（-1..1），但人想写像素
;;   （"中心在 (200,200)、宽 300"）。投影矩阵 P 就是"像素 → NDC"的换算：
;;     x: 0..w   → -1..1   （÷w 缩放成 0..1，再 ×2-1 到 -1..1）
;;     y: 0..h   → +1..-1  （同上，但要翻转：像素 y 向下，NDC y 向上）
;;   于是 gl_Position = P · M · vec4(顶点,0,1)，其中 M 是模型矩阵、P 是投影。
;;
;; ★正交投影矩阵的"拼法"（本步主角，裸写）：
;;   它 = 缩放 + 翻转 + 平移三件事合进一个矩阵。以 (l=0, r=w, b=h, t=0) 为例：
;;     [2/w  0    0   -1]     第 0 列：x 方向缩放 2/w
;;     [0   -2/h  0    1]     第 1 列：y 方向缩放 2/h 并翻号（翻转）
;;     [0    0   -1   0]      z 压到 -1..1（2D 无所谓）
;;     [0    0    0   1]      第 3 列：平移，把左上角 (0,0) 移到 NDC (-1,1)
;;
;; 本步视觉：一个 300×200 像素的方块，中心在窗口 (w/2, h/2)，绕自己中心转。
;;   现在你写的是像素——"中心在哪、多大"一眼可读。
;; =========================================================

(require "../04-animate/05-gui-tool.rkt")   ; make-window + start-animation
(require "../racket-glsl/rewrite.rkt")        ; (glsl ...) 宏
(require "../racket-glsl/tool.rkt")           ; build-program / use-program / uniform-location
(require "lib.rkt")                           ; mat4-translate/rot-z/scale/mult（04 步收的）

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

;; 正交投影（裸写，本步主角）：把 [l,r]×[b,t] 映射到 NDC。
;; 这里 l=0,r=w,b=h,t=0（像素世界、左上原点、y 向下），n=-1,f=1。
(define (mat4-ortho l r b t n f)
  (define rl (- r l)) (define tb (- t b)) (define fn (- f n))
  (mat4 (/ 2.0 rl) 0.0 0.0 0.0
             0.0 (/ 2.0 tb) 0.0 0.0
             0.0 0.0 (/ -2.0 fn) 0.0
             (- (/ (+ r l) rl)) (- (/ (+ t b) tb)) (- (/ (+ f n) fn)) 1.0))

;; 单位方块：中心在原点、边长 1
(define verts (vec (vec2 -0.5 -0.5) (vec2 0.5 -0.5) (vec2 0.5 0.5) (vec2 -0.5 0.5)))
(define idx   (u16vector 0 1 2  0 2 3))

(define (draw)
  (define t (/ (- (current-inexact-milliseconds) start-ms) 1000.0))
  ;; 真实像素尺寸（和 gl-viewport 用的同一个数）
  (define-values (w h) (send canvas get-gl-client-size))
  (define P (mat4-ortho 0.0 (exact->inexact w) (exact->inexact h) 0.0 -1.0 1.0))
  ;; 模型矩阵：先缩放(300×200 像素) 再旋转 再平移到窗口中心
  (define M (mat4-mult (mat4-translate (/ w 2.0) (/ h 2.0))
                       (mat4-mult (mat4-rot-z (* t 60.0))
                                  (mat4-scale 300.0 200.0))))
  (gl-clear-color 0.10 0.12 0.20 1.0)
  (gl-clear gl-color-buffer-bit)
  (use-program prog)
  (gl-uniform-matrix-4fv loc-mvp 1 #f (mat4-mult P M))
  (gl-uniform-3f loc-color 0.30 0.65 0.95)
  (gl-bind-vertex-array vao)
  (gl-draw-elements gl-triangles 6 gl-unsigned-short 0))

(define-values (frame canvas)
  (make-window #:title "08-05 正交投影（像素世界）" #:width 400 #:height 400 #:draw draw))

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
