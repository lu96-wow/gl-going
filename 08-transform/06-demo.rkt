#lang racket/base
;; =========================================================
;; 08-transform/06-demo.rkt —— 第六步：综合，像素世界里的旋转与公转
;; 运行：racket 08-transform/06-demo.rkt    点 X = 退出
;; =========================================================

;; 本课前五步：平移(01)、旋转缩放(02)、矩阵乘法与顺序(03)、收进 lib(04)、
;; 正交投影像素世界(05)。本步**不引入新语法**，把"像素世界 + 变换"拼成成品，
;; 顺带把 05 步裸写的 mat4-ortho 也收进了 lib.rkt。
;;
;; 画面（800×600 像素世界）：
;;   中央大矩形：绕自己中心自转
;;   右上角小方块：绕大矩形中心公转 + 自己自转
;;   中心小红点：标记大矩形中心
;;
;; 每画一个物体，就是给它拼一个模型矩阵 M = T·R·S（先缩放再旋转最后平移），
;; 再乘上投影 P，一次上传。所有"位置/朝向/大小"都在这一个矩阵里。
;; =========================================================

(require "../04-animate/05-gui-tool.rkt")   ; make-window + start-animation
(require "../racket-glsl/rewrite.rkt")        ; (glsl ...) 宏
(require "../racket-glsl/tool.rkt")           ; build-program / use-program / uniform-location
(require "lib.rkt")                           ; mat4-* 全在 lib 里（含 mat4-ortho）

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

;; 单位方块：中心在原点、边长 1
(define verts (vec (vec2 -0.5 -0.5) (vec2 0.5 -0.5) (vec2 0.5 0.5) (vec2 -0.5 0.5)))
(define idx   (u16vector 0 1 2  0 2 3))

(define (draw)
  (define t (/ (- (current-inexact-milliseconds) start-ms) 1000.0))
  (define-values (w h) (send canvas get-gl-client-size))
  (define P (mat4-ortho 0.0 (exact->inexact w) (exact->inexact h) 0.0 -1.0 1.0))

  ;; 画一个"单位方块"：给中心(cx,cy)、半宽半高(hx,hy)、角度、颜色
  (define (quad cx cy hx hy ang r g b)
    (define S (mat4-scale (* 2.0 hx) (* 2.0 hy)))
    (define R (mat4-rot-z ang))
    (define T (mat4-translate cx cy))
    (define M (mat4-mult T (mat4-mult R S)))   ; T·R·S（先缩放再旋转最后平移）
    (gl-uniform-matrix-4fv loc-mvp 1 #f (mat4-mult P M))
    (gl-uniform-3f loc-color r g b)
    (gl-draw-elements gl-triangles 6 gl-unsigned-short 0))

  (gl-clear-color 0.10 0.12 0.20 1.0)
  (gl-clear gl-color-buffer-bit)
  (use-program prog)
  (gl-bind-vertex-array vao)

  ;; 中央大矩形：绕自己中心转
  (quad (/ w 2.0) (/ h 2.0) 150.0 90.0 (* t 55.0) 0.30 0.65 0.95)
  ;; 右上角小方块：公转（绕大矩形中心）+ 自转
  (define a (* t 95.0))
  (define rad (* (/ pi 180.0) a))
  (define ox (+ (/ w 2.0) (* 200.0 (cos rad))))
  (define oy (+ (/ h 2.0) (* 150.0 (sin rad))))
  (quad ox oy 34.0 34.0 (* t 200.0) 0.95 0.70 0.30)
  ;; 大矩形中心的小标记
  (quad (/ w 2.0) (/ h 2.0) 6.0 6.0 0.0 1.0 0.3 0.3))

(define-values (frame canvas)
  (make-window #:title "08-06 旋转与公转（综合）" #:width 800 #:height 600 #:draw draw))

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
