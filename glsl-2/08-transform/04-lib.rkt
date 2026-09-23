#lang racket/base
;; =========================================================
;; 08-transform/04-lib.rkt —— 第四步：把矩阵工具收进 lib.rkt
;; 运行：racket 08-transform/04-lib.rkt    点 X = 退出
;; =========================================================

;; 前 3 步：平移/旋转/缩放/乘法 四个矩阵函数每次都要重新裸写一遍——纯重复。
;; 本步把它们收进本文件夹 lib.rkt（见同文件夹 lib.rkt 的 mat4-* 部分），
;; 之后每课直接用 mat4-translate / mat4-rot-z / mat4-scale / mat4-mult。
;;
;; ★节奏（整门课通用）：① 裸写一遍新机制（前 3 步，矩阵的数字亲手写过才懂）
;;   ② 发现它重复 → 收进 lib（本步）③ 后面的课直接调用，聚焦新东西。
;;
;; 本步演示和 03 步完全一样（左 = T·R·S 对，右 = R·T·S 错），只是代码变短了：
;;   03 步要自己写 4 个矩阵函数，本步直接用 lib 里的。
;; =========================================================

(require "../04-animate/05-gui-tool.rkt")   ; make-window + start-animation
(require "../racket-glsl/rewrite.rkt")        ; (glsl ...) 宏
(require "../racket-glsl/tool.rkt")           ; build-program / use-program / uniform-location
(require "lib.rkt")                           ; mat4-* + mat4/vec2/u16vector/glsl-*

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

(define verts (vec (vec2 -0.5 -0.5) (vec2 0.5 -0.5) (vec2 0.5 0.5) (vec2 -0.5 0.5)))
(define idx   (u16vector 0 1 2  0 2 3))

(define (draw-square M r g b)
  (gl-uniform-matrix-4fv loc-mvp 1 #f M)
  (gl-uniform-3f loc-color r g b)
  (gl-draw-elements gl-triangles 6 gl-unsigned-short 0))

(define (draw)
  (define t (/ (- (current-inexact-milliseconds) start-ms) 1000.0))
  (define ang (* t 90.0))
  (gl-clear-color 0.10 0.12 0.20 1.0)
  (gl-clear gl-color-buffer-bit)
  (use-program prog)
  (gl-bind-vertex-array vao)
  ;; 左：T·R·S（对）
  (draw-square (mat4-mult (mat4-translate -0.5 0.0)
                          (mat4-mult (mat4-rot-z ang) (mat4-scale 0.5 0.5)))
               0.30 0.65 0.95)
  ;; 右：R·T·S（错）
  (draw-square (mat4-mult (mat4-rot-z ang)
                          (mat4-mult (mat4-translate 0.5 0.0) (mat4-scale 0.5 0.5)))
               0.95 0.70 0.30))

(define-values (frame canvas)
  (make-window #:title "08-04 矩阵工具收进 lib" #:width 400 #:height 400 #:draw draw))

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
