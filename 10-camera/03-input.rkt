#lang racket/base
;; =========================================================
;; 10-camera/03-input.rkt —— 第三步：鼠标 / 键盘交互
;; 运行：racket 10-camera/03-input.rkt
;; 操作：左键拖 = 环绕   滚轮 = 拉近拉远   R = 复位   ESC/点X = 退出
;; =========================================================

;; 上一步：相机自动环绕。本步把控制交给鼠标——拖拽环绕、滚轮拉近拉远。
;;
;; 本步新增（1 组，同属"交互输入"这一件事）：
;;   on-event —— 鼠标事件（按下/抬起/拖动）
;;   on-char  —— 键盘事件（R 复位 / ESC 退出）+ 滚轮（★滚轮是键盘事件！）
;;
;; ★事件驱动：racket/gui 没有"轮询鼠标"，而是回调。拖动 = 按下时记起点、
;;   移动时按位移改 yaw/pitch；滚轮 = 直接改 dist。每次改完，下一帧 draw
;;   用新的 yaw/pitch/dist 重算 V——相机就跟着鼠标动了。
;;   pitch 夹在 -85°..85°（别翻过头顶），dist 夹在 2.5..20（别穿模/飞太远）。
;;
;; ★注意：这些回调只改几个 box 里的数字（yaw/pitch/dist），不碰 GL；
;;   真正用它们的是 draw（每帧算 V）。状态与渲染分离。
;; =========================================================

(require "gui-tool.rkt")
(require "../racket-glsl/rewrite.rkt")
(require "../racket-glsl/tool.rkt")
(require "lib.rkt")

(define PI (acos -1.0))
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

;; 视图矩阵（同前两步，下一步收进 lib）
(define (mat4-look-at ex ey ez cx cy cz ux uy uz)
  (define fx (- cx ex)) (define fy (- cy ey)) (define fz (- cz ez))
  (define fl (sqrt (+ (* fx fx) (* fy fy) (* fz fz))))
  (define fxx (/ fx fl)) (define fyy (/ fy fl)) (define fzz (/ fz fl))
  (define sx (- (* fyy uz) (* fzz uy)))
  (define sy (- (* fzz ux) (* fxx uz)))
  (define sz (- (* fxx uy) (* fyy ux)))
  (define sl (sqrt (+ (* sx sx) (* sy sy) (* sz sz))))
  (define sxx (/ sx sl)) (define syy (/ sy sl)) (define szz (/ sz sl))
  (define uxx (- (* syy fzz) (* szz fyy)))
  (define uyy (- (* szz fxx) (* sxx fzz)))
  (define uzz (- (* sxx fyy) (* syy fxx)))
  (mat4 sxx uxx (- fxx) 0.0
             syy uyy (- fyy) 0.0
             szz uzz (- fzz) 0.0
             (- (+ (* sxx ex) (* syy ey) (* szz ez)))
             (- (+ (* uxx ex) (* uyy ey) (* uzz ez)))
             (+ (* fxx ex) (* fyy ey) (* fzz ez))
             1.0))

(define (grid-verts span step)
  (define color (vec3 0.30 0.32 0.50))
  (apply concat-vecs
         (apply append
                (for/list ([s (in-range (- span) (+ span step) step)])
                  (list (vec3 (- span) 0.0 s) color
                        (vec3 span 0.0 s) color
                        (vec3 s 0.0 (- span)) color
                        (vec3 s 0.0 span) color)))))
(define grid (grid-verts 5.0 0.5))
(define grid-count (quotient (f32vector-length grid) 6))

;; ---- 相机状态（输入回调只改这几个 box）----
(define yaw (box 30.0)) (define pitch (box 20.0)) (define dist (box 9.0))
(define drag? (box #f)) (define last-px (box 0.0)) (define last-py (box 0.0))

;; 鼠标：左键拖 = 环绕；滚轮 = 拉近拉远
(define (on-event e)
  (define et (send e get-event-type))
  (cond
    [(send e button-down? 'left)
     (set-box! drag? #t)
     (set-box! last-px (send e get-x))
     (set-box! last-py (send e get-y))]
    [(send e button-up? 'left)
     (set-box! drag? #f)]
    [(eq? et 'motion)
     (when (unbox drag?)
       (define px (send e get-x))
       (define py (send e get-y))
       (define sens 0.25)
       (set-box! yaw (+ (unbox yaw) (* (- px (unbox last-px)) sens)))
       (set-box! pitch (max -85.0 (min 85.0
                                        (+ (unbox pitch) (* (- (unbox last-py) py) sens)))))
       (set-box! last-px px)
       (set-box! last-py py))]))

;; 键盘：滚轮缩放、R 复位、ESC 退出
(define (on-char e)
  (define code (send e get-key-code))
  (when (not (eq? code 'release))
    (cond
      ;; ★滚轮是"键盘事件"（key-event%），不是鼠标事件：在 on-char 里处理，
      ;;   key-code 是 'wheel-up / 'wheel-down（GTK/Win32/Cocoa 三平台一致）。
      [(eq? code 'wheel-up)   (set-box! dist (max 2.5 (min 20.0 (* (unbox dist) 0.88))))]
      [(eq? code 'wheel-down) (set-box! dist (max 2.5 (min 20.0 (* (unbox dist) 1.12))))]
      [(or (eq? code #\r) (eq? code #\R))
       (set-box! yaw 30.0) (set-box! pitch 20.0) (set-box! dist 9.0)]
      [(eq? code 'escape) (exit 0)])))

(define (draw)
  (define t (/ (- (current-inexact-milliseconds) start-ms) 1000.0))
  (define-values (w h) (send canvas get-gl-client-size))
  (define aspect (/ (exact->inexact w) (exact->inexact h)))
  (define P (mat4-perspective 45.0 aspect 0.1 100.0))

  ;; 球坐标 → 眼睛 → V（yaw/pitch/dist 由鼠标/键盘改）
  (define ph (* (/ PI 180.0) (unbox pitch)))
  (define ya (* (/ PI 180.0) (unbox yaw)))
  (define ex (* (unbox dist) (cos ph) (sin ya)))
  (define ey (* (unbox dist) (sin ph)))
  (define ez (* (unbox dist) (cos ph) (cos ya)))
  (define V (mat4-look-at ex ey ez  0.0 0.0 0.0  0.0 1.0 0.0))

  (gl-clear-color 0.07 0.08 0.14 1.0)
  (gl-clear (bitwise-ior gl-color-buffer-bit gl-depth-buffer-bit))
  (use-program prog)

  (gl-uniform-matrix-4fv loc-mvp 1 #f (mat4-mult P V))
  (gl-bind-vertex-array vao-grid)
  (gl-draw-arrays gl-lines 0 grid-count)

  (define M (mat4-mult (mat4-translate 0.0 1.0 0.0)
                       (mat4-mult (mat4-mult (mat4-rot-y (* t 60.0)) (mat4-rot-x (* t 40.0)))
                                  (mat4-scale 0.8 0.8 0.8))))
  (gl-uniform-matrix-4fv loc-mvp 1 #f (mat4-mult (mat4-mult P V) M))
  (gl-bind-vertex-array vao-cube)
  (gl-draw-elements gl-triangles 36 gl-unsigned-short 0))

(define-values (frame canvas)
  (make-window #:title "10-03 鼠标轨道相机"
               #:width 600 #:height 600
               #:draw draw
               #:on-event on-event
               #:on-char on-char))

(define prog
  (send canvas with-gl-context
    (lambda () (build-program (gl-vertex-shader vert-src) (gl-fragment-shader frag-src)))))
(define loc-mvp
  (send canvas with-gl-context (lambda () (uniform-location prog "uMVP"))))
(define vao-grid
  (send canvas with-gl-context
    (lambda ()
      (gl-enable gl-depth-test)
      (define vbo (u32vector-ref (gl-gen-buffers 1) 0))
      (gl-bind-buffer gl-array-buffer vbo)
      (gl-buffer-data gl-array-buffer (gl-vector-sizeof grid) grid gl-static-draw)
      (define v (u32vector-ref (gl-gen-vertex-arrays 1) 0))
      (gl-bind-vertex-array v)
      (gl-vertex-attrib-pointer 0 (glsl-size 'vec3) gl-float #f (glsl-stride-bytes 'vec3 'vec3) 0)
      (gl-enable-vertex-attrib-array 0)
      (gl-vertex-attrib-pointer 1 (glsl-size 'vec3) gl-float #f
                                (glsl-stride-bytes 'vec3 'vec3)
                                (glsl-stride-bytes 'vec3))
      (gl-enable-vertex-attrib-array 1)
      (gl-bind-vertex-array 0)
      v)))
(define vao-cube
  (send canvas with-gl-context
    (lambda ()
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
(send canvas focus)   ; 让键盘事件（滚轮 / R / ESC）落到画布
