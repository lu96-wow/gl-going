#lang racket/base
;; =========================================================
;; 10-camera/02-orbit.rkt —— 第二步：球坐标轨道相机
;; 运行：racket 10-camera/02-orbit.rkt    点 X = 退出
;; =========================================================

;; 上一步：相机固定在 (5,3,5)。本步让相机**绕场景转**——轨道相机。
;;
;; 本步新增（1 个）：
;;   球坐标 → 眼睛位置 —— 用 yaw/pitch/dist 三个数算出相机站在哪
;;
;; ★球坐标（轨道相机的经典参数化）：目标在原点，相机的位置由三个数决定：
;;   yaw（经度，绕 y 转多少）、pitch（纬度/仰角）、dist（离目标多远）。
;;   反推成直角坐标：
;;     eye.x = dist · cos(pitch) · sin(yaw)
;;     eye.y = dist · sin(pitch)
;;     eye.z = dist · cos(pitch) · cos(yaw)
;;   然后 V = lookAt(eye, 原点, (0,1,0))——上一步的函数直接用。
;;   "相机在绕"不过是每帧重算一次 eye、再算一次 V 上传，场景代码不变。
;;
;; 本步视觉：相机自动环绕（yaw 随时间走），网格地面和立方体在视野里转圈。
;; =========================================================

(require "gui-tool.rkt")
(require "../racket-glsl/rewrite.rkt")
(require "../racket-glsl/tool.rkt")
(require "lib.rkt")                    ; mat4-perspective / mat4-* + cube-verts/cube-idx

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

;; 视图矩阵（同 01 步；04 步收进 lib）
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

;; 网格地面（同 01 步）
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

(define (draw)
  (define t (/ (- (current-inexact-milliseconds) start-ms) 1000.0))
  (define-values (w h) (send canvas get-gl-client-size))
  (define aspect (/ (exact->inexact w) (exact->inexact h)))
  (define P (mat4-perspective 45.0 aspect 0.1 100.0))

  ;; ★球坐标 → 眼睛位置：yaw 随时间走，pitch/dist 固定
  (define yaw (* t 30.0))
  (define pitch 20.0)
  (define dist 9.0)
  (define ph (* (/ PI 180.0) pitch))
  (define ya (* (/ PI 180.0) yaw))
  (define ex (* dist (cos ph) (sin ya)))
  (define ey (* dist (sin ph)))
  (define ez (* dist (cos ph) (cos ya)))
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
  (make-window #:title "10-02 轨道相机（自动环绕）" #:width 600 #:height 600 #:draw draw))

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
