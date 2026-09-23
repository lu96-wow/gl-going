#lang racket/base
;; =========================================================
;; 10-camera/06-gpu.rkt —— 第六步（附）：把相机矩阵搬进 GPU
;; 运行：racket 10-camera/06-gpu.rkt
;; 操作：左键拖 = 环绕   滚轮 = 拉近拉远   R = 复位   ESC/点X = 退出
;; =========================================================

;; 前五步 V（视图）和 P（透视）都在 CPU 拼好、整块上传。本步反过来：
;; CPU 只传 5 个 float（yaw/pitch/dist/fovy/aspect），V 和 P 在顶点着色器里现算。
;;
;; ★对照：矩阵在哪算，看"谁每帧算几次"。
;;   之前：CPU 每帧算一次 V/P（就几次）；GPU 每个顶点算一次 P·V·M·v（几十万次）。
;;   本步：CPU 连 V/P 都不算了，直接传参数；GPU 每个顶点多算一次 V/P——
;;         其实每个顶点都重算一遍同样的 V/P，是浪费，数值一样，这里只为演示。
;;   真实权衡：V/P 是"每帧一次"的全局量，放哪边都行，差别可忽略；真正该放
;;   GPU 的是"每物体/每顶点"的矩阵（实例化、骨骼、粒子）。本步只演示语法。
;;
;; 本步**不引入新 GL API**：gl-uniform-1f（04 课）、gl-uniform-matrix-4fv（08 课）。
;; 新东西是 GLSL 里的自写函数（06 课学过 define）+ 在 shader 里拼矩阵。
;; =========================================================

(require "gui-tool.rkt")
(require "../racket-glsl/rewrite.rkt")
(require "../racket-glsl/tool.rkt")
(require "lib.rkt")

(define start-ms (current-inexact-milliseconds))

;; 顶点着色器：V 和 P 都在 shader 里算。
;;   lookAt / perspective 两个自写函数 = CPU 侧 lib.rkt 的 mat4-look-at / mat4-perspective，
;;   只是换到 GLSL 里写（写法一模一样：拼 4 个列向量喂给 mat4 构造器）。
(define vert-src
  (glsl (version 330 core)
        (layout (location 0) in vec3 aPos)
        (layout (location 1) in vec3 aColor)
        (uniform float uYaw)
        (uniform float uPitch)
        (uniform float uDist)
        (uniform float uFovy)
        (uniform float uAspect)
        (uniform mat4 uM)              ; 模型矩阵仍由 CPU 每物体传（本步不动它）
        (out vec3 vColor)
        ;; 视图矩阵：CPU 只传 yaw/pitch/dist 三个数，lookAt 在 shader 里现算。
        ;; 与 CPU 的 mat4-look-at 写法一致：16 个标量按列主序喂给 mat4——
        ;;   列 0=(s.x,u.x,-f.x,0)  列 1=(s.y,u.y,-f.y,0)
        ;;   列 2=(s.z,u.z,-f.z,0)  列 3=(-s.e,-u.e,f.e,1)
        ;; 注意别写成 (mat4 (vec4 s ..) (vec4 u ..) (vec4 -f ..) ..)：
        ;; 那是把"行"当"列"，会得到转置矩阵，立方体就扭曲成面/缺面了。
        (define (lookAt (vec3 eye) (vec3 center) (vec3 up)) mat4
          (vec3 f (normalize (- center eye)))
          (vec3 s (normalize (cross f up)))
          (vec3 u (cross s f))
          (mat4 (x s) (x u) (- (x f)) 0.0
                (y s) (y u) (- (y f)) 0.0
                (z s) (z u) (- (z f)) 0.0
                (- (dot s eye)) (- (dot u eye)) (dot f eye) 1.0))
        ;; 透视矩阵：同样在 shader 里现算（fovy 是角度，radians 转弧度）。
        (define (perspective (float fovy) (float aspect) (float near) (float far)) mat4
          (float f (/ 1.0 (tan (* (radians fovy) 0.5))))
          (mat4 (vec4 (/ f aspect) 0.0 0.0 0.0)
                (vec4 0.0 f 0.0 0.0)
                (vec4 0.0 0.0 (/ (+ near far) (- near far)) -1.0)
                (vec4 0.0 0.0 (/ (* 2.0 near far) (- near far)) 0.0)))
        (define (main) void
          (float ph (radians uPitch))
          (float ya (radians uYaw))
          (vec3 eye (vec3 (* uDist (cos ph) (sin ya))
                          (* uDist (sin ph))
                          (* uDist (cos ph) (cos ya))))
          (mat4 V (lookAt eye (vec3 0.0 0.0 0.0) (vec3 0.0 1.0 0.0)))
          (mat4 P (perspective uFovy uAspect 0.1 100.0))
          (set! vColor aColor)
          (set! gl_Position (* (* (* P V) uM) (vec4 aPos 1.0))))))

(define frag-src
  (glsl (version 330 core)
        (in vec3 vColor)
        (out vec4 FragColor)
        (define (main) void
          (set! FragColor (vec4 vColor 1.0)))))

(define grid (grid-verts 5.0 0.5))
(define grid-count (quotient (f32vector-length grid) 6))

(define yaw (box 30.0)) (define pitch (box 20.0)) (define dist (box 9.0))
(define drag? (box #f)) (define last-px (box 0.0)) (define last-py (box 0.0))

(define (on-event e)
  (define et (send e get-event-type))
  (cond
    [(send e button-down? 'left)
     (set-box! drag? #t)
     (set-box! last-px (send e get-x))
     (set-box! last-py (send e get-y))]
    [(send e button-up? 'left) (set-box! drag? #f)]
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

(define (on-char e)
  (define code (send e get-key-code))
  (when (not (eq? code 'release))
    (cond
      [(eq? code 'wheel-up)   (set-box! dist (max 2.5 (min 20.0 (* (unbox dist) 0.88))))]
      [(eq? code 'wheel-down) (set-box! dist (max 2.5 (min 20.0 (* (unbox dist) 1.12))))]
      [(or (eq? code #\r) (eq? code #\R))
       (set-box! yaw 30.0) (set-box! pitch 20.0) (set-box! dist 9.0)]
      [(eq? code 'escape) (exit 0)])))

(define (draw)
  (define t (/ (- (current-inexact-milliseconds) start-ms) 1000.0))
  (define-values (w h) (send canvas get-gl-client-size))
  (define aspect (/ (exact->inexact w) (exact->inexact h)))
  (gl-clear-color 0.07 0.08 0.14 1.0)
  (gl-clear (bitwise-ior gl-color-buffer-bit gl-depth-buffer-bit))
  (use-program prog)
  ;; ★不再 (mat4-mult (mat4-mult P V) M) 整块上传，而是传 5 个参数 + 每物体的 M：
  (gl-uniform-1f loc-yaw (unbox yaw))
  (gl-uniform-1f loc-pitch (unbox pitch))
  (gl-uniform-1f loc-dist (unbox dist))
  (gl-uniform-1f loc-fovy 45.0)
  (gl-uniform-1f loc-aspect aspect)

  ;; 统一入口：给定模型矩阵画立方体（M 仍由 CPU 拼——它是"每物体"的量）
  (define (draw-cube m)
    (gl-uniform-matrix-4fv loc-m 1 #f m)
    (gl-bind-vertex-array vao-cube)
    (gl-draw-elements gl-triangles 36 gl-unsigned-short 0))

  ;; 地面网格（M = 单位阵）
  (gl-uniform-matrix-4fv loc-m 1 #f (mat4-identity))
  (gl-bind-vertex-array vao-grid)
  (gl-draw-arrays gl-lines 0 grid-count)

  ;; 中央翻滚立方体
  (draw-cube (mat4-mult (mat4-translate 0.0 1.0 0.0)
                        (mat4-mult (mat4-mult (mat4-rot-y (* t 60.0)) (mat4-rot-x (* t 40.0)))
                                   (mat4-scale 0.8 0.8 0.8))))
  ;; 三颗散落小立方体
  (for ([p (list (list 2.4 0.5 -1.7) (list -2.5 0.6 -1.3) (list 1.7 0.4 2.1))])
    (draw-cube (mat4-mult (mat4-translate (car p) (cadr p) (caddr p))
                          (mat4-mult (mat4-rot-y (* t 90.0)) (mat4-scale 0.5 0.5 0.5))))))

(define-values (frame canvas)
  (make-window #:title "10-06 相机矩阵搬进 GPU（对照）"
               #:width 800 #:height 600
               #:draw draw
               #:on-event on-event
               #:on-char on-char))

(define prog
  (send canvas with-gl-context
    (lambda () (build-program (gl-vertex-shader vert-src) (gl-fragment-shader frag-src)))))
(define loc-yaw
  (send canvas with-gl-context (lambda () (uniform-location prog "uYaw"))))
(define loc-pitch
  (send canvas with-gl-context (lambda () (uniform-location prog "uPitch"))))
(define loc-dist
  (send canvas with-gl-context (lambda () (uniform-location prog "uDist"))))
(define loc-fovy
  (send canvas with-gl-context (lambda () (uniform-location prog "uFovy"))))
(define loc-aspect
  (send canvas with-gl-context (lambda () (uniform-location prog "uAspect"))))
(define loc-m
  (send canvas with-gl-context (lambda () (uniform-location prog "uM"))))

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
(send canvas focus)
