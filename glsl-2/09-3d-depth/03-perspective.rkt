#lang racket/base
;; =========================================================
;; 09-3d-depth/03-perspective.rkt —— 第三步：透视投影（近大远小）
;; 运行：racket 09-3d-depth/03-perspective.rkt    点 X = 退出
;; =========================================================

;; 上一步：实心立方体翻滚，遮挡正确，但"近处和远处一样大"——因为用的是正交
;; 投影（平行光线）。真实世界是**近大远小**，本步换透视投影。
;;
;; 本步新增（2 个，同属"透视"这一件事）：
;;   ① mat4-perspective —— 透视投影矩阵（本步裸写）
;;   ② 透视除法 —— GPU 自动做 xyz÷w，让远的变小
;;
;; ★透视矩阵的关键（本步主角，裸写）：它让 w 分量 = -z（离相机越远 w 越大）。
;;   顶点着色器输出 vec4 后，GPU 固定步骤会自动做"透视除法" xyz÷w——
;;   远处顶点 w 大 → 除完后坐标被挤向中心 → 视觉上更小。这就是近大远小的
;;   数学来源。near/far 之外的顶点会被裁剪掉。
;;
;;   mat4-perspective(fovy, aspect, near, far)：
;;     fovy   = 垂直视角（度），45° 是常见默认
;;     aspect = 宽/高（窗口比例）
;;     near/far = 近/远裁剪面。★别设 0.0001/1e9：z 以非线性方式存进深度缓冲，
;;                near 太小或 far 太大都会让远处深度精度变差，取 0.1/100 量级。
;;
;; 本步视觉：同一个翻滚立方体，换成透视后，近处的棱更长、远处的更短——
;;   立体感一下就出来了。对照上一步的正交投影，能明显看出差别。
;; =========================================================

(require "gui-tool.rkt")              ; make-window（带深度缓冲）+ start-animation
(require "../racket-glsl/rewrite.rkt")
(require "../racket-glsl/rename-vector.rkt")  ; vec3 / concat-vecs / u16vector / glsl-*
(require "../racket-glsl/tool.rkt")
(require "lib.rkt")                   ; mat4-translate / rot-y / rot-x / mult

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

;; 透视投影矩阵（裸写，本步主角）
(define (mat4-perspective fovy aspect near far)
  (define f (/ 1.0 (tan (* 0.5 (/ pi 180.0) fovy))))
  (define nf (/ (+ near far) (- near far)))
  (define n2f (/ (* 2.0 near far) (- near far)))
  (mat4 (/ f aspect) 0.0 0.0 0.0
             0.0 f 0.0 0.0
             0.0 0.0 nf -1.0
             0.0 0.0 n2f 0.0))

;; 立方体数据（同 02 步）
(define pos8
  (list (vec3 -1.0 -1.0  1.0) (vec3  1.0 -1.0  1.0) (vec3  1.0  1.0  1.0) (vec3 -1.0  1.0  1.0)
        (vec3 -1.0 -1.0 -1.0) (vec3  1.0 -1.0 -1.0) (vec3  1.0  1.0 -1.0) (vec3 -1.0  1.0 -1.0)))
(define faces
  (list (list (vec3 0.85 0.20 0.20) '(0 1 2 3))
        (list (vec3 0.20 0.80 0.25) '(5 4 7 6))
        (list (vec3 0.95 0.60 0.10) '(1 5 6 2))
        (list (vec3 0.95 0.85 0.15) '(4 0 3 7))
        (list (vec3 0.20 0.60 0.95) '(3 2 6 7))
        (list (vec3 0.75 0.30 0.90) '(4 5 1 0))))
(define (face-verts f)
  (apply append (for/list ([i (cadr f)]) (list (list-ref pos8 i) (car f)))))
(define verts (apply concat-vecs (apply append (map face-verts faces))))
(define idx
  (apply u16vector
         (apply append
                (for/list ([i (in-range 6)])
                  (define b (* i 4))
                  (list b (+ b 1) (+ b 2)  b  (+ b 2) (+ b 3))))))

(define (draw)
  (define t (/ (- (current-inexact-milliseconds) start-ms) 1000.0))
  (define-values (w h) (send canvas get-gl-client-size))
  (define aspect (/ (exact->inexact w) (exact->inexact h)))
  (define V (mat4-translate 0.0 0.0 -6.0))
  (define P (mat4-perspective 45.0 aspect 0.1 100.0))   ; ★透视投影
  (define M (mat4-mult (mat4-rot-y (* t 40.0)) (mat4-rot-x (* t 30.0))))
  (gl-clear-color 0.10 0.12 0.20 1.0)
  (gl-clear (bitwise-ior gl-color-buffer-bit gl-depth-buffer-bit))
  (use-program prog)
  (gl-uniform-matrix-4fv loc-mvp 1 #f (mat4-mult (mat4-mult P V) M))
  (gl-bind-vertex-array vao)
  (gl-draw-elements gl-triangles 36 gl-unsigned-short 0))

(define-values (frame canvas)
  (make-window #:title "09-03 透视投影（近大远小）" #:width 400 #:height 400 #:draw draw))

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
      (gl-buffer-data gl-array-buffer (gl-vector-sizeof verts) verts gl-static-draw)
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
      (gl-buffer-data gl-element-array-buffer (gl-vector-sizeof idx) idx gl-static-draw)
      (gl-bind-vertex-array 0)
      v)))

(define ticker (start-animation canvas 16))

(send frame show #t)
