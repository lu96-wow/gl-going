#lang racket/base
;; =========================================================
;; 08-transform/01-translate.rkt —— 第一步：uniform mat4 + 平移矩阵
;; 运行：racket 08-transform/01-translate.rkt    点 X = 退出
;; =========================================================

;; 前七课顶点直接写 NDC 坐标。本步开始用**矩阵**移动它，这是 3D 的第一块地基。
;;
;; 本步新增（2 组）：
;;   ① uniform mat4 + gl-uniform-matrix-4fv —— 上传一个 4×4 矩阵
;;   ② 平移矩阵 + 齐次坐标 —— 为什么偏偏是 4×4
;;
;; ★为什么是 4×4（齐次坐标）：平移是"加"（x+tx），旋转缩放是"乘"（x*s）。
;;   如果平移单独是加法，就没法和旋转缩放统一成"一串矩阵连乘"。齐次坐标的
;;   办法：给顶点补一个 1，写成 (x, y, 0, 1)，再用 4×4 矩阵的第 4 列装平移量：
;;
;;     [1 0 0 tx]   [x]     [x + tx]
;;     [0 1 0 ty] · [y]  =  [y + ty]
;;     [0 0 1 0 ]   [0]     [0     ]
;;     [0 0 0 1 ]   [1]     [1     ]
;;
;;   于是"加"被伪装成"乘"，所有变换统一成矩阵乘法。gl_Position = uMVP * vec4(aPos,0,1)。
;;
;; ★矩阵在内存里的顺序（列主序）：gl-uniform-matrix-4fv 按"列"读 16 个 float。
;;   本课的矩阵也按列存（第 0 列 4 个数、第 1 列 4 个数……）。矩阵的第 4 列
;;   是 (tx, ty, 0, 1)，所以 mat4 的第 12、13 号位置放 tx、ty。
;;
;; 本步视觉：单位方块（中心在原点、边长 1）随时间来回平移——一条 sin 横着
;;   摆、一条 cos 竖着摆，合成椭圆轨迹。
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

;; 平移矩阵（裸写，本步的主角）：4×4 单位阵，第 4 列放 (tx, ty, 0, 1)。
;; 列主序存储：第 c 列的第 r 个元素在下标 c*4+r。
(define (mat4-translate tx ty)
  (mat4 1.0 0.0 0.0 0.0    ; 第 0 列
             0.0 1.0 0.0 0.0    ; 第 1 列
             0.0 0.0 1.0 0.0    ; 第 2 列
             tx  ty  0.0 1.0))  ; 第 3 列 = (tx, ty, 0, 1)

;; 单位方块（EBO）：中心在原点、边长 1，范围 [-0.5, 0.5]。
(define verts (vec (vec2 -0.5 -0.5) (vec2 0.5 -0.5) (vec2 0.5 0.5) (vec2 -0.5 0.5)))
(define idx   (u16vector 0 1 2  0 2 3))

(define (draw)
  (define t (/ (- (current-inexact-milliseconds) start-ms) 1000.0))
  ;; 平移量随时间摆：横着 sin、竖着 cos → 椭圆轨迹
  (define M (mat4-translate (* 0.5 (sin t)) (* 0.3 (cos (* 1.3 t)))))
  (gl-clear-color 0.10 0.12 0.20 1.0)
  (gl-clear gl-color-buffer-bit)
  (use-program prog)
  ;; ★上传矩阵：M 已经是 mat4（f32vector），直接上传（GL 要 float）
  (gl-uniform-matrix-4fv loc-mvp 1 #f M)
  (gl-uniform-3f loc-color 0.30 0.65 0.95)
  (gl-bind-vertex-array vao)
  (gl-draw-elements gl-triangles 6 gl-unsigned-short 0))

(define-values (frame canvas)
  (make-window #:title "08-01 平移矩阵" #:width 400 #:height 400 #:draw draw))

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
