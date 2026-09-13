#lang racket/base
;; =========================================================
;; 04-animate/02-uniform.rkt —— 把时间传进 shader（uniform）
;; 运行：racket 04-animate/02-uniform.rkt    点 X = 退出
;; =========================================================

;; 上一步让帧持续发生，但每帧画的东西都一样。本步让"每帧不同"：
;; 把时间传进 shader，三角形左右滑动。
;;
;; 本步新增两个（同属"传参数给 shader"这一件事）：
;;   uniform     —— shader 里声明"本次 draw 全体共用的参数"
;;   gl-uniform-1f —— 从 CPU 上传这个参数的值（1 个 float）
;;
;; ★uniform 和 attribute(in) 正好互补：
;;   attribute(in) = 每个顶点一份 → 存在 VBO 里，顶点着色器每顶点取一次
;;   uniform       = 每次 draw 一份 → 用 gl-uniform-* 从 CPU 上传，本次 draw 通吃
;;   时间对所有顶点都一样，所以是 uniform 而不是 attribute。
;;
;; ★用法三件套：
;;   (uniform float uTime)        shader 里声明（名字自己起）
;;   (uniform-location prog ...)  查这个 uniform 的"位置号"（只查一次）
;;   (gl-uniform-1f 位置号 值)       每帧上传新值（必须在 use-program 之后）

(require "../02-triangle/04-gui-tool.rkt")
(require "../racket-glsl/rewrite.rkt")
(require "../racket-glsl/rename-vector.rkt")
(require "../racket-glsl/tool.rkt")

(define start-ms (current-inexact-milliseconds))

;; 顶点着色器：uniform uTime，给 x 加一个随 sin(uTime) 变化的偏移 → 左右滑动。
(define vert-src
  (glsl (version 330 core)
        (layout (location 0) in vec2 aPos)
        (uniform float uTime)                                     ; ★声明：本次 draw 全体共用
        (define (main) void
          (set! gl_Position
                (vec4 (+ aPos (vec2 (* 0.5 (sin uTime)) 0.0))     ; x 随 sin 摆，y 不动
                      0.0 1.0)))))

(define frag-src
  (glsl (version 330 core)
        (out vec4 FragColor)
        (define (main) void
          (set! FragColor (vec4 1.0 0.35 0.2 1.0)))))

;; 顶点数据（同 02-03：vec2 = 2 维向量，vec = 打包成连续缓冲，不再解释）。
(define verts
  (vec (vec2 -0.5 -0.5)
       (vec2  0.5 -0.5)
       (vec2  0.0  0.5)))

;; 每帧：量时间 → 上传 uTime → 画。
;; 注意顺序：gl-uniform-1f 必须在 use-program 之后（uniform 位置属于某个程序）。
(define (draw)
  (define t (/ (- (current-inexact-milliseconds) start-ms) 1000.0))
  (gl-clear-color 0.10 0.12 0.20 1.0)  ; 清屏（draw 自己负责）
  (gl-clear gl-color-buffer-bit)
  (use-program prog)
  (gl-uniform-1f loc-time t)
  (gl-bind-vertex-array vao)
  (gl-draw-arrays gl-triangles 0 3))

(define-values (frame canvas)
  (make-window #:title "04-02 三角形动起来" #:width 400 #:height 400 #:draw draw))

;; 初始化：程序 + 查 uniform 位置 + VAO。
(define prog
  (send canvas with-gl-context
    (lambda () (build-program (gl-vertex-shader vert-src) (gl-fragment-shader frag-src)))))
(define loc-time
  (send canvas with-gl-context (lambda () (uniform-location prog "uTime"))))
(define vao
  (send canvas with-gl-context
    (lambda ()
      (define data (vec->f32vector verts))
      (define vbo (u32vector-ref (gl-gen-buffers 1) 0))
      (gl-bind-buffer gl-array-buffer vbo)
      (gl-buffer-data gl-array-buffer (gl-vector-sizeof data) data gl-static-draw)
      (define v (u32vector-ref (gl-gen-vertex-arrays 1) 0))
      (gl-bind-vertex-array v)
      (gl-vertex-attrib-pointer 0 2 gl-float #f 8 0)
      (gl-enable-vertex-attrib-array 0)
      (gl-bind-vertex-array 0)
      v)))

;; 定时器（同 01）：每 16ms 触发重画。
(define ticker
  (new timer% (interval 16) (notify-callback (lambda () (send canvas refresh)))))

(send frame show #t)
