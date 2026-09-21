#lang racket/base
;; =========================================================
;; 07-primitives/06-demo.rkt —— 第六步：综合，一个窗口看全图元 + 索引
;; 运行：racket 07-primitives/06-demo.rkt    点 X = 退出
;; =========================================================

;; 本课前五步分别讲了：顶点颜色(01)、点(02)、线(03)、三角形条带(04)、
;; EBO 索引(05)。本步**不引入任何新语法**，把它们放进同一个窗口对比着看。
;;
;; 画面布局（600×600，NDC 坐标）：
;;   顶部   ：一横排点      gl-points        （02 步）
;;   中部   ：波浪折线      gl-line-strip    （03 步）
;;   右下   ：四边形条带    gl-triangle-strip（04 步，4 顶点画 2 三角形）
;;   左下   ：同款四边形    EBO + gl-draw-elements（05 步，4 顶点 + 6 索引）
;;
;; ★核心对比：右下和左下两个四边形**长得一模一样**，区别只在数据组织——
;;   右下 = 4 个顶点按 strip 顺序直接画；左下 = 4 个角 + 6 个编号（索引）。
;;   这就是"图元类型"和"索引"两种省顶点方式同台亮相。
;; =========================================================

(require "../02-triangle/04-gui-tool.rkt")   ; make-window（带视口）
(require "../racket-glsl/rewrite.rkt")        ; (glsl ...) 宏
(require "../racket-glsl/rename-vector.rkt")  ; vec2/vec3/concat-vecs/glsl-size/glsl-stride-bytes + u16vector
(require "../racket-glsl/tool.rkt")           ; build-program / use-program

(define vert-src
  (glsl (version 330 core)
        (layout (location 0) in vec2 aPos)
        (layout (location 1) in vec3 aColor)
        (out vec3 vColor)
        (define (main) void
          (set! vColor aColor)
          (set! gl_PointSize 14.0)      ; 只有画点（顶部那排）时生效
          (set! gl_Position (vec4 aPos 0.0 1.0)))))

(define frag-src
  (glsl (version 330 core)
        (in vec3 vColor)
        (out vec4 FragColor)
        (define (main) void
          (set! FragColor (vec4 vColor 1.0)))))

;; 小工具：一个顶点 = (位置 vec2)(颜色 vec3)，返回两个 f32vector 的 list。
(define (v x y r g b) (list (vec2 x y) (vec3 r g b)))

;; 每幅图的顶点数（用来写 figure-info 的"起点/数量"）。
(define npts 5)
(define nzig 7)
(define nstrip 4)

;; ① 顶部：5 个点
(define pts
  (append (v -0.8 0.70 1.0 0.3 0.3) (v -0.4 0.70 0.9 0.5 0.2)
          (v  0.0 0.70 0.9 0.9 0.2) (v  0.4 0.70 0.3 0.9 0.4)
          (v  0.8 0.70 0.3 0.6 1.0)))
;; ② 中部：波浪折线 7 点
(define zig
  (append (v -0.9 -0.15 0.3 0.8 1.0) (v -0.6  0.35 0.3 0.8 1.0)
          (v -0.3 -0.15 0.3 0.8 1.0) (v  0.0  0.35 0.3 0.8 1.0)
          (v  0.3 -0.15 0.3 0.8 1.0) (v  0.6  0.35 0.3 0.8 1.0)
          (v  0.9 -0.15 0.3 0.8 1.0)))
;; ③ 右下：四边形条带 4 顶点
(define strip
  (append (v 0.35 -0.75 0.3 0.9 0.5) (v 0.35 -0.30 0.9 0.9 0.3)
          (v 0.95 -0.75 0.5 0.3 1.0) (v 0.95 -0.30 1.0 0.4 0.3)))

;; 三幅图拼进同一块缓冲，并记下每幅的 (图元类型 起点 顶点数)。
(define figs (apply concat-vecs (append pts zig strip)))
(define figure-info
  (list (list gl-points        0 npts)
        (list gl-line-strip    npts nzig)
        (list gl-triangle-strip (+ npts nzig) nstrip)))

;; ④ 左下：同款四边形，但用"4 角 + 6 索引"（EBO）
(define corners
  (append (v -0.95 -0.75 1.0 0.5 0.2) (v -0.40 -0.75 0.9 0.9 0.2)
          (v -0.40 -0.30 0.2 0.9 0.4) (v -0.95 -0.30 0.3 0.5 1.0)))
(define everts (apply concat-vecs corners))
(define eidx (u16vector 0 1 2  0 2 3))

(define (draw)
  (gl-clear-color 0.10 0.12 0.20 1.0)
  (gl-clear gl-color-buffer-bit)
  (use-program prog)
  ;; 三个 gl-draw-arrays 图（同一块缓冲，各画各的拓扑）
  (gl-bind-vertex-array vao-figs)
  (for ([fi figure-info])
    (gl-draw-arrays (list-ref fi 0) (list-ref fi 1) (list-ref fi 2)))
  ;; EBO 四边形：按索引取顶点
  (gl-bind-vertex-array vao-ebo)
  (gl-draw-elements gl-triangles 6 gl-unsigned-short 0))

(define-values (frame canvas)
  (make-window #:title "07-06 图元与索引（综合）" #:width 600 #:height 600 #:draw draw))

(define prog
  (send canvas with-gl-context
    (lambda () (build-program (gl-vertex-shader vert-src) (gl-fragment-shader frag-src)))))

;; 三个 gl-draw-arrays 图共用一个 VAO（同一份交错缓冲）
(define vao-figs
  (send canvas with-gl-context
    (lambda ()
      (define vbo (u32vector-ref (gl-gen-buffers 1) 0))
      (gl-bind-buffer gl-array-buffer vbo)
      (gl-buffer-data gl-array-buffer (gl-vector-sizeof figs) figs gl-static-draw)
      (define v (u32vector-ref (gl-gen-vertex-arrays 1) 0))
      (gl-bind-vertex-array v)
      (gl-vertex-attrib-pointer 0 (glsl-size 'vec2) gl-float #f (glsl-stride-bytes 'vec2 'vec3) 0)
      (gl-enable-vertex-attrib-array 0)
      (gl-vertex-attrib-pointer 1 (glsl-size 'vec3) gl-float #f
                                (glsl-stride-bytes 'vec2 'vec3)
                                (glsl-stride-bytes 'vec2))
      (gl-enable-vertex-attrib-array 1)
      (gl-bind-vertex-array 0)
      v)))

;; EBO 四边形：单独的 VAO + 索引缓冲（VAO 记住 EBO）
(define vao-ebo
  (send canvas with-gl-context
    (lambda ()
      (define vbo (u32vector-ref (gl-gen-buffers 1) 0))
      (gl-bind-buffer gl-array-buffer vbo)
      (gl-buffer-data gl-array-buffer (gl-vector-sizeof everts) everts gl-static-draw)
      (define v (u32vector-ref (gl-gen-vertex-arrays 1) 0))
      (gl-bind-vertex-array v)
      (gl-vertex-attrib-pointer 0 (glsl-size 'vec2) gl-float #f (glsl-stride-bytes 'vec2 'vec3) 0)
      (gl-enable-vertex-attrib-array 0)
      (gl-vertex-attrib-pointer 1 (glsl-size 'vec3) gl-float #f
                                (glsl-stride-bytes 'vec2 'vec3)
                                (glsl-stride-bytes 'vec2))
      (gl-enable-vertex-attrib-array 1)
      (define ebo (u32vector-ref (gl-gen-buffers 1) 0))
      (gl-bind-buffer gl-element-array-buffer ebo)
      (gl-buffer-data gl-element-array-buffer (gl-vector-sizeof eidx) eidx gl-static-draw)
      (gl-bind-vertex-array 0)
      v)))

(send frame show #t)
