#lang racket/base
;; =========================================================
;; 07-primitives/02-points.rkt —— 第二步：图元类型 + 点（gl-points）
;; 运行：racket 07-primitives/02-points.rkt    点 X = 退出
;; =========================================================

;; 上一步：顶点带颜色了。本步开始讲本课的核心——**图元类型**。
;;
;; 本步新增（2 个，同属"画点"这一件事）：
;;   gl-points    —— gl-draw-arrays 的第一个参数：每个顶点画一个点
;;   gl_PointSize —— 内置输出变量：点的大小（像素），只对点生效
;;
;; ★图元类型是什么：gl-draw-arrays(第一个参数, 起点, 数量) 的第一个参数告诉
;;   GPU"这一串顶点怎么连成形状"。同一份顶点数据，换第一个参数，形状就变。
;;   gl-points = 每个顶点一个点，互不相连。
;;
;; ★gl_PointSize：又一个"内置变量"（像 gl_Position）。它只在画点时被 GPU
;;   读取，决定点多少像素大；画线/三角形时被忽略。这里设 14.0 = 14 像素。
;;
;; 本步视觉：一横排 5 个彩色圆点，每个顶点的颜色清晰可见。
;; =========================================================

(require "../02-triangle/04-gui-tool.rkt")   ; make-window（带视口）
(require "../racket-glsl/rewrite.rkt")        ; (glsl ...) 宏
(require "../racket-glsl/rename-vector.rkt")  ; vec2/vec3/concat-vecs/glsl-size/glsl-stride-bytes
(require "../racket-glsl/tool.rkt")           ; build-program / use-program

(define vert-src
  (glsl (version 330 core)
        (layout (location 0) in vec2 aPos)
        (layout (location 1) in vec3 aColor)
        (out vec3 vColor)
        (define (main) void
          (set! vColor aColor)
          (set! gl_PointSize 14.0)        ; 点的大小（像素），只对点生效
          (set! gl_Position (vec4 aPos 0.0 1.0)))))

(define frag-src
  (glsl (version 330 core)
        (in vec3 vColor)
        (out vec4 FragColor)
        (define (main) void
          (set! FragColor (vec4 vColor 1.0)))))

;; 5 个顶点一横排（y=0，x 从 -0.8 到 0.8），颜色各不相同。
(define verts
  (concat-vecs (vec2 -0.8 0.0) (vec3 1.0 0.3 0.3)
               (vec2 -0.4 0.0) (vec3 0.9 0.5 0.2)
               (vec2  0.0 0.0) (vec3 0.9 0.9 0.2)
               (vec2  0.4 0.0) (vec3 0.3 0.9 0.4)
               (vec2  0.8 0.0) (vec3 0.3 0.6 1.0)))

(define (draw)
  (gl-clear-color 0.10 0.12 0.20 1.0)
  (gl-clear gl-color-buffer-bit)
  (use-program prog)
  (gl-bind-vertex-array vao)
  (gl-draw-arrays gl-points 0 5))    ; ★第一个参数 gl-points = 画点

(define-values (frame canvas)
  (make-window #:title "07-02 点" #:width 400 #:height 400 #:draw draw))

(define prog
  (send canvas with-gl-context
    (lambda () (build-program (gl-vertex-shader vert-src) (gl-fragment-shader frag-src)))))
(define vao
  (send canvas with-gl-context
    (lambda ()
      (define vbo (u32vector-ref (gl-gen-buffers 1) 0))
      (gl-bind-buffer gl-array-buffer vbo)
      (gl-buffer-data gl-array-buffer (gl-vector-sizeof verts) verts gl-static-draw)
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

(send frame show #t)
