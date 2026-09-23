#lang racket/base
;; =========================================================
;; 07-primitives/01-color.rkt —— 第一步：顶点颜色（aColor）
;; 运行：racket 07-primitives/01-color.rkt    点 X = 退出
;; =========================================================

;; 画图元之前先解决一个问题：怎么**看清每个顶点**。位置属性（aPos）只能让
;; 顶点落在某个坐标，看不出"谁是谁"。所以本步给每个顶点加一个**颜色**属性。
;;
;; 本步新增（1 个）：
;;   aColor —— 每个顶点一份的 vec3 颜色（第二个 attribute，3 个分量）
;;
;; ★复习与推广（05 课讲过双属性，这里只是分量数不同）：
;;   05 课的第二个属性 aUV 是 vec2（2 个 float）。本步 aColor 是 vec3
;;   （3 个 float：红绿蓝）。于是每个顶点 = 位置 2 float + 颜色 3 float
;;   = 5 个 float，交错存在一块缓冲里。
;;
;; ★从本步起，stride/offset 不再手写字节数，改用工具算：
;;     glsl-size        —— 一个类型的元素数（vec3 → 3，传给 gl-vertex-attrib-pointer 的 size）
;;     glsl-stride-bytes —— 若干类型交错后的字节步长（'vec2 'vec3 → 20）
;;                         offset 也用它：第二属性 offset = 跳过前面的类型
;;   （02/05 手写 8/16 是为了理解字节布局；现在收成工具，不易错、易读。）
;;
;; ★顶点数据怎么拼：一个顶点 = 一个 vec2 + 一个 vec3，宽度不一样，不能用
;;   (vec ...)（它要求同宽）。改用 concat-vecs：把一串 f32vector 依次连成
;;   一个连续缓冲——正是给"混合宽度的交错属性"用的。
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
          (set! gl_Position (vec4 aPos 0.0 1.0)))))

(define frag-src
  (glsl (version 330 core)
        (in vec3 vColor)
        (out vec4 FragColor)
        (define (main) void
          (set! FragColor (vec4 vColor 1.0)))))

;; 3 个顶点，每个 = (vec2 位置)(vec3 颜色)，红/绿/蓝三个角。
;; concat-vecs 把它们连成 [x y r g b, x y r g b, x y r g b] 共 15 个 float。
(define verts
  (concat-vecs (vec2 -0.5 -0.5) (vec3 1.0 0.2 0.2)   ; 左下 红
               (vec2  0.5 -0.5) (vec3 0.2 1.0 0.2)   ; 右下 绿
               (vec2  0.0  0.5) (vec3 0.2 0.2 1.0))) ; 上   蓝

(define (draw)
  (gl-clear-color 0.10 0.12 0.20 1.0)
  (gl-clear gl-color-buffer-bit)
  (use-program prog)
  (gl-bind-vertex-array vao)
  (gl-draw-arrays gl-triangles 0 3))

(define-values (frame canvas)
  (make-window #:title "07-01 顶点颜色" #:width 400 #:height 400 #:draw draw))

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
      ;; 位置：2 个 float，步长 20，起点 0
      (gl-vertex-attrib-pointer 0 (glsl-size 'vec2) gl-float #f (glsl-stride-bytes 'vec2 'vec3) 0)
      (gl-enable-vertex-attrib-array 0)
      ;; 颜色：3 个 float，步长 20，起点 8（跳过位置）
      (gl-vertex-attrib-pointer 1 (glsl-size 'vec3) gl-float #f
                                (glsl-stride-bytes 'vec2 'vec3)
                                (glsl-stride-bytes 'vec2))
      (gl-enable-vertex-attrib-array 1)
      (gl-bind-vertex-array 0)
      v)))

(send frame show #t)
;; 三个角红绿蓝，中间是 02 课讲过的插值——现在你能"看见"重心坐标在混颜色。
