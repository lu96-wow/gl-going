#lang racket/base
;; =========================================================
;; 05-primitives/01-color.rkt —— 第一步：顶点颜色（aColor）
;; 运行：racket 05-primitives/01-color.rkt    点 X = 退出
;; =========================================================
;; 画图元之前先解决一个问题：怎么**看清每个顶点**。位置属性（aPos）只能让
;; 顶点落在某个坐标，看不出"谁是谁"。所以本步给每个顶点加一个**颜色**属性。
;;
;; 本步新增（1 个）：
;;   aColor —— 每个顶点一份的 vec3 颜色（第二个 attribute，3 个分量）
;;
;; ★复习与推广（03 课讲过双属性，这里只是分量数不同）：
;;   03 课的第二个属性 aUV 是 vec2（2 个 float）。本步 aColor 是 vec3
;;   （3 个 float：红绿蓝）。于是每个顶点 = 位置 2 float + 颜色 3 float
;;   = 5 个 float，交错存在一块缓冲里。
;;
;; ★从本步起，stride/offset 不再手写字节数，改用 rename-vector 的工具算：
;;     glsl-size        —— 一个类型的元素数（vec3 → 3，传给 glVertexAttribPointer 的 size）
;;     glsl-stride-bytes —— 若干类型交错后的字节步长（'vec2 'vec3 → 20）
;;                         offset 也用它：第二属性 offset = 跳过前面的类型
;;   （02/03 手写 8/16 是为了理解字节布局；现在收成工具，不易错、易读。）
;;
;; ★顶点数据怎么拼：一个顶点 = 一个 vec2 + 一个 vec3，宽度不一样，不能用
;;   (vec ...)（它要求同宽）。改用 concat-vecs：把一串 f32vector 依次连成
;;   一个连续缓冲——正是给"混合宽度的交错属性"用的。
;; =========================================================

(require "lib-gui.rkt")
(require "lib.rkt")

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
  (glClearColor 0.07 0.08 0.14 1.0)
  (glClear GL_COLOR_BUFFER_BIT)
  (glUseProgram prog)
  (glBindVertexArray vao)
  (glDrawArrays GL_TRIANGLES 0 3))

(define-values (frame canvas)
  (make-window #:title "05-01 顶点颜色" #:width 400 #:height 400 #:draw draw))

(define prog (send canvas with-gl-context (lambda () (build-program vert-src frag-src))))
(define vao
  (send canvas with-gl-context
        (lambda ()
          (define vbo (u32vector-ref (glGenBuffers 1) 0))
          (glBindBuffer GL_ARRAY_BUFFER vbo)
          (glBufferData GL_ARRAY_BUFFER (gl-vector-sizeof verts) verts GL_STATIC_DRAW)
          (define v (u32vector-ref (glGenVertexArrays 1) 0))
          (glBindVertexArray v)
          (glVertexAttribPointer 0 (glsl-size 'vec2) GL_FLOAT #f (glsl-stride-bytes 'vec2 'vec3) 0)   ; 位置：2 float，步长 20，起点 0
          (glEnableVertexAttribArray 0)
          (glVertexAttribPointer 1 (glsl-size 'vec3) GL_FLOAT #f (glsl-stride-bytes 'vec2 'vec3) (glsl-stride-bytes 'vec2))   ; 颜色：3 float，步长 20，起点 8
          (glEnableVertexAttribArray 1)
          (glBindVertexArray 0)
          v)))

(send frame show #t)
;; 三个角红绿蓝，中间是 02 课讲过的插值——现在你能"看见"重心坐标在混颜色。
