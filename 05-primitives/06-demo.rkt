#lang racket/base
;; =========================================================
;; 05-primitives/06-demo.rkt —— 第六步：综合，一个窗口看全图元 + 索引
;; 运行：racket 05-primitives/06-demo.rkt    点 X = 退出
;; =========================================================
;; 本课前五步分别讲了：顶点颜色(01)、点(02)、线(03)、三角形条带(04)、
;; EBO 索引(05)。本步**不引入任何新语法**，把它们放进同一个窗口对比着看。
;;
;; 画面布局（600×600，NDC 坐标）：
;;   顶部   ：一横排点      GL_POINTS        （02 步）
;;   中部   ：波浪折线      GL_LINE_STRIP    （03 步）
;;   右下   ：四边形条带    GL_TRIANGLE_STRIP（04 步，4 顶点画 2 三角形）
;;   左下   ：同款四边形    EBO + glDrawElements（05 步，4 顶点 + 6 索引）
;;
;; ★核心对比：右下和左下两个四边形**长得一模一样**，区别只在数据组织——
;;   右下 = 4 个顶点按 STRIP 顺序直接画；左下 = 4 个角 + 6 个编号（索引）。
;;   这就是"图元类型"和"索引"两种省顶点方式同台亮相。
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
  (list (list GL_POINTS       0 (length pts))
        (list GL_LINE_STRIP   (length pts) (length zig))
        (list GL_TRIANGLE_STRIP (+ (length pts) (length zig)) (length strip))))

;; ④ 左下：同款四边形，但用"4 角 + 6 索引"（EBO）
(define corners
  (append (v -0.95 -0.75 1.0 0.5 0.2) (v -0.40 -0.75 0.9 0.9 0.2)
          (v -0.40 -0.30 0.2 0.9 0.4) (v -0.95 -0.30 0.3 0.5 1.0)))
(define everts (apply concat-vecs corners))
(define eidx (u16vector 0 1 2  0 2 3))

(define (draw)
  (glClearColor 0.07 0.08 0.14 1.0)
  (glClear GL_COLOR_BUFFER_BIT)
  (glUseProgram prog)
  ;; 三个 glDrawArrays 图（同一块缓冲，各画各的拓扑）
  (glBindVertexArray vao-figs)
  (for ([fi figure-info])
    (glDrawArrays (list-ref fi 0) (list-ref fi 1) (list-ref fi 2)))
  ;; EBO 四边形：按索引取顶点
  (glBindVertexArray vao-ebo)
  (glDrawElements GL_TRIANGLES 6 GL_UNSIGNED_SHORT 0))

(define-values (frame canvas)
  (make-window #:title "05-06 图元与索引（综合）" #:width 600 #:height 600 #:draw draw))

(define prog (send canvas with-gl-context (lambda () (build-program vert-src frag-src))))

;; 三个 glDrawArrays 图共用一个 VAO（同一份交错缓冲）
(define vao-figs
  (send canvas with-gl-context
        (lambda ()
          (define vbo (u32vector-ref (glGenBuffers 1) 0))
          (glBindBuffer GL_ARRAY_BUFFER vbo)
          (glBufferData GL_ARRAY_BUFFER (gl-vector-sizeof figs) figs GL_STATIC_DRAW)
          (define v (u32vector-ref (glGenVertexArrays 1) 0))
          (glBindVertexArray v)
          (glVertexAttribPointer 0 2 GL_FLOAT #f 20 0)
          (glEnableVertexAttribArray 0)
          (glVertexAttribPointer 1 3 GL_FLOAT #f 20 8)
          (glEnableVertexAttribArray 1)
          (glBindVertexArray 0)
          v)))

;; EBO 四边形：单独的 VAO + 索引缓冲（VAO 记住 EBO）
(define vao-ebo
  (send canvas with-gl-context
        (lambda ()
          (define vbo (u32vector-ref (glGenBuffers 1) 0))
          (glBindBuffer GL_ARRAY_BUFFER vbo)
          (glBufferData GL_ARRAY_BUFFER (gl-vector-sizeof everts) everts GL_STATIC_DRAW)
          (define v (u32vector-ref (glGenVertexArrays 1) 0))
          (glBindVertexArray v)
          (glVertexAttribPointer 0 2 GL_FLOAT #f 20 0)
          (glEnableVertexAttribArray 0)
          (glVertexAttribPointer 1 3 GL_FLOAT #f 20 8)
          (glEnableVertexAttribArray 1)
          (define ebo (u32vector-ref (glGenBuffers 1) 0))
          (glBindBuffer GL_ELEMENT_ARRAY_BUFFER ebo)
          (glBufferData GL_ELEMENT_ARRAY_BUFFER (gl-vector-sizeof eidx) eidx GL_STATIC_DRAW)
          (glBindVertexArray 0)
          v)))

(send frame show #t)
