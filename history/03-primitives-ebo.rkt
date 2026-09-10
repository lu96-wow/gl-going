#lang racket/base
;; =========================================================
;; 03-primitives-ebo.rkt —— 图元类型 + 索引缓冲（EBO）
;; 运行：racket 03-primitives-ebo.rkt    ESC/点X = 退出
;; =========================================================
;; 上一课(02)（GLSL 语言）：01 管数据流，02 管着色器语言，glDrawArrays 画了一个三角形。
;; 本课把"画什么形状"讲透，新 API 只有两个：
;;
;;   ① glDrawArrays(GL_xxx, first, count)
;;        —— 第一个参数是"图元类型"：GL_POINTS / GL_LINES / GL_LINE_STRIP
;;           / GL_LINE_LOOP / GL_TRIANGLES / GL_TRIANGLE_STRIP / GL_TRIANGLE_FAN
;;           同一份缓冲可以从第 first 个顶点画 count 个顶点，一行代码一种拓扑
;;
;;   ② glDrawElements + EBO（索引缓冲）
;;        —— 问题：画一个矩形(两个三角形)要 6 个顶点，但角只有 4 个，
;;           两个三角形共享一条对角线上的两个角。重复上传统计是浪费。
;;        —— EBO 存"角的编号"，让 GPU 按编号取顶点：4 个顶点 + 6 个索引
;;
;; 图形原理：图元类型决定"顶点流如何连成形状"——
;;   POINTS 每个顶点一个点；LINES 每两个一组画线段；LINE_STRIP 顺次连；
;;   TRIANGLES 每三个一组独立三角形；TRIANGLE_STRIP 后一个顶点与前面两个
;;   组成下一个三角形（省顶点，常用于条带）；EBO 则是"编号取点"代替"重复
;;   存点"，数据更小，GPU 还能缓存最近用过的顶点。
;;
;; 演示（600×600 窗口，NDC 坐标）：
;;   顶部一横排点(GL_POINTS) → 中间折线(GL_LINE_STRIP) →
;;   右下一个四边形(GL_TRIANGLE_STRIP,4 顶点) ↔ 左下一个同款四边形
;;   (EBO,4 顶点+6 索引)——两个四边形视觉一模一样，区别只在数据组织。
;; =========================================================

(require racket/gui opengl)
(require "lib.rkt")

;; 顶点着色器：gl_PointSize 只有画点（GL_POINTS）时才有意义，
;; 它决定点的大小（像素），画线/三角形时被忽略。
(define vert-src
  (glsl-pretty
   (glsl
    (version 330 core)
    (layout (location 0) in vec2 aPos)
    (layout (location 1) in vec3 aColor)
    (out vec3 vColor)
    (define (main) void
      (set! vColor aColor)
      (set! gl_PointSize 14.0)
      (set! gl_Position (vec4 aPos 0.0 1.0))))))

(define frag-src
  (glsl-pretty
   (glsl
    (version 330 core)
    (in vec3 vColor)
    (out vec4 FragColor)
    (define (main) void
      (set! FragColor (vec4 vColor 1.0))))))

(define cfg (new gl-config%))
(send cfg set-legacy? #f)
(send cfg set-double-buffered #t)
(define frame
  (new (class frame%
         (augment* [on-close (lambda () (exit 0))])
         (super-new))
       (label "03 图元与索引") (width 600) (height 600)))

(define init? (box #f))
(define prog #f)
(define vao-figs 0) (define vao-ebo 0)
;; 三种 glDrawArrays 图元的 (类型, 起始顶点, 顶点数)
(define figure-info '())

(define canvas
  (new (class canvas%
         (inherit with-gl-context swap-gl-buffers)
         (define/override (on-size w h)
           (with-gl-context
            (lambda ()
              (define-values (fw fh) (send this get-gl-client-size))
              (glViewport 0 0 fw fh)
              (glClearColor 0.07 0.08 0.14 1.0))))
         (define/override (on-char e)
           (when (eq? (send e get-key-code) 'escape) (exit 0)))
         (define/override (on-paint)
           (with-gl-context
            (lambda ()
              (unless (unbox init?)
                (set-box! init? #t)
                (set! prog (build-program vert-src frag-src))

                ;; ============ 数据：一 个缓冲装三幅图 ============
                ;; 每顶点 5 float = 位置 vec2 + 颜色 vec3
                (define (v x y r g b) (list x y r g b))
                (define pts      ; ① 5 个点（一横排）
                  (list (v -0.8 0.70 1.0 0.3 0.3) (v -0.4 0.70 0.9 0.5 0.2)
                        (v  0.0 0.70 0.9 0.9 0.2) (v  0.4 0.70 0.3 0.9 0.4)
                        (v  0.8 0.70 0.3 0.6 1.0)))
                (define zig      ; ② 折线（波浪）7 个点顺次连
                  (list (v -0.90 -0.15 0.3 0.8 1.0) (v -0.60  0.35 0.3 0.8 1.0)
                        (v -0.30 -0.15 0.3 0.8 1.0) (v  0.00  0.35 0.3 0.8 1.0)
                        (v  0.30 -0.15 0.3 0.8 1.0) (v  0.60  0.35 0.3 0.8 1.0)
                        (v  0.90 -0.15 0.3 0.8 1.0)))
                (define strip    ; ③ 四边形条带：4 顶点→2 三角形
                  (list (v  0.35 -0.75 0.3 0.9 0.5) (v 0.35 -0.30 0.9 0.9 0.3)
                        (v  0.95 -0.75 0.5 0.3 1.0) (v 0.95 -0.30 1.0 0.4 0.3)))
                ;; 拼成一个缓冲，同时记下每幅图的 (类型 起点 数量)
                (define all (append pts zig strip))
                (define flat (apply append all))
                (set! figure-info
                      (list (list GL_POINTS       0 (length pts))
                            (list GL_LINE_STRIP   (length pts) (length zig))
                            (list GL_TRIANGLE_STRIP (+ (length pts) (length zig))
                                  (length strip))))
                (define vao-a (u32vector-ref (glGenVertexArrays 1) 0))
                (glBindVertexArray vao-a)
                (define vbo (u32vector-ref (glGenBuffers 1) 0))
                (glBindBuffer GL_ARRAY_BUFFER vbo)
                (define fverts (apply f32vector flat))
                (glBufferData GL_ARRAY_BUFFER (gl-vector-sizeof fverts) fverts GL_STATIC_DRAW)
                (define stride (* 5 4))
                (glVertexAttribPointer 0 2 GL_FLOAT #f stride 0)
                (glEnableVertexAttribArray 0)
                (glVertexAttribPointer 1 3 GL_FLOAT #f stride (* 2 4))
                (glEnableVertexAttribArray 1)
                (set! vao-figs vao-a)

                ;; ============ EBO：同款四边形，但存"索引" ============
                ;; 4 个角（左下、右下、右上、左上），颜色不同更易辨
                (define corners
                  (list (v -0.95 -0.75 1.0 0.5 0.2) (v -0.40 -0.75 0.9 0.9 0.2)
                        (v -0.40 -0.30 0.2 0.9 0.4) (v -0.95 -0.30 0.3 0.5 1.0)))
                (define everts (apply f32vector (apply append corners)))
                (define idx (u16vector 0 1 2  0 2 3))   ; 两个三角形共 4 个角
                (define vao-e (u32vector-ref (glGenVertexArrays 1) 0))
                (glBindVertexArray vao-e)
                (define ebo-v (u32vector-ref (glGenBuffers 1) 0))
                (glBindBuffer GL_ARRAY_BUFFER ebo-v)
                (glBufferData GL_ARRAY_BUFFER (gl-vector-sizeof everts) everts GL_STATIC_DRAW)
                (glVertexAttribPointer 0 2 GL_FLOAT #f stride 0)
                (glEnableVertexAttribArray 0)
                (glVertexAttribPointer 1 3 GL_FLOAT #f stride (* 2 4))
                (glEnableVertexAttribArray 1)
                (define ebo (u32vector-ref (glGenBuffers 1) 0))
                (glBindBuffer GL_ELEMENT_ARRAY_BUFFER ebo)   ; ★EBO 绑定在 VAO 上
                (glBufferData GL_ELEMENT_ARRAY_BUFFER (gl-vector-sizeof idx) idx GL_STATIC_DRAW)
                (glBindVertexArray 0)
                (set! vao-ebo vao-e))

              (glClear GL_COLOR_BUFFER_BIT)
              (glUseProgram prog)
              ;; 三幅 glDrawArrays 图（同一缓冲，各画各的拓扑）
              (glBindVertexArray vao-figs)
              (for ([fi figure-info])
                (glDrawArrays (list-ref fi 0) (list-ref fi 1) (list-ref fi 2)))
              ;; EBO 四边形：glDrawElements 按索引取顶点
              (glBindVertexArray vao-ebo)
              (glDrawElements GL_TRIANGLES 6 GL_UNSIGNED_SHORT 0)
              (send this swap-gl-buffers))))
         (super-new))
       (style '(gl no-autoclear))
       (gl-config cfg)
       (parent frame)))

(define ticker
  (new timer% (interval 16)
       (notify-callback
        (lambda () (send canvas refresh)))))
(send frame show #t)
(send canvas focus)
