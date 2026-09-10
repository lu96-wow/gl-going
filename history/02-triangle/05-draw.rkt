#lang racket/base
;; =========================================================
;; 02-triangle/05-draw.rkt —— 第五步：画出来！三角形出现
;; 运行：racket 02-triangle/05-draw.rkt    点 X = 退出
;; =========================================================
;; 上一步：程序、数据、说明书都齐了。本步补上最后一句"画"。
;; 本步新增（2 个）：
;;   glDrawArrays —— 真正把顶点画出来（装配 → 光栅化 → 片元着色）
;;   out/in 对接  —— 顶点着色器算出的值交给片元着色器（GPU 自动插值）
;;
;; ★插值的数学（为什么三角形中间是渐变色）：片元着色器收到的 vPos 不是某个
;;   顶点的原值，而是"这个像素在三角形内的重心坐标"加权出的值——三角形内
;;   任一点都能写成三顶点的凸组合  p = αA + βB + γC（α+β+γ=1），GPU 就用
;;   同一组 α,β,γ 去混合顶点传下来的任何量。于是三个顶点位置不同，中间的
;;   颜色连续过渡。这是 GPU 光栅化最核心的一条数学。
;; =========================================================

(require "../lib-gui.rkt")
(require "../lib.rkt")

;; 顶点着色器：多了一行 (out vec2 vPos)，把位置传给片元。
(define vert-src
  (glsl (version 330 core)
        (layout (location 0) in vec2 aPos)
        (out vec2 vPos)
        (define (main) void
          (set! vPos aPos)
          (set! gl_Position (vec4 aPos 0.0 1.0)))))

;; 片元着色器：收下（已插值的）vPos，用它当颜色 → 渐变。
;; vPos 范围 -1..1，*0.5+0.5 映射到 0..1 当红/绿通道。
(define frag-src
  (glsl (version 330 core)
        (in vec2 vPos)
        (out vec4 FragColor)
        (define (main) void
          (set! FragColor (vec4 (+ (* vPos 0.5) (vec2 0.5)) 0.0 1.0)))))

(define prog #f)
(define vao 0)
(run-gl
 #:title "02-05 第一个三角形" #:width 400 #:height 300
 #:init (lambda ()
          ;; 程序 + VBO + VAO（同前两步）
          (set! prog (build-program vert-src frag-src))
          (define verts (f32vector -0.5 -0.5  0.5 -0.5  0.0 0.5))
          (define vbo (u32vector-ref (glGenBuffers 1) 0))
          (glBindBuffer GL_ARRAY_BUFFER vbo)
          (glBufferData GL_ARRAY_BUFFER (gl-vector-sizeof verts) verts GL_STATIC_DRAW)
          (set! vao (u32vector-ref (glGenVertexArrays 1) 0))
          (glBindVertexArray vao)
          (glVertexAttribPointer 0 2 GL_FLOAT #f 8 0)
          (glEnableVertexAttribArray 0)
          (glBindVertexArray 0))
 #:draw (lambda ()
          (glClearColor 0.10 0.12 0.20 1.0)
          (glClear GL_COLOR_BUFFER_BIT)
          (glUseProgram prog)        ; 每帧绘制前，把程序设为当前
          (glBindVertexArray vao)    ; 绑上 VAO（拿到"数据说明书"）
          ;; GL_TRIANGLES = 每 3 个顶点一组三角形；从第 0 个顶点画 3 个
          (glDrawArrays GL_TRIANGLES 0 3)))
