#lang racket/base
;; =========================================================
;; 04-glsl-types/03-swizzle.rkt —— 第三步：swizzle 重排分量
;; 运行：racket 04-glsl-types/03-swizzle.rkt    点 X = 退出
;; =========================================================
;; 上一步：认识了类型和构造器。本步学 GLSL 最顺手的一个语法：swizzle。
;;
;; 本步新增（1 个）：
;;   swizzle —— 用 x/y/z/w（或 r/g/b/a、s/t/p/q）的组合，"点"出一个重排向量
;;
;; ★为什么需要它：向量里的分量有固定含义（颜色 = r g b、位置 = x y z）。
;;   很多时候你只想"取出"或"换个顺序"用它们。swizzle 就是为此生的捷径，
;;   不用单独写 分量→变量→再构造。
;;
;; ★语法（DSL 里把 swizzle 名写在前面，像函数一样；展开后变成 .xx 的写法）：
;;   (x c)        → c.x         取第 1 个分量（一个标量）
;;   (xy c)       → c.xy        取前两个 → 一个 vec2
;;   (yx c)       → c.yx        前两个**交换顺序** → vec2
;;   (zyx c)      → c.zyx       三个分量**倒序** → vec3
;;   分量字母只能来自 x y z w（位置）/ r g b a（颜色）/ s t p q（纹理坐标），
;;   不能乱拼；长度 1–4 任意。
;;
;; 本步视觉：上一步 c = (vUV.x, vUV.y, 0.5)，红绿梯度是"红随 x、绿随 y"。
;;   本步把前两个分量交换 (yx c)，红绿梯度就跟着**对调**：
;;   上一步右下角是红、左上角是绿；本步右下角变绿、左上角变红。
;; =========================================================

(require "lib-gui.rkt")
(require "lib.rkt")

(define vert-src
  (glsl (version 330 core)
        (layout (location 0) in vec2 aPos)
        (layout (location 1) in vec2 aUV)
        (out vec2 vUV)
        (define (main) void
          (set! vUV aUV)
          (set! gl_Position (vec4 aPos 0.0 1.0)))))

;; 片元着色器（本步主角）：
;;   (vec3 c (vec3 vUV 0.5))      c = (u, v, 0.5)
;;   (yx c)                       交换前两个分量 → (v, u)
;;   所以 FragColor = (v, u, 0.5, 1.0)：红色通道接的是 v，绿色接的是 u。
(define frag-src
  (glsl (version 330 core)
        (in vec2 vUV)
        (out vec4 FragColor)
        (define (main) void
          (vec3 c (vec3 vUV 0.5))
          (set! FragColor (vec4 (yx c) 0.5 1.0)))))

(printf "片元着色器展开为：\n~a\n" (glsl-pretty frag-src))

(define verts
  (vec (vec4 -1.0 -1.0  0.0 0.0)
       (vec4  1.0 -1.0  1.0 0.0)
       (vec4  1.0  1.0  1.0 1.0)
       (vec4 -1.0 -1.0  0.0 0.0)
       (vec4  1.0  1.0  1.0 1.0)
       (vec4 -1.0  1.0  0.0 1.0)))

(define (draw)
  (glClearColor 0.10 0.12 0.20 1.0)
  (glClear GL_COLOR_BUFFER_BIT)
  (glUseProgram prog)
  (glBindVertexArray vao)
  (glDrawArrays GL_TRIANGLES 0 6))

(define-values (frame canvas)
  (make-window #:title "04-03 swizzle" #:width 400 #:height 400 #:draw draw))

(define prog (send canvas with-gl-context (lambda () (build-program (GL_VERTEX_SHADER vert-src) (GL_FRAGMENT_SHADER frag-src)))))
(define vao
  (send canvas with-gl-context
        (lambda ()
          (define vbo (u32vector-ref (glGenBuffers 1) 0))
          (glBindBuffer GL_ARRAY_BUFFER vbo)
          (glBufferData GL_ARRAY_BUFFER (gl-vector-sizeof (vec->f32vector verts)) (vec->f32vector verts) GL_STATIC_DRAW)
          (define v (u32vector-ref (glGenVertexArrays 1) 0))
          (glBindVertexArray v)
          (glVertexAttribPointer 0 2 GL_FLOAT #f 16 0)
          (glEnableVertexAttribArray 0)
          (glVertexAttribPointer 1 2 GL_FLOAT #f 16 8)
          (glEnableVertexAttribArray 1)
          (glBindVertexArray 0)
          v)))

(send frame show #t)
