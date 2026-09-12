#lang racket/base
;; =========================================================
;; 03-pipeline/02-glsl-what.rkt —— 第二步：GLSL 是什么
;; 运行：racket glsl/03-pipeline/02-glsl-what.rkt
;; =========================================================
;; 上一步：管线有 7 段，其中②⑥两段可编程。本步回答"用什么写、写出来的
;; 是什么、怎么跑"。
;;
;; ★GLSL = OpenGL Shading Language：
;;   - 一种 C 风格的小语言，专门在 GPU 上跑。
;;   - 源码不是"提前编译进程序"，而是**运行时**由驱动编译（03/04 步）。
;;   - 每个顶点/每个像素独立跑同一份代码，没有全局状态、不能互相通信。
;;
;; ★GLSL 代码的骨架（下面 vert-src 逐行读）：
;;   (version 330 core)                   版本 + core profile（按 core 写，不用旧式内建变量）
;;   (layout (location 0) in vec2 aPos)   输入 attribute：每个顶点一份，
;;                                        挂在 0 号槽，类型 vec2
;;   (define (main) void ...)             入口固定叫 main，void = 无返回值
;;   (set! gl_Position (vec4 aPos 0.0 1.0))
;;     gl_Position = 内置变量，顶点着色器必须写它（顶点落点，裁剪坐标）
;;
;;   frag-src 逐行读：
;;   (out vec4 FragColor)                 输出：这个像素的颜色
;;   (set! FragColor (vec4 r g b a))      红 绿 蓝 不透明度（1.0=不透明）
;;
;; ★变量分两种（后面每课都用）：
;;   ① 内置变量：GLSL 规定死的名字（gl_Position），不能改名。
;;   ② 用户自定义：in / out / uniform。
;;      in      = 每个顶点/片元一份、从上一段流过来的数据
;;      out     = 传给下一段的数据（顶点 out 会在光栅化时被自动插值）
;;      uniform = CPU 每帧传给 GPU 的"全局常量"（本步没用，05 课讲）
;;
;; ★两段怎么串起来：顶点着色器算出 gl_Position → GPU 光栅化铺成像素 →
;;   片元着色器给每个像素上色。数据从②流到⑥，中间③④⑤你不用管。
;;
;; ★写 GLSL 的姿势：本项目用 (glsl ...) 宏把 S 表达式展开成 GLSL 文本——
;;   Racket 里的 vec2/vec4 和 GLSL 里的 vec2/vec4 名字一一对应，方便对齐。
;; =========================================================

(require "lib-gui.rkt")
(require "lib.rkt")

(define vert-src
  (glsl (version 330 core)
        (layout (location 0) in vec2 aPos)
        (define (main) void
          (set! gl_Position (vec4 aPos 0.0 1.0)))))
(define frag-src
  (glsl (version 330 core)
        (out vec4 FragColor)
        (define (main) void
          (set! FragColor (vec4 1.0 0.35 0.2 1.0)))))

;; 展开出的真实 GLSL——S 表达式和 GLSL 一一对应，照这个读。
(printf "顶点着色器展开为：\n~a\n\n片元着色器展开为：\n~a\n"
        (glsl-pretty vert-src) (glsl-pretty frag-src))

(define verts (vec (vec2 -0.5 -0.5) (vec2 0.5 -0.5) (vec2 0.0 0.5)))

(define (draw)
  (glClearColor 0.10 0.12 0.20 1.0)
  (glClear GL_COLOR_BUFFER_BIT)
  (glUseProgram prog)
  (glBindVertexArray vao)
  (glDrawArrays GL_TRIANGLES 0 3))

(define-values (frame canvas)
  (make-window #:title "03-02 GLSL 是什么" #:width 400 #:height 300 #:draw draw))

(define prog (send canvas with-gl-context
                    (lambda ()
                      (build-program (GL_VERTEX_SHADER vert-src)
                                     (GL_FRAGMENT_SHADER frag-src)))))
(define vao
  (send canvas with-gl-context
        (lambda ()
          (define vbo (u32vector-ref (glGenBuffers 1) 0))
          (glBindBuffer GL_ARRAY_BUFFER vbo)
          (glBufferData GL_ARRAY_BUFFER (gl-vector-sizeof (vec->f32vector verts))
                        (vec->f32vector verts) GL_STATIC_DRAW)
          (define v (u32vector-ref (glGenVertexArrays 1) 0))
          (glBindVertexArray v)
          (glVertexAttribPointer 0 2 GL_FLOAT #f 8 0)
          (glEnableVertexAttribArray 0)
          (glBindVertexArray 0)
          v)))

(send frame show #t)
