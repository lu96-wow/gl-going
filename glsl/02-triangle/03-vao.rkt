#lang racket/base
;; =========================================================
;; 02-triangle/03-vao.rkt —— 第三步：VAO 描述"字节的含义"
;; 运行：racket glsl/02-triangle/03-vao.rkt
;; =========================================================
;; 上一步：数据在显存里，但只是一堆字节，GPU 不知道每段是什么。
;; 本步新增（4 个，同属"描述数据含义"这一件事）：
;;   glGenVertexArrays      —— 生成 VAO 对象
;;   glBindVertexArray      —— 绑定 VAO（之后的所有设置都记在它上面）
;;   glVertexAttribPointer  —— 声明"第 N 号槽的数据在缓冲里怎么取"
;;   glEnableVertexAttribArray —— 启用第 N 号槽
;;
;; ★VAO 解决什么问题：VBO 只是字节流。VAO 是"字节说明书"，把每个 attribute
;;   （槽位）的 类型/起点/步长 记录下来；画的时候绑上 VAO，GPU 就知道怎么
;;   解读 VBO。还记得 01 步 shader 里的 layout(location=0) 吗？那个 0 就是
;;   槽位号——这里 glVertexAttribPointer(0, ...) 用同一个数字对齐，两边
;;   对不上画面就错位/黑屏。
;;
;;   stride/offset 是字节数学：本课每顶点只有 2 个 float = 8 字节，
;;   所以 stride=8、offset=0（只有一份属性，不用跳）。
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

(define verts (vec (vec2 -0.5 -0.5) (vec2 0.5 -0.5) (vec2 0.0 0.5)))

(define (draw)
  (glClearColor 0.10 0.12 0.20 1.0)
  (glClear GL_COLOR_BUFFER_BIT))

(define-values (frame canvas)
  (make-window #:title "02-03 VAO" #:width 400 #:height 300 #:draw draw))

;; VBO 上传 + VAO 说明书，一起在上下文里做完，返回 vao 供 draw 用。
;; build-program 来自 lib.rkt（黑盒）：把两段着色器编译链接成一个程序，03 课讲内部。
(define vao
  (send canvas with-gl-context
        (lambda ()
          (define prog (build-program (GL_VERTEX_SHADER vert-src) (GL_FRAGMENT_SHADER frag-src)))
          (glUseProgram prog)
          (define vbo (u32vector-ref (glGenBuffers 1) 0))
          (glBindBuffer GL_ARRAY_BUFFER vbo)
          (glBufferData GL_ARRAY_BUFFER (gl-vector-sizeof (vec->f32vector verts)) (vec->f32vector verts) GL_STATIC_DRAW)
          (define v (u32vector-ref (glGenVertexArrays 1) 0))
          (glBindVertexArray v)          ; 之后这些设置都记在 v 上
          ;; 参数：槽位号 0、每属性 2 个 float、float 类型、
          ;;       不归一化、步长 8 字节、起点偏移 0
          (glVertexAttribPointer 0 2 GL_FLOAT #f 8 0)
          (glEnableVertexAttribArray 0)
          (glBindVertexArray 0)          ; 解绑收好（设置已记录）
          v)))
(printf "VAO 已记录：location 0 = 每顶点 2 个 float（位置）\n")

(send frame show #t)
;; 程序、数据、说明书都齐了——只差最后一句"画"。
