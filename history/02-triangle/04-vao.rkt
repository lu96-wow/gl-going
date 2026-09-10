#lang racket/base
;; =========================================================
;; 02-triangle/04-vao.rkt —— 第四步：VAO 描述"字节的含义"
;; 运行：racket 02-triangle/04-vao.rkt
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
;;   解读 VBO。槽位号（location）在 shader 里声明 layout(location=0)，在
;;   Racket 侧用 glVertexAttribPointer(0, ...) 对齐——两边对不上就错位/黑屏。
;;
;;   stride/offset 是字节数学：本课每顶点只有 2 个 float = 8 字节，
;;   所以 stride=8、offset=0（只有一份属性，不用跳）。
;; =========================================================

(require "../lib-gui.rkt")
(require "../lib.rkt")

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

(define vao 0)   ; #:init 里 set!，#:draw 里用，所以放顶层
(run-gl
 #:title "02-04 VAO" #:width 400 #:height 300
 #:init (lambda ()
          (define prog (build-program vert-src frag-src))
          (glUseProgram prog)
          ;; VBO：上传（同 03 步）
          (define verts (f32vector -0.5 -0.5  0.5 -0.5  0.0 0.5))
          (define vbo (u32vector-ref (glGenBuffers 1) 0))
          (glBindBuffer GL_ARRAY_BUFFER vbo)
          (glBufferData GL_ARRAY_BUFFER (gl-vector-sizeof verts) verts GL_STATIC_DRAW)
          ;; ---- VAO：描述这份数据的字节含义 ----
          (set! vao (u32vector-ref (glGenVertexArrays 1) 0))
          (glBindVertexArray vao)          ; 之后这些设置都记在 vao 上
          ;; 参数：槽位号 0、每属性 2 个 float、float 类型、
          ;;       不归一化、步长 8 字节、起点偏移 0
          (glVertexAttribPointer 0 2 GL_FLOAT #f 8 0)
          (glEnableVertexAttribArray 0)
          (glBindVertexArray 0)            ; 解绑收好（设置已记录）
          (printf "VAO ~a 已记录：location 0 = 每顶点 2 个 float（位置）\n" vao))
 #:draw (lambda ()
          (glClearColor 0.10 0.12 0.20 1.0)
          (glClear GL_COLOR_BUFFER_BIT)
          ;; 程序、数据、说明书都齐了——只差最后一句"画"。
          ))
