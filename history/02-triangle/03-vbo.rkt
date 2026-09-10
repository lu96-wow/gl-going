#lang racket/base
;; =========================================================
;; 02-triangle/03-vbo.rkt —— 第三步：把顶点数据搬进显存（VBO）
;; 运行：racket 02-triangle/03-vbo.rkt
;; =========================================================
;; 上一步：程序编译好了，但没有数据可画。本步准备数据。
;; 本步新增（4 个，同属"上传数据"这一件事）：
;;   glGenBuffers     —— 生成一个缓冲对象，拿到它的编号
;;   glBindBuffer     —— 把这个缓冲设为"当前操作对象"（状态机）
;;   glBufferData     —— 把数据真正拷进 GPU 显存
;;   gl-vector-sizeof —— 算一个 ffi 向量占多少字节（opengl 提供的小工具）
;;
;; ★为什么这样设计：老 API 画三角形是 CPU 每顶点调一次函数，CPU 和 GPU
;;   之间一趟趟通信——慢。现代做法 = 把整块顶点数据"一次批量上传"到显存
;;   （VBO = GPU 显存里的一块缓冲），之后 GPU 自己读，不再打扰 CPU。
;;   GL_ARRAY_BUFFER 表示"这块缓冲存的是顶点属性数组"；GL_STATIC_DRAW
;;   表示"数据基本不变"——GPU 好据此选最合适的显存位置。
;;
;;   本课数据：3 个顶点 × 每个 2 个 float（位置），共 6 个 float = 24 字节。
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

(run-gl
 #:title "02-03 VBO" #:width 400 #:height 300
 #:init (lambda ()
          (define prog (build-program vert-src frag-src))
          (glUseProgram prog)
          ;; ---- 3 个顶点：[x y] ×3（NDC 坐标，-1..1）----
          (define verts (f32vector -0.5 -0.5
                                   0.5 -0.5
                                   0.0  0.5))
          ;; ---- VBO：数据搬进 GPU 显存 ----
          (define vbo (u32vector-ref (glGenBuffers 1) 0)) ; 生成 1 个缓冲，取回编号
          (glBindBuffer GL_ARRAY_BUFFER vbo)              ; 设为当前顶点缓冲
          (glBufferData GL_ARRAY_BUFFER (gl-vector-sizeof verts) verts GL_STATIC_DRAW)
          (printf "已上传 ~a 个 float（~a 字节）到 VBO ~a\n"
                  (f32vector-length verts) (gl-vector-sizeof verts) vbo))
 #:draw (lambda ()
          (glClearColor 0.10 0.12 0.20 1.0)
          (glClear GL_COLOR_BUFFER_BIT)
          ;; 数据在显存里了，但 GPU 还不知道"这些字节是什么含义"——
          ;; 所以画面仍是清屏色，下一步（VAO）来当说明书。
          ))
