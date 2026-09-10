#lang racket/base
;; =========================================================
;; 02-triangle/04-vbo.rkt —— 第四步：把顶点数据搬进显存（VBO）
;; 运行：racket 02-triangle/04-vbo.rkt
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
;;   GL_ARRAY_BUFFER = "这块缓冲存的是顶点属性数组"；GL_STATIC_DRAW = 数据
;;   基本不变（只画不常改）。
;;
;;   本课数据：3 个顶点 × 每个 2 个 float（位置），共 6 个 float = 24 字节。
;;   注意：数据本身（f32vector）在普通 Racket 层就能造，只有"上传"要进上下文。
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

;; 顶点数据：3 个顶点，每个 = 一个 vec2 位置（shader 里 (in vec2 aPos) 说的就是它）。
;; 用 vec 包起来 = "3 个 vec2"（同宽度向量数组，对应 GLSL 的 vec2[3]）：
;;   shader 里的 vec2 是类型，Racket 的 vec2 是造 f32vector 的构造器，名字对应。
(define verts (vec (vec2 -0.5 -0.5)
                    (vec2  0.5 -0.5)
                    (vec2  0.0  0.5)))

(define (draw)
  (glClearColor 0.10 0.12 0.20 1.0)
  (glClear GL_COLOR_BUFFER_BIT))

(define-values (frame canvas)
  (make-window #:title "02-04 VBO" #:width 400 #:height 300 #:draw draw))

;; 上传：在上下文里把数据拷进显存
(define vbo
  (send canvas with-gl-context
        (lambda ()
          (define data (vec->f32vector verts))          ; vec 的底层 f32vector
          (define v (u32vector-ref (glGenBuffers 1) 0))  ; 生成 1 个缓冲，取回编号
          (glBindBuffer GL_ARRAY_BUFFER v)               ; 设为当前顶点缓冲
          (glBufferData GL_ARRAY_BUFFER (gl-vector-sizeof data) data GL_STATIC_DRAW)
          v)))
(printf "已上传 ~a 个顶点（~a 字节）到 VBO\n"
        (vec-count verts) (gl-vector-sizeof (vec->f32vector verts)))

(send frame show #t)
;; 数据在显存里了，但 GPU 还不知道"这些字节是什么含义"——
;; 所以画面仍是清屏色，下一步（VAO）来当说明书。
