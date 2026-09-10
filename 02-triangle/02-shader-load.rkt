#lang racket/base
;; =========================================================
;; 02-triangle/02-shader-load.rkt —— 第二步：把 GLSL 文本变成程序（编译链接）
;; 运行：racket 02-triangle/02-shader-load.rkt
;; =========================================================
;; 上一步：读懂了着色器文本。本步把它变成 GPU 能跑的程序。
;; 本步新增（7 个 gl* 调用，同属"把 GLSL 变成程序"这一件事）：
;;   glCreateShader / glShaderSource / glCompileShader —— 编译每一段着色器
;;   glCreateProgram / glAttachShader / glLinkProgram    —— 链接成程序
;;   glUseProgram —— 把程序设为"当前要用的程序"
;;
;; ★概念：GLSL 文本要经过两步才能被 GPU 执行——
;;   编译：每段文本 → "着色器对象"（GPU 侧的中间产物）
;;   链接：顶点 + 片元两个着色器对象 → 一个"程序"（两段一起才能跑完整个管线）
;;   glUseProgram 之后，GPU 就知道"接下来用这个程序画"。
;;
;; ★注意本步的新结构：因为 gl* 调用要在 GL 上下文里，而上下文挂在 canvas 上，
;;   所以先 make-window 拿到 canvas，再用 (send canvas with-gl-context ...)
;;   在上下文里做初始化——这是 02 课起每课的固定套路。
;; =========================================================

(require "lib-gui.rkt")
(require "../racket-glsl/rewrite.rkt")
(require ffi/vector)   ; s32vector（glShaderSource 的签名要求它）

;; 着色器源码（同 01 步，这里只列代码）
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

;; 编译：一段 GLSL 文本 → 一个着色器对象
(define (compile-shader type src)
  (define shader (glCreateShader type))
  ;; glShaderSource 喂源码（绑定要求传 字符串向量 + 每段长度，所以绕一下）
  (glShaderSource shader 1 (vector src) (s32vector (string-length src)))
  (glCompileShader shader)
  shader)

;; 链接：若干着色器对象 → 一个程序
(define (link-program . shaders)
  (define prog (glCreateProgram))
  (for-each (lambda (s) (glAttachShader prog s)) shaders)
  (glLinkProgram prog)
  prog)

;; 每帧：还是清屏（本步只编译，还没数据可画）
(define (draw)
  (glClearColor 0.10 0.12 0.20 1.0)
  (glClear GL_COLOR_BUFFER_BIT))

(define-values (frame canvas)
  (make-window #:title "02-02 编译链接" #:width 400 #:height 300 #:draw draw))

;; ★初始化：在画布的上下文里编译链接，设为当前程序
(send canvas with-gl-context
      (lambda ()
        (define vs (compile-shader GL_VERTEX_SHADER vert-src))
        (define fs (compile-shader GL_FRAGMENT_SHADER frag-src))
        (define prog (link-program vs fs))
        (glUseProgram prog)))          ; 设为当前程序（状态机会记住）
(printf "着色器程序编译链接成功\n")

(send frame show #t)
