#lang racket/base
;; =========================================================
;; 02-triangle/01-shader-load.rkt —— 第一步：GLSL 文本 → 着色器程序（原始过程）
;; 运行：racket 02-triangle/01-shader-load.rkt
;; =========================================================
;; 01 课学会开窗口/清屏。本步进入现代 OpenGL 的核心：可编程管线。
;; 本步新增（7 个 gl* 调用，同属"把 GLSL 变成程序"这一件事）：
;;   glCreateShader / glShaderSource / glCompileShader —— 编译每一段着色器
;;   glCreateProgram / glAttachShader / glLinkProgram    —— 链接成程序
;;   glUseProgram —— 把程序设为"当前要用的程序"
;;
;; ★为什么必须自己写着色器：core profile 没有固定管线，GPU 不再内置
;;   "怎么把三角形画出来"。它只认你交给它的两个小程序：
;;     顶点着色器 = 每个顶点跑一次，算"这个顶点落在哪"（gl_Position）
;;     片元着色器 = 每个像素跑一次，算"这个像素是什么颜色"
;;   成千上万个并行单元同时跑这份代码——所以 GLSL 被设计成无全局状态、
;;   一次只处理一个顶点/片元的小函数。
;; =========================================================

(require "../lib-gui.rkt")   ; run-gl + gl*
(require "../lib.rkt")       ; (glsl ...) 宏、s32vector 等（由 ffi/vector 转发）

;; 顶点着色器（S 表达式，展开成 GLSL 文本，逐条对照见 SYNTAX.md）
(define vert-src
  (glsl (version 330 core)
        (layout (location 0) in vec2 aPos)
        (define (main) void
          (set! gl_Position (vec4 aPos 0.0 1.0)))))

;; 片元着色器：所有像素一个颜色（橙）
(define frag-src
  (glsl (version 330 core)
        (out vec4 FragColor)
        (define (main) void
          (set! FragColor (vec4 1.0 0.35 0.2 1.0)))))

;; 打印展开出的真实 GLSL
(printf "顶点着色器展开为：\n~a\n\n" (glsl-pretty vert-src))
(printf "片元着色器展开为：\n~a\n" (glsl-pretty frag-src))

;; 把一段 GLSL 文本编译成一个"着色器对象"（在 GPU 侧）
(define (compile-shader type src)
  (define shader (glCreateShader type))
  ;; glShaderSource 喂源码：这个绑定要求 字符串向量 + 每段长度（s32vector）。
  ;; （这些繁琐的转换，下一步的 build-program 会替我们做）
  (glShaderSource shader 1 (vector src) (s32vector (string-length src)))
  (glCompileShader shader)
  shader)

;; 把若干着色器对象链接成一个"程序"
(define (link-program . shaders)
  (define prog (glCreateProgram))
  (for-each (lambda (s) (glAttachShader prog s)) shaders)
  (glLinkProgram prog)
  prog)

(run-gl
 #:title "02-01 着色器程序（原始过程）" #:width 400 #:height 300
 #:init (lambda ()
          (define vs (compile-shader GL_VERTEX_SHADER vert-src))
          (define fs (compile-shader GL_FRAGMENT_SHADER frag-src))
          (define prog (link-program vs fs))
          (glUseProgram prog)          ; 设为当前程序（状态机会记住）
          (printf "着色器程序编译链接成功，ID = ~a\n" prog))
 #:draw (lambda ()
          (glClearColor 0.10 0.12 0.20 1.0)
          (glClear GL_COLOR_BUFFER_BIT)))
