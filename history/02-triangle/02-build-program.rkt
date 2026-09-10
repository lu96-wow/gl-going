#lang racket/base
;; =========================================================
;; 02-triangle/02-build-program.rkt —— 第二步：把编译链接收进 build-program
;; 运行：racket 02-triangle/02-build-program.rkt
;; =========================================================
;; 上一步：glCreateShader → glLinkProgram 那 7 个调用每次都一样——纯重复。
;; 本步把它收成一个函数 build-program（lib.rkt 已提供），从本步起直接调用。
;;
;; build-program 的实现 = 上一步的 compile-shader + link-program：
;;   编译两段着色器 → 链接成一个程序 → 返回它的编号。
;; 它还多做一件上一步省掉的事：链接失败时打印着色器日志
;;   （glGetShaderInfoLog / glGetProgramInfoLog）——以后 shader 写错，
;;   报错信息就是从这里来的。
;;
;; ★记住这个节奏（整门课通用）：
;;   ① 裸写一遍新机制（上一步）
;;   ② 发现它重复 → 收进 lib 成一个函数（本步）
;;   ③ 后面的课直接调用，聚焦本课真正的新东西
;; =========================================================

(require "../lib-gui.rkt")
(require "../lib.rkt")   ; build-program + (glsl ...)

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
 #:title "02-02 用 build-program" #:width 400 #:height 300
 #:init (lambda ()
          (define prog (build-program vert-src frag-src))  ; 上一步的 7 行 → 1 行
          (glUseProgram prog)
          (printf "着色器程序编译链接成功，ID = ~a\n" prog))
 #:draw (lambda ()
          (glClearColor 0.10 0.12 0.20 1.0)
          (glClear GL_COLOR_BUFFER_BIT)))
