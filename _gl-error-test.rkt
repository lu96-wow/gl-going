#lang racket/base
;; 临时测试：故意写错着色器，看报错输出
(require "01-window/04-gui-tool.rkt")   ; make-window（拿 GL 上下文）
(require "racket-glsl/rewrite.rkt")     ; (glsl ...) 宏 + glsl-pretty
(require "racket-glsl/tool.rkt")        ; build-program

;; 顶点着色器里故意把 aPos 打成 aPs（未声明的变量）
(define vert-src
  (glsl (version 330 core)
        (layout (location 0) in vec2 aPos)
        (define (main) void
          (set! gl_Position (vec4 aPs 0.0 1.0)))))

(define frag-src
  (glsl (version 330 core)
        (out vec4 FragColor)
        (define (main) void
          (set! FragColor (vec4 1.0 0.35 0.2 1.0)))))

(printf "== 编译器收到的字符串（glsl-program-src）==\n~a\n\n" (glsl-program-src vert-src))
(printf "== 单行中间串（已不对外暴露，仅供对照）==\n（无——glsl-mapped 内部拼完就直接美化，不再保留）\n\n")
(printf "== 美化后的样子 ==\n~a\n\n" (glsl-pretty vert-src))

(define-values (frame canvas) (make-window #:title "错误测试"))
(send canvas with-gl-context
  (lambda ()
    (build-program (gl-vertex-shader vert-src)
                   (gl-fragment-shader frag-src))))
