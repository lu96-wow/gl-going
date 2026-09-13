#lang racket/base
(require "../racket-glsl/rewrite.rkt")

(define (dump title prog)
  (printf "\n===== ~a =====\n" title)
  (printf "--- 美化源码 ---\n~a\n" (glsl-program-src prog))
  (printf "--- 映射 ---\n")
  (for ([f (glsl-program-forms prog)])
    (printf "  行~a-~a  .rkt~a:~a  text=~s\n"
            (glsl-form-start-line f) (glsl-form-end-line f)
            (glsl-form-src-line f) (glsl-form-src-col f)
            (glsl-form-text f))))

;; 1) 空函数体 + 多个函数
(dump "空函数体"
      (glsl (version 330 core)
            (define (f) void)
            (define (main) void
              (f))))

;; 2) if / else（} else 同行） + while + do-while（} while 同行）
(dump "if/else + while + do-while"
      (glsl (version 330 core)
            (define (main) void
              (if #t (f) (g))
              (while #f (h))
              (do-while #f (k)))))

;; 3) struct 顶层
(dump "struct"
      (glsl (version 330 core)
            (struct Light (vec3 pos) (vec3 color))
            (define (main) void
              (set! gl_Position (vec4 0.0 0.0 0.0 1.0)))))

;; 4) raw 里带字符串字面量（含 ;{}()）
(dump "raw 字符串"
      (glsl (version 330 core)
            (raw "const char* s = \"a;b{c}(d)\";")
            (define (main) void)))

;; 5) raw 不带结尾换行 + 后续 form（映射边界）
(dump "raw 不带换行"
      (glsl (version 330 core)
            (raw "int x = 1; int y = 2;")
            (define (main) void)))
