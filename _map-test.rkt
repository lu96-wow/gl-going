#lang racket/base
(require "racket-glsl/rewrite.rkt")

(define prog
  (glsl (version 330 core)
        (layout (location 0) in vec2 aPos)
        (define (main) void
          (set! gl_Position (vec4 aPos 0.0 1.0)))))

(printf "=== 美化源码 ===\n~a\n\n" (glsl-program-src prog))
(printf "=== 源映射（每个顶层 form）===\n")
(for ([f (glsl-program-forms prog)])
  (printf "  .rkt ~a:~a (span ~a) → GLSL 第 ~a-~a 行  text=~s\n"
          (glsl-form-src-line f)
          (glsl-form-src-col f)
          (glsl-form-src-span f)
          (glsl-form-start-line f)
          (glsl-form-end-line f)
          (glsl-form-text f)))
(printf "\n=== 反向查询（GLSL 行号 → form）===\n")
(for ([line (in-range 1 (add1 (glsl-form-end-line (car (reverse (glsl-program-forms prog))))))])
  (define f (glsl-program-lookup prog line))
  (printf "  行 ~a → ~a\n" line (and f (glsl-form-text f))))
