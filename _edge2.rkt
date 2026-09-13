#lang racket/base
(require "racket-glsl/rewrite.rkt")

(define (dump title prog)
  (printf "\n===== ~a =====\n~a" title (glsl-program-src prog))
  (printf "-- 映射 --\n")
  (for ([f (glsl-program-forms prog)])
    (printf "  行~a-~a  text=~s\n"
            (glsl-form-start-line f) (glsl-form-end-line f) (glsl-form-text f))))

;; 6) swizzle 带参（回归：.xy / .g）+ 零参 (g) 现在应是函数调用
(dump "swizzle"
      (glsl (version 330 core)
            (define (main) void
              (set! gl_Position (xy aPos 0.0 1.0))   ; 其实是 (vec4 (xy aPos) 0.0 1.0)
              (g))))                                 ; 零参：函数调用 g()

;; 7) cond 语句 + array 类型
(dump "cond + array"
      (glsl (version 330 core)
            (uniform (array float 4) weights)
            (define (main) void
              (cond [(> aPos.x 0.0) (h)]
                    [else (k)]))))

;; 8) raw 不带终止符（退化：下一 form 会接到同一行）
(dump "raw 无终止符"
      (glsl (version 330 core)
            (raw "int x = 1")
            (define (main) void)))

;; 9) glsl-pretty 幂等性
(define p (glsl (version 330 core)
                (layout (location 0) in vec2 aPos)
                (define (main) void
                  (set! gl_Position (vec4 aPos 0.0 1.0)))))
(define s (glsl-program-src p))
(printf "\n===== 幂等性 =====\n")
(printf "glsl-pretty(glsl-pretty(src)) == glsl-pretty(src): ~a\n"
        (string=? (glsl-pretty (glsl-pretty s)) (glsl-pretty s)))
