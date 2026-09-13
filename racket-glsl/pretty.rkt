#lang racket/base

;; ============================================================
;; pretty.rkt —— GLSL 美化器（字符串 → 多行缩进文本）
;;
;; 纯函数：数据 → 数据，无副作用（不可变状态 + 递归，无 set!/box）。
;; 规则：按 ; { } 换行，按 {} 深度缩进 2 格；保留字符串内容与已有 \n。
;;   } 之后仅 else / while / ; / , / 接口块实例名 保持同行，否则换行。
;;
;; glsl-pretty-line-map：美化 + 在 marks（升序 raw 下标）处记录"该字符落在第几行"
;;   —— 这是源映射的基础：glsl-program.rkt 靠它算出每个顶层 form 的行区间。
;; ============================================================

(require racket/string "core.rkt")   ; string-join / ident-char?

(provide glsl-pretty-line-map glsl-pretty)

;; ---------- 美化状态（不可变，靠 struct-copy 推进） ----------

(struct pstate (lines cur brace paren in-str? started?) #:transparent)

(define (pstate-flush st)
  (if (string=? (pstate-cur st) "")
      (struct-copy pstate st [started? #f])
      (struct-copy pstate st [lines (cons (pstate-cur st) (pstate-lines st))] [cur ""] [started? #f])))

(define (pstate-append st cs)
  (struct-copy pstate st [cur (string-append (pstate-cur st) cs)]))

;; 新起一行时先补缩进；已在本行则原样
(define (pstate-start st)
  (if (pstate-started? st)
      st
      (pstate-append (struct-copy pstate st [started? #t])
                     (make-string (* 2 (pstate-brace st)) #\space))))

;; i 起跳过空白，返回下一个非空白下标（无则 n）
(define (skip-ws s n i)
  (cond [(>= i n) n]
        [(char-whitespace? (string-ref s i)) (skip-ws s n (add1 i))]
        [else i]))

;; i 处是否以 word 开头（词边界）
(define (word-at? s n i word)
  (define wl (string-length word))
  (and (<= (+ i wl) n)
       (equal? (substring s i (+ i wl)) word)
       (or (= (+ i wl) n)
           (not (ident-char? (string-ref s (+ i wl)))))))

;; j 处是否形如 "名字;" / "名字["（接口块实例名，} 之后应保持同行）
(define (instance-name? s n j)
  (and (< j n)
       (ident-char? (string-ref s j))
       (let scan ([k j])
         (cond [(>= k n) #f]
               [(ident-char? (string-ref s k)) (scan (add1 k))]
               [(char=? (string-ref s k) #\space) (scan (add1 k))]
               [else (memv (string-ref s k) '(#\; #\[))]))))

;; 处理一个字符 → 新状态（纯：s=整串 n=长度 i=下标）
(define (pstate-advance s n st i)
  (define c (string-ref s i))
  (define cs (string c))
  (cond
    [(pstate-in-str? st)
     (struct-copy pstate st [cur (string-append (pstate-cur st) cs)] [in-str? (not (char=? c #\"))])]
    [(char=? c #\")
     (struct-copy pstate (pstate-append (pstate-start st) cs) [in-str? #t])]
    [(char=? c #\newline)
     (pstate-flush st)]
    [(char=? c #\()
     (struct-copy pstate (pstate-append (pstate-start st) cs) [paren (add1 (pstate-paren st))])]
    [(char=? c #\))
     (struct-copy pstate (pstate-append st cs) [paren (sub1 (pstate-paren st))])]
    [(char=? c #\{)
     (struct-copy pstate (pstate-flush (pstate-append (pstate-start st) cs)) [brace (add1 (pstate-brace st))])]
    [(char=? c #\})
     (define st1 (struct-copy pstate (pstate-flush st) [brace (sub1 (pstate-brace st))]))
     (define st2 (pstate-append (pstate-start st1) cs))
     (define j (skip-ws s n (add1 i)))
     (if (or (and (< j n) (memv (string-ref s j) '(#\; #\,)))
             (word-at? s n j "else")
             (word-at? s n j "while")
             (instance-name? s n j))
         st2
         (pstate-flush st2))]
    [(and (char=? c #\;) (zero? (pstate-paren st)))
     (pstate-flush (pstate-append st cs))]
    [(char-whitespace? c)
     (if (pstate-started? st) (pstate-append st cs) st)]
    [else
     (pstate-append (pstate-start st) cs)]))

;; 美化 + 在 marks（升序 raw 下标）处记录"该字符落在第几行"。
;; 返回 (values 美化串 各行号)，行号与 marks 一一对应。
(define (glsl-pretty-line-map s marks)
  (define n (string-length s))
  (define (go i st marks-left recorded)
    (if (>= i n)
        (let ([final (pstate-flush st)])
          (values (string-join (reverse (pstate-lines final)) "\n")
                  (reverse recorded)))
        (let ([hit? (and (pair? marks-left) (= i (car marks-left)))])
          (go (add1 i)
              (pstate-advance s n st i)
              (if hit? (cdr marks-left) marks-left)
              (if hit? (cons (add1 (length (pstate-lines st))) recorded) recorded)))))
  (go 0 (pstate '() "" 0 0 #f #f) marks '()))

;; 美化（不记录位置）：字符串 → 多行缩进文本
(define (glsl-pretty s)
  (define-values (str _) (glsl-pretty-line-map s '()))
  str)
