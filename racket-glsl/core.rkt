#lang racket/base

;; ============================================================
;; GLSL 核心（Layer 0）——全部函数返回字符串，不含换行
;;
;; 规则：
;;   1. 文本都是字符串（类型 "vec2"、变量 "aPos"、运算符 "+"）；
;;      数字是 Racket 数，~a 输出。
;;   2. 表达式不带分号；语句带分号/花括号；花括号内用空格而非换行。
;;   3. 二元/一元/三元永远自带括号，优先级天然安全。
;;   4. 名字全部 glsl- 前缀，与 Racket 核心零冲突。
;;   5. 换行/缩进不属于核心：用 glsl-pretty 统一美化。
;;      例外：#version 等预处理指令在 GLSL 中必须独占一行，
;;      所以 glsl-version 保留结尾 \n（语义要求，非格式化）。
;;
;; 之后的重写层（Layer 1）只做"位置驱动改名"，
;; 把用户表面(vec2 / set! / if / + ...)重写成本文件的调用。
;; ============================================================

(require racket/format racket/string)

(provide
 ;; 拼装 / 美化 / 源映射
 glsl-shader glsl-pretty glsl-version glsl-raw
 glsl-src glsl-mapped glsl-program-lookup
 (struct-out glsl-program)
 (struct-out glsl-form)
 ;; 声明
 glsl-decl glsl-in glsl-out glsl-uniform glsl-const glsl-layout glsl-layout-qual
 ;; 表达式
 glsl-call glsl-ctor glsl-bin glsl-unary glsl-ternary
 glsl-assign glsl-cassign glsl-swizzle glsl-field glsl-aref glsl-inc glsl-dec
 ;; 语句
 glsl-stmt glsl-block glsl-if glsl-for glsl-while glsl-do-while
 glsl-switch glsl-break glsl-continue glsl-return glsl-discard
 ;; 函数 / 结构
 glsl-param glsl-fn glsl-field-decl glsl-struct-decl)

;; ---------- 内部工具 ----------

;; 任意值 → 字符串：字符串原样，其余用 ~a；拒绝有理数（GLSL 无此字面量）
(define (->str x)
  (cond
    [(string? x) x]
    [(and (exact? x) (rational? x) (not (integer? x)))
     (error 'glsl "GLSL 没有有理数字面量：~s。请写小数（如 0.5）或用 (/ 1.0 2.0)" x)]
    [else (~a x)]))

;; 若是 "{" 开头的块就不重复包，否则包成块（单行，空格分隔）
(define (block? x) (and (string? x) (string-prefix? x "{")))
(define (wrap-block x)
  (if (block? x) x (format "{ ~a }" x)))

;; 去掉结尾分号（for 的 init 子句复用 glsl-decl 时用）
(define (trim-semicolon str)
  (if (string-suffix? str ";")
      (substring str 0 (sub1 (string-length str)))
      str))

;; 限定符列表 → 前缀字符串（空列表 → ""）
(define (quals->prefix quals)
  (if (null? quals) "" (string-append (string-join quals " ") " ")))

;; 一组片段拼成空格分隔文本
(define (join-spaces xs)
  (string-join (map ->str xs) " "))

;; ---------- 0. 拼装 / 美化 ----------

;; 拼装：片段之间用空格分隔（换行交给 glsl-pretty）
(define (glsl-shader . parts)
  (string-join (map ->str parts) " "))

;; ---------- (glsl ...) 的产物：带源映射的 GLSL 程序 ----------

;; glsl-program：src = 美化后的 GLSL（可直接编译）；forms = 每个顶层 form 的源映射
(struct glsl-program (src forms) #:transparent)
;; glsl-form：一个顶层 form 的源映射
;;   src-line/src-col/src-span = 它在 .rkt 源文件里的位置（来自 syntax）
;;   text = 该 form 的原始 s 表达式；start-line/end-line = 生成 GLSL 落在哪几行（闭区间）
(struct glsl-form (src-line src-col src-span text start-line end-line) #:transparent)

;; 取出 GLSL 源文本：glsl-program → 它的 src；字符串 → 原样
(define (glsl-src x)
  (if (glsl-program? x) (glsl-program-src x) x))

;; ---------- 美化（纯函数：数据 → 数据，无副作用） ----------

;; 美化状态（不可变，靠 struct-copy 推进）
(struct ps (lines cur brace paren in-str? started?) #:transparent)

(define (ps-flush st)
  (if (string=? (ps-cur st) "")
      (struct-copy ps st [started? #f])
      (struct-copy ps st [lines (cons (ps-cur st) (ps-lines st))] [cur ""] [started? #f])))

(define (ps-append st cs)
  (struct-copy ps st [cur (string-append (ps-cur st) cs)]))

;; 新起一行时先补缩进；已在本行则原样
(define (ps-start st)
  (if (ps-started? st)
      st
      (ps-append (struct-copy ps st [started? #t])
                 (make-string (* 2 (ps-brace st)) #\space))))

(define (ident-char? c)
  (or (char-alphabetic? c) (char-numeric? c) (char=? c #\_)))

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
(define (ps-advance s n st i)
  (define c (string-ref s i))
  (define cs (string c))
  (cond
    [(ps-in-str? st)
     (struct-copy ps st [cur (string-append (ps-cur st) cs)] [in-str? (not (char=? c #\"))])]
    [(char=? c #\")
     (struct-copy ps (ps-append (ps-start st) cs) [in-str? #t])]
    [(char=? c #\newline)
     (ps-flush st)]
    [(char=? c #\()
     (struct-copy ps (ps-append (ps-start st) cs) [paren (add1 (ps-paren st))])]
    [(char=? c #\))
     (struct-copy ps (ps-append st cs) [paren (sub1 (ps-paren st))])]
    [(char=? c #\{)
     (struct-copy ps (ps-flush (ps-append (ps-start st) cs)) [brace (add1 (ps-brace st))])]
    [(char=? c #\})
     (define st1 (struct-copy ps (ps-flush st) [brace (sub1 (ps-brace st))]))
     (define st2 (ps-append (ps-start st1) cs))
     (define j (skip-ws s n (add1 i)))
     (if (or (and (< j n) (memv (string-ref s j) '(#\; #\,)))
             (word-at? s n j "else")
             (word-at? s n j "while")
             (instance-name? s n j))
         st2
         (ps-flush st2))]
    [(and (char=? c #\;) (zero? (ps-paren st)))
     (ps-flush (ps-append st cs))]
    [(char-whitespace? c)
     (if (ps-started? st) (ps-append st cs) st)]
    [else
     (ps-append (ps-start st) cs)]))

;; 美化 + 在 marks（升序 raw 下标）处记录"该字符落在第几行"。
;; 纯函数：数据 → 数据，无 set! / box。
(define (glsl-pretty-line-map s marks)
  (define n (string-length s))
  (define (go i st marks-left recorded)
    (if (>= i n)
        (let ([final (ps-flush st)])
          (values (string-join (reverse (ps-lines final)) "\n")
                  (reverse recorded)))
        (let ([hit? (and (pair? marks-left) (= i (car marks-left)))])
          (go (add1 i)
              (ps-advance s n st i)
              (if hit? (cdr marks-left) marks-left)
              (if hit? (cons (add1 (length (ps-lines st))) recorded) recorded)))))
  (go 0 (ps '() "" 0 0 #f #f) marks '()))

;; 美化（不记录位置）。参数也接受 glsl-program（取它的 src）。
(define (glsl-pretty s)
  (define-values (str _) (glsl-pretty-line-map (glsl-src s) '()))
  str)

;; ---------- 源映射构造 ----------

;; 每个片段在 raw 串里的 [start, end] 闭区间下标
(define (part-spans strs)
  (let loop ([xs strs] [offset 0] [acc '()])
    (if (null? xs)
        (reverse acc)
        (let ([len (string-length (car xs))])
          (define start offset)
          (define end (max start (- (+ start len) 1)))  ; len>=1 时 = start+len-1；空串退化为 start
          (loop (cdr xs) (+ start len 1) (cons (cons start end) acc))))))

;; 把 (parts source-infos) 拼成 glsl-program：
;;   parts = 各顶层 form 生成的 GLSL 字符串（已求值，顺序对应）
;;   source-infos = 各 form 的 (src-line src-col src-span text)
(define (glsl-mapped parts source-infos)
  (define spans (part-spans parts))
  (define marks (apply append (map (lambda (sp) (list (car sp) (cdr sp))) spans)))
  (define-values (pretty lines) (glsl-pretty-line-map (string-join parts " ") marks))
  (define forms
    (for/list ([info source-infos] [i (in-naturals)])
      (glsl-form (list-ref info 0) (list-ref info 1) (list-ref info 2) (list-ref info 3)
                 (list-ref lines (* 2 i))
                 (list-ref lines (+ (* 2 i) 1)))))
  (glsl-program pretty forms))

;; 查询：美化 GLSL 第 line 行落在哪个顶层 form；没有则 #f
(define (glsl-program-lookup prog line)
  (for/first ([f (glsl-program-forms prog)]
              #:when (<= (glsl-form-start-line f) line (glsl-form-end-line f)))
    f))

(define (glsl-version n [profile #f])
  (if profile
      (format "#version ~a ~a\n" (->str n) (->str profile))
      (format "#version ~a\n" (->str n))))

(define (glsl-raw s) (->str s))

;; ---------- 1. 声明 ----------

;; quals = 字符串列表，如 '("in")、'("in" "flat")，无则 '()
;; init  = 表达式字符串或 #f
(define (glsl-decl quals type name [init #f])
  (format "~a~a ~a~a;"
          (quals->prefix quals) (->str type) (->str name)
          (if init (format " = ~a" (->str init)) "")))

(define (glsl-in t n) (glsl-decl '("in") t n))
(define (glsl-out t n) (glsl-decl '("out") t n))
(define (glsl-uniform t n) (glsl-decl '("uniform") t n))
(define (glsl-const t n init) (glsl-decl '("const") t n init))

;; spec = 字符串或 (名 值) 的列表：'("std140" ("location" 0) ("binding" 1))
(define (layout-item->str p)
  (if (list? p)
      (format "~a = ~a" (car p) (->str (cadr p)))
      (->str p)))

(define (glsl-layout spec quals type name [init #f])
  (format "~a~a ~a ~a~a;"
          (format "layout(~a)" (string-join (map layout-item->str spec) ", "))
          (if (null? quals) "" (string-append " " (string-join quals " ")))
          (->str type) (->str name)
          (if init (format " = ~a" (->str init)) "")))

;; 独立 layout（无类型/变量名）：layout(...) in;（compute/几何/early_fragment_tests）
(define (glsl-layout-qual spec quals)
  (format "layout(~a)~a;"
          (string-join (map layout-item->str spec) ", ")
          (if (null? quals) "" (string-append " " (string-join quals " ")))))

;; ---------- 2. 表达式 ----------

(define (glsl-call f . args)
  (format "~a(~a)" (->str f) (string-join (map ->str args) ", ")))

;; 构造器 = 类型名当函数调用
(define glsl-ctor glsl-call)

(define (glsl-bin op . args)
  (format "(~a)" (string-join (map ->str args) (format " ~a " (->str op)))))

(define (glsl-unary op x)
  (format "(~a~a)" (->str op) (->str x)))

(define (glsl-ternary c t e)
  (format "(~a ? ~a : ~a)" (->str c) (->str t) (->str e)))

(define (glsl-assign l r)
  (format "~a = ~a" (->str l) (->str r)))

(define (glsl-cassign op l r)
  (format "~a ~a= ~a" (->str l) (->str op) (->str r)))

(define (glsl-swizzle comps v)
  (format "~a.~a" (->str v) (->str comps)))

(define (glsl-field f s)
  (format "~a.~a" (->str s) (->str f)))

(define (glsl-aref a i)
  (format "~a[~a]" (->str a) (->str i)))

(define (glsl-inc x [post #f])
  (if post (format "~a++" (->str x)) (format "++~a" (->str x))))

(define (glsl-dec x [post #f])
  (if post (format "~a--" (->str x)) (format "--~a" (->str x))))

;; ---------- 3. 语句 ----------

(define (glsl-stmt e) (format "~a;" (->str e)))

(define (glsl-block . stmts)
  (format "{ ~a }" (join-spaces stmts)))

(define (glsl-if c t [e #f])
  (if e
      (format "if (~a) ~a else ~a"
              (->str c) (wrap-block t)
              ;; else 分支若本身是 if → 生成 else if（不套多余的 {}）
              (if (string-prefix? (->str e) "if ") (->str e) (wrap-block e)))
      (format "if (~a) ~a" (->str c) (wrap-block t))))

(define (glsl-for init cond update . body)
  (format "for (~a; ~a; ~a) ~a"
          (trim-semicolon (->str init)) (->str cond) (->str update)
          (wrap-block (join-spaces body))))

(define (glsl-while cond . body)
  (format "while (~a) ~a" (->str cond) (wrap-block (join-spaces body))))

(define (glsl-do-while cond . body)
  (format "do ~a while (~a);" (wrap-block (join-spaces body)) (->str cond)))

;; switch：cases = (list (list "1" "{...}") ...)，default = "{...}" 或 #f
;; 每个 case 体是块，glsl-pretty 靠 {} 自动排版
(define (glsl-switch test cases [default #f])
  (format "switch (~a) { ~a ~a }"
          (->str test)
          (string-join (for/list ([c cases])
                         (format "case ~a: ~a" (->str (car c)) (cadr c)))
                       " ")
          (if default (format "default: ~a" default) "")))

(define (glsl-break) "break;")
(define (glsl-continue) "continue;")

(define (glsl-return [v #f])
  (if v (format "return ~a;" (->str v)) "return;"))

(define (glsl-discard) "discard;")

;; ---------- 4. 函数 / 结构 ----------

(define (glsl-param quals type name)
  (format "~a~a ~a" (quals->prefix quals) (->str type) (->str name)))

(define (glsl-fn name ret params . body)
  (format "~a ~a(~a) { ~a }"
          (->str ret) (->str name)
          (string-join (map ->str params) ", ")
          (join-spaces body)))

(define (glsl-field-decl type name)
  (format "~a ~a;" (->str type) (->str name)))

(define (glsl-struct-decl name . fields)
  (format "struct ~a { ~a };"
          (->str name) (join-spaces fields)))
