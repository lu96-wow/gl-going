#lang racket/base

;; ============================================================
;; gl-error.rkt —— OpenGL 报错的四段式源映射处理（纯函数层）
;;
;; 报错拆成四个独立的"视角"，每个只做一件事：
;;   ① OpenGL 行列报错   原始编译器日志（0:行(列): error: ...）
;;   ② OpenGL 美化报错   美化后的 GLSL 源码 + ^（指到 GLSL 出错列）
;;   ③ s表达式 行列报错   .rkt 里的 form / 标识符位置（文件:行:列）
;;   ④ s表达式 美化报错   s 表达式本身 + ^（指到出错标识符）
;;
;; 全部是 数据 → lambda → 数据 的纯函数，无副作用。
;; 四个渲染器各自独立、可单独打印；render-error 把它们拼成一整段。
;; ============================================================

(require racket/string "core.rkt" "glsl-program.rkt")

(provide parse-gl-error-log locate-gl-error render-error
         render-gl-linecol render-gl-pretty render-sexpr-linecol render-sexpr-pretty
         (struct-out gl-error) (struct-out located-error))

;; ---------- 数据 ----------

;; 一条解析出的编译器错误：line/col 解析不出时为 #f
(struct gl-error (line col msg) #:transparent)

;; 一条定位后的错误：
;;   line/col/msg = GLSL 里的位置与消息；form = 命中的顶层 form（源映射）
;;   caret-col = 美化 GLSL 里的 1 起列；token-name/token-locs = 出错标识符及其 .rkt 位置
(struct located-error (line col msg form caret-col token-name token-locs) #:transparent)

;; ---------- ① 解析 ----------

;; 驱动数据：日志行的位置格式。Mesa：0:行(列): error: 消息
;; 正则 group1 = shader 号，group2 = 行，group3 = 列（0 起）
(define log-line-pattern #rx"([0-9]+):([0-9]+)[(]([0-9]+)[)]:")

(define (parse-gl-error-log log)
  (for/list ([ln (string-split log "\n")]
             #:when (not (string=? ln "")))
    (define m (regexp-match log-line-pattern ln))
    (if m
        (gl-error (string->number (list-ref m 2))
                  (string->number (list-ref m 3))
                  ln)
        (gl-error #f #f ln))))

;; ---------- ② 定位 ----------

;; 从日志消息里抠出出错标识符名：`aPs' undeclared → "aPs"
(define log-token-pattern #rx"`([A-Za-z_][A-Za-z0-9_]*)'")
(define (token-name-from-msg msg)
  (define m (regexp-match log-token-pattern msg))
  (and m (list-ref m 1)))

;; 在源码第 line 行第 col 列（1 起）处读出那个标识符名；读不到则 #f
(define (token-name-at src line col)
  (define ls (string-split src "\n"))
  (and (<= 1 line (length ls))
       (let ([l (list-ref ls (sub1 line))])
         (and (<= 1 col (string-length l))
              (let ([i (sub1 col)])
                (and (ident-char? (string-ref l i))
                     (let* ([start (let loop ([j i])
                                     (if (and (> j 0) (ident-char? (string-ref l (sub1 j))))
                                         (loop (sub1 j)) j))]
                            [end (let loop ([j i])
                                   (if (and (< j (string-length l)) (ident-char? (string-ref l j)))
                                       (loop (add1 j)) j))])
                       (substring l start end))))))))

;; 在 token 表里查这个名字的所有出现位置
(define (token-locations prog name)
  (for/list ([t (glsl-program-tokens prog)]
             #:when (string=? (glsl-token-name t) name))
    t))

(define (locate-gl-error src err)
  (define line (gl-error-line err))
  (define col (gl-error-col err))
  (cond
    [(and line col)
     (define form (and (glsl-program? src) (glsl-program-lookup src line)))
     (define caret-col (add1 col))   ; Mesa 列 0 起 → 1 起
     (define name (or (token-name-from-msg (gl-error-msg err))
                      (token-name-at (glsl-src src) line caret-col)))
     (define locs (or (and name (glsl-program? src) (token-locations src name)) '()))
     (located-error line col (gl-error-msg err) form caret-col name locs)]
    [else
     (located-error #f #f (gl-error-msg err) #f #f #f '())]))

;; ---------- ③ 四个渲染器 ----------

;; 取第一条满足条件的定位错误
(define (first-located located pred)
  (for/first ([e located] #:when (pred e)) e))

;; ① OpenGL 行列报错：原始日志
(define (render-gl-linecol log)
  (string-append "① OpenGL 行列报错（编译器日志）\n" log))

;; ② OpenGL 美化报错：美化 GLSL 源码 + ^
(define (render-gl-pretty src located)
  (define e (first-located located located-error-line))
  (if (not e)
      ""
      (string-append "② OpenGL 美化报错\n"
                     (render-gl-src-caret (glsl-src src)
                                          (located-error-line e)
                                          (located-error-caret-col e)))))

;; ③ s表达式 行列报错：form 起点 + 出错标识符在 .rkt 里的位置
(define (render-sexpr-linecol located)
  (define e (first-located located located-error-form))
  (if (not e)
      ""
      (let* ([f (located-error-form e)]
             [p (glsl-form-src-path f)]
             [form-pos (if p
                           (format "~a:~a:~a" p (glsl-form-src-line f) (add1 (glsl-form-src-col f)))
                           "未知")]
             [tok (located-error-token-name e)]
             [tok-poses
              (map (lambda (t)
                     (format "~a:~a:~a" (glsl-token-src-path t) (glsl-token-src-line t) (add1 (glsl-token-src-col t))))
                   (located-error-token-locs e))])
        (string-append
         "③ s表达式 行列报错\n"
         (format "  form 起点：~a\n" form-pos)
         (if tok
             (format "  标识符 ~a：~a\n" tok (string-join tok-poses "、"))
             "")))))

;; ④ s表达式 美化报错：s 表达式 + ^（指到出错标识符）
(define (render-sexpr-pretty located)
  (define e (first-located located (lambda (x) (and (located-error-form x) (located-error-token-name x)))))
  (if (not e)
      ""
      (let* ([f (located-error-form e)]
             [s (pretty-sexpr (glsl-form-text f))]
             [pos (token-position-in s (located-error-token-name e))])
        (if (not pos)
            ""
            (string-append "④ s表达式 美化报错\n"
                           (render-sexpr-caret s (car pos) (cadr pos)
                                               (string-length (located-error-token-name e))))))))

;; ---------- 渲染工具 ----------

;; GLSL 源码 + 行号 + ^（caret-col 1 起）
(define (render-gl-src-caret src line caret-col)
  (define src-lines (string-split src "\n"))
  (define w (string-length (number->string (length src-lines))))
  (define (pad s) (string-append (make-string (- w (string-length s)) #\space) s))
  (string-join
   (for/list ([i (in-naturals 1)] [l src-lines])
     (cond
       [(= i line)
        (string-append
         (pad (number->string i)) " | " l "\n"
         (make-string w #\space) " | " (make-string (sub1 caret-col) #\space) "^")]
       [else
        (string-append (pad (number->string i)) " | " l)]))
   "\n"))

;; s 表达式 + ^（无行号；caret-len = 下划线长度）
(define (render-sexpr-caret src line caret-col caret-len)
  (define ls (string-split src "\n"))
  (string-join
   (for/list ([i (in-naturals 1)] [l ls])
     (if (= i line)
         (string-append l "\n" (make-string (sub1 caret-col) #\space) (make-string caret-len #\^))
         l))
   "\n"))

;; s 表达式 → 美化文本（短列表一行；长列表按深度换行缩进）
(define (pretty-sexpr d)
  (let go ([d d] [depth 0])
    (cond
      [(pair? d)
       (define parts (map (lambda (x) (go x (add1 depth))) d))
       (define one (string-append "(" (string-join parts " ") ")"))
       (if (<= (string-length one) 70)
           one
           (string-append "(" (string-join parts (string-append "\n" (make-string (* 2 (add1 depth)) #\space))) ")"))]
      [else (format "~s" d)])))

;; 在 s 里找名为 name 的标识符（词边界），返回 (line col) 或 #f
(define (token-position-in s name)
  (define n (string-length s))
  (define nl (string-length name))
  (let loop ([i 0] [line 1] [col 1])
    (cond
      [(>= i n) #f]
      [else
       (define c (string-ref s i))
       (cond
         [(char=? c #\newline) (loop (add1 i) (add1 line) 1)]
         [(and (<= (+ i nl) n)
               (equal? (substring s i (+ i nl)) name)
               (or (= i 0) (not (ident-char? (string-ref s (sub1 i)))))
               (or (= (+ i nl) n) (not (ident-char? (string-ref s (+ i nl))))))
          (list line col)]
         [else (loop (add1 i) line (add1 col))])])))

;; ---------- ④ 组合 ----------

;; 四段拼起来；空段跳过
(define (render-error log src located)
  (string-join
   (filter (lambda (s) (not (string=? s "")))
           (list (render-gl-linecol log)
                 (render-gl-pretty src located)
                 (render-sexpr-linecol located)
                 (render-sexpr-pretty located)))
   "\n\n"))
