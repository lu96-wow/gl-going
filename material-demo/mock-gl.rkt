#lang racket/base
;; ============================================================
;; mock-gl.rkt —— 极简 GL 模拟（只为对比 material，不需要 GPU）
;;
;; 忠实复现两件真 GL 的行为：
;;   1) glGetUniformLocation 返回的是"**某个 program 内**的整数位置"；
;;   2) 把 A program 的位置用在 B program 上 → **静默忽略**（GL 不报错、
;;      也不抛异常，只是什么都没设上）。
;; 把每次调用记进 trace，方便两种写法对照。
;; ============================================================

(require racket/list racket/string)

(provide make-gl glCreateProgram glGetUniformLocation glUseProgram
         glUniform glDraw
         gl-trace gl-value gl-dump gl-clear-trace
         prog-id prog-name)

;; 一个 program：id、名字、uniform 名↔loc 双向表
(struct prog (id name name->loc loc->name) #:transparent)

;; GL 状态：程序表 / 全局 loc 计数 / 当前程序 / 已生效的值 / 调用轨迹
;; （loc 全局唯一，便于演示"外来 loc 在当前 program 里不存在"）
(struct gl (progs next-loc cur values log) #:transparent #:mutable)

(define (make-gl) (gl '() 0 #f (make-hash) '()))

(define (note! g s) (set-gl-log! g (cons s (gl-log g))))

(define (glCreateProgram g name uniform-names)
  (define base (gl-next-loc g))
  (define name->loc (for/hash ([u (in-list uniform-names)] [i (in-naturals base)])
                      (values u i)))
  (define loc->name (for/hash ([(u loc) (in-hash name->loc)]) (values loc u)))
  (define p (prog (length (gl-progs g)) name name->loc loc->name))
  (set-gl-next-loc! g (+ base (length uniform-names)))
  (set-gl-progs! g (append (gl-progs g) (list p)))
  (note! g (format "glCreateProgram  ~a  {uniforms: ~a}" name (string-join uniform-names ", ")))
  p)

(define (glGetUniformLocation g p name)
  (define loc (hash-ref (prog-name->loc p) name -1))
  (note! g (format "glGetUniformLocation  ~a.~a -> ~a" (prog-name p) name loc))
  loc)

(define (glUseProgram g p)
  (set-gl-cur! g p)
  (note! g (format "glUseProgram  ~a" (prog-name p))))

(define (glUniform g loc value)
  (define p (gl-cur g))
  (cond
    [(not p)
     (note! g (format "glUniform loc=~a  -> **静默忽略**：当前没有绑定 program" loc))]
    [(hash-has-key? (prog-loc->name p) loc)
     (define nm (hash-ref (prog-loc->name p) loc))
     (hash-set! (gl-values g) (cons (prog-id p) nm) value)
     (note! g (format "glUniform loc=~a  -> ~a.~a = ~s" loc (prog-name p) nm value))]
    [else
     (note! g (format "glUniform loc=~a  -> **静默忽略**：loc 不属于当前 program `~a'（真 GL 也不报错）"
                      loc (prog-name p)))]))

(define (glDraw g what) (note! g (format "glDraw  ~a" what)))

(define (gl-trace g) (reverse (gl-log g)))
(define (gl-clear-trace g) (set-gl-log! g '()))
(define (gl-value g p name) (hash-ref (gl-values g) (cons (prog-id p) name) '(未设置)))

;; 打印"已生效的 uniform 值"
(define (gl-dump g progs)
  (for ([p (in-list progs)])
    (printf "    ~a: ~a\n" (prog-name p)
            (string-join (for/list ([u (sort (hash-keys (prog-name->loc p)) string<?)])
                           (format "~a=~s" u (gl-value g p u)))
                         "  "))))
