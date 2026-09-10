#lang racket/base
;; =========================================================
;; macro-demo.rkt —— Racket 宏在 (glsl ...) 之上能做什么
;; 运行：racket macro-demo.rkt   （只打印，不开窗口）
;;
;; 边界：宏活在 (glsl ...) 之上，职责是"生成/改写 surface forms"，
;; 产出整段 (glsl ...)。宏跑完后，rename 层（rewrite.rkt）才照常把
;; surface 数据转成核心。两层不重叠 → 不冲突。
;;
;; 四个宏，各解决一类"手写会累/会错"的问题：
;;   1. alias-shader —— 自定义 surface 词汇（vector2 → vec2）
;;   2. attr-shader  —— 顶点布局 → in 声明（location 自动编号）
;;   3. ring-shader  —— 参数化 shader 模板（编译期常量）
;;   4. unroll       —— 编译期展开成直线代码（GLSL 无递归）
;; =========================================================

(require "racket-glsl/rewrite.rkt")
(require "racket-glsl/rename-vector.rkt")   ; glsl-byte-size/glsl-stride（宏2 的 CPU 侧演示）
(require (for-syntax racket/base))

;; 打印帮助：shader 已经是展开后的 GLSL 字符串
(define (show title src)
  (printf "━━━ ~a ━━━\n~a\n\n" title (glsl-pretty src)))

;; ═══════════ 宏 1：alias-shader —— 自定义类型名 ═══════════
;; 写 vector2/vector3/vector4/matrix4/real/boolean…，宏在编译期把它们
;; 改回 GLSL 的 vec2/vec3/vec4/mat4/float/bool，再交给 (glsl ...)。
;; 这是"用户自己的 rename"，发生在 DSL rename 层之前——两层接力：
;;   vector2 --(用户宏)--> vec2 --(DSL rename 层)--> glsl-ctor "vec2"
(begin-for-syntax
  (define alias-table
    '((vector2 . vec2) (vector3 . vec3) (vector4 . vec4)
      (matrix2 . mat2) (matrix3 . mat3) (matrix4 . mat4)
      (real . float) (integer . int) (boolean . bool)))
  (define (alias-subst x)
    (cond
      [(symbol? x) (let ([e (assq x alias-table)]) (if e (cdr e) x))]
      [(pair? x) (cons (alias-subst (car x)) (alias-subst (cdr x)))]
      [else x])))

(define-syntax (alias-shader stx)
  (syntax-case stx ()
    [(_ form ...)
     (with-syntax ([(ff ...)
                    (map (lambda (f)
                           (datum->syntax f (alias-subst (syntax->datum f))))
                         (syntax->list #'(form ...)))])
       #'(glsl ff ...))]))

(show "宏1  alias-shader：写 vector2 → 输出 vec2"
  (alias-shader
   (version 330 core)
   (in vector2 aPos)
   (out vector4 FragColor)
   (define (main) void
     (set! gl_Position (vector4 aPos 0.0 1.0)))))

;; ═══════════ 宏 2：attr-shader —— 布局列表 → in 声明 ═══════════
;; 一个 (TYPE name) 列表同时是 shader 声明和 CPU 步长的"唯一真相"。
;; location 按列表下标自动编号，不会再有 02 课那种手写 (location N)
;; 与 glVertexAttribPointer(N) 对不上的隐患。
(define-syntax (attr-shader stx)
  (syntax-case stx ()
    [(_ (spec ...) body ...)
     (let* ([specs (map syntax->datum (syntax->list #'(spec ...)))]
            [decls (for/list ([s specs] [i (in-naturals)])
                     (list 'layout (list 'location i) 'in (car s) (cadr s)))]
            [bodys (map syntax->datum (syntax->list #'(body ...)))])
       (datum->syntax stx (list* 'glsl (list 'version 330 'core)
                                 (append decls bodys))))]))

(show "宏2  attr-shader：布局列表 → in 声明（location 自动编号）"
  (attr-shader ((vec2 aPos) (vec2 aUV))
    (out vec2 vUV)
    (define (main) void
      (set! vUV aUV)
      (set! gl_Position (vec4 aPos 0.0 1.0)))))

;; 同一份布局列表还驱动 CPU 侧 stride/offset（运行时函数，非宏）——
;; 这是 attr-shader 的"另一面"：声明与步长同源，魔法数字消失。
(define (attr-offsets specs)
  (define out '())
  (define off 0)
  (for ([s specs] [i (in-naturals)])
    (set! out (cons (list i (car s) (glsl-byte-size (car s)) off) out))
    (set! off (+ off (glsl-byte-size (car s)))))
  (reverse out))

(printf "布局 ((vec2 aPos) (vec2 aUV)) 的 CPU 侧（对应 02 的 s4=16 / offset=8）：\n")
(for ([row (attr-offsets '((vec2 aPos) (vec2 aUV)))])
  (printf "  location ~a: type ~a, size ~a B, offset ~a B\n"
          (list-ref row 0) (list-ref row 1) (list-ref row 2) (list-ref row 3)))
(printf "  stride = ~a B\n\n" (* 4 (glsl-stride 'vec2 'vec2)))

;; ═══════════ 宏 3：ring-shader —— 参数化模板 ═══════════
;; 02 课的环形着色器，密度/两色作为编译期参数。多个变体 = 一个宏，
;; 复制粘贴消失；参数是编译期常量，GLSL 里不会冒出多余的 uniform。
(define-syntax (ring-shader stx)
  (syntax-case stx ()
    [(_ density colA colB)
     #'(glsl (version 330 core)
             (in vec2 vUV) (uniform float uTime) (out vec4 FragColor)
             (define (main) void
               (vec2 p (- (* vUV 2.0) 1.0))
               (float ring (fract (- (* (length p) density) uTime)))
               (vec3 c (mix colA colB ring))
               (set! FragColor (vec4 c 1.0))))]))

(show "宏3  ring-shader：密度 6.0 / 蓝紫两色"
  (ring-shader 6.0 (vec3 0.10 0.15 0.40) (vec3 0.10 0.70 1.00)))

;; ═══════════ 宏 4：unroll —— 编译期展开成直线代码 ═══════════
;; GLSL 没有递归；低版本还要求循环边界是编译期常数。把 n 个 octave
;; 在编译期展开成 (+ (noise 0) (noise 1) ...)，生成直线代码。
(define-syntax (unroll stx)
  (syntax-case stx ()
    [(_ n)
     (with-syntax ([(terms ...)
                    (for/list ([i (in-range (syntax->datum #'n))])
                      (with-syntax ([i i]) #'(noise i)))])
       #'(glsl (version 330 core)
               (in vec2 vUV) (out vec4 c)
               (define (main) void
                 (set! c (vec4 (+ terms ...) 0.0 1.0)))))]))

(show "宏4  unroll：4 个噪声 octave 展开成直线代码" (unroll 4))
