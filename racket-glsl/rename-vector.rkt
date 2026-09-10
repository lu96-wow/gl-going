#lang racket/base

;; ============================================================
;; rename-vector.rkt —— GLSL 类型名 ↔ ffi/vector 命名桥
;;
;; 目的：shader 里写 (in vec3 aPos)，CPU 侧也用 vec3 这个名字构造数据；
;;       顺带消掉 glVertexAttribPointer 的 stride/offset 魔法数字。
;;
;; 边界：
;;   - 只做"数据命名桥"，不重实现 GLSL 数学。矩阵计算仍在 lib.rkt 的 m4-*（f64vector）。
;;   - 只覆盖"CPU 有数据表示"的类型：vec/ivec/uvec/bvec/mat。
;;     sampler*/image*/atomic_uint 没有 CPU 向量，不在本模块。
;;   - 标量（float/int/uint/bool）直接就是 Racket 数，不重定义，避免遮蔽 Racket 内置。
;;   - 列主序约定：GLSL mat 与 lib.rkt 的 f64vector[16] 都是列主序，直接转、不转置。
;;   - GLSL bool 在内存是 32 位：bvec 映射到 u32vector（0/1）。
;;
;; 显式输入检查（不隐式转换）：
;;   - float 构造器要求 flonum（如 1.0）；整数/有理数直接报错，由调用方写 1.0。
;;     （依据：ffi/vector 的 f32vector 走 _float ctype，本就不收精确数——
;;      隐式 exact->inexact 反而掩盖了这一点。）
;;   - int/uint 构造器要求精确整数。
;; ============================================================

(require ffi/vector)

(provide (all-from-out ffi/vector)
         ;; 向量构造器（元数由 Racket 自身 arity 保证）
         vec2 vec3 vec4
         ivec2 ivec3 ivec4
         uvec2 uvec3 uvec4
         bvec2 bvec3 bvec4
         ;; 矩阵构造器（全形式分派）
         mat2 mat3 mat4
         ;; 拼装：把多个 vec（f32vector）连成一个连续缓冲
         concat-vecs concat-vecs!
         ;; vec：n 个同型向量的缓冲（静态/动态顶点数据）
         vec make-vec vec? vec-count vec-width vec-ref vec-set! vec->f32vector
         ;; 尺寸 / 步长帮助
         glsl-size glsl-byte-size glsl-stride glsl-stride-bytes glsl-type-table)

;; ---------- 输入检查 ----------

(define (check-float who x)
  (unless (flonum? x)
    (error who "参数应为浮点（如 1.0，不要整数/有理数），实际 ~s" x))
  x)

(define (check-int who x)
  (unless (and (exact? x) (integer? x))
    (error who "参数应为整数，实际 ~s" x))
  x)

;; ---------- 向量 ----------

(define (vec2 x y) (f32vector (check-float 'vec2 x) (check-float 'vec2 y)))
(define (vec3 x y z) (f32vector (check-float 'vec3 x) (check-float 'vec3 y) (check-float 'vec3 z)))
(define (vec4 x y z w) (f32vector (check-float 'vec4 x) (check-float 'vec4 y) (check-float 'vec4 z) (check-float 'vec4 w)))

(define (ivec2 x y) (s32vector (check-int 'ivec2 x) (check-int 'ivec2 y)))
(define (ivec3 x y z) (s32vector (check-int 'ivec3 x) (check-int 'ivec3 y) (check-int 'ivec3 z)))
(define (ivec4 x y z w) (s32vector (check-int 'ivec4 x) (check-int 'ivec4 y) (check-int 'ivec4 z) (check-int 'ivec4 w)))

(define (uvec2 x y) (u32vector (check-int 'uvec2 x) (check-int 'uvec2 y)))
(define (uvec3 x y z) (u32vector (check-int 'uvec3 x) (check-int 'uvec3 y) (check-int 'uvec3 z)))
(define (uvec4 x y z w) (u32vector (check-int 'uvec4 x) (check-int 'uvec4 y) (check-int 'uvec4 z) (check-int 'uvec4 w)))

;; bool → 0/1：#f 或 0 → 0，其余（#t / 非零数）→ 1
(define (->bool x)
  (cond
    [(boolean? x) (if x 1 0)]
    [(number? x) (if (zero? x) 0 1)]
    [else (error 'bvec "元素应为 #t/#f 或数字，实际 ~s" x)]))

(define (bvec2 x y) (u32vector (->bool x) (->bool y)))
(define (bvec3 x y z) (u32vector (->bool x) (->bool y) (->bool z)))
(define (bvec4 x y z w) (u32vector (->bool x) (->bool y) (->bool z) (->bool w)))

;; ---------- 拼装 ----------

;; 把一串 vec（f32vector）依次写进 dst（原地，从下标 0 开始），返回写入的元素数。
;; 给"每帧重新生成顶点数据"的动态场景：dst 预分配一次、每帧覆写，零额外分配。
;;   (define buf (make-f32vector MAX 0.0))            ; init 时分配一次
;;   (define n (concat-vecs! buf (list (vec2 ...) ...)))  ; 每帧覆写，n = 元素数
;;   (glBufferSubData GL_ARRAY_BUFFER 0 (* 4 n) buf)   ; 只更新 GPU，不重分配
(define (concat-vecs! dst vs)
  (define i 0)
  (for ([v vs])
    (for ([j (in-range (f32vector-length v))])
      (f32vector-set! dst i (f32vector-ref v j))
      (set! i (add1 i))))
  i)

;; 把多个 vec（f32vector）连成一个**新**连续缓冲（静态数据：拼一次即可）。
;; 例：(concat-vecs (vec2 -0.5 -0.5) (vec2 0.5 -0.5) (vec2 0.0 0.5))
;;     → (f32vector -0.5 -0.5 0.5 -0.5 0.0 0.5)
;; 只分配输出这一块，不经过 list/apply（无参数个数上限）。
;; 注：同宽度的向量请优先用 vec（见下）；本函数主要留给"混合宽度"的交错数据。
(define (concat-vecs . vs)
  (define total (for/sum ([v vs]) (f32vector-length v)))
  (define out (make-f32vector total 0.0))
  (concat-vecs! out vs)
  out)

;; ---------- vec：n 个同型向量的缓冲 ----------

;; 内部结构：一个 f32vector + 每个 vec 的宽度（分量数）。
;; 对应 GLSL 的 vec2[N]/vec3[N]（同宽度向量数组），是"动态数量顶点"的 CPU 形状。
;; （结构名用 gl-vec，把 vec 留给公开构造器；#:transparent 便于打印调试）
(struct gl-vec (data width) #:transparent)

(define vec? gl-vec?)
(define vec-width gl-vec-width)

;; 静态构造：(vec (vec2 ...) (vec2 ...) ...) —— 若干同型 vec，宽度取第一个、校验其余。
;; 个数 = 你列了几个 vec（不用手写 num）。
(define (vec . vs)
  (unless (pair? vs)
    (error 'vec "至少给一个 vec，如 (vec (vec2 0.0 0.0))"))
  (define w (f32vector-length (car vs)))
  (for ([v (cdr vs)])
    (unless (= (f32vector-length v) w)
      (error 'vec "所有 vec 宽度须一致，实际 ~a 与 ~a" w (f32vector-length v))))
  (define data (make-f32vector (* w (length vs)) 0.0))
  (concat-vecs! data vs)
  (gl-vec data w))

;; 动态构造：(make-vec 1000 (vec3 0.0 0.0 0.0)) —— 预分配 n 个同型 vec（都填 template）。
;; 之后用 vec-set! 逐帧原地覆写，零分配。
(define (make-vec n template)
  (unless (and (exact? n) (integer? n) (>= n 0))
    (error 'make-vec "n 应为非负整数，实际 ~s" n))
  (define w (f32vector-length template))
  (define data (make-f32vector (* n w) 0.0))
  (for ([i (in-range n)])
    (for ([j (in-range w)])
      (f32vector-set! data (+ (* i w) j) (f32vector-ref template j))))
  (gl-vec data w))

;; 有多少个 vec
(define (vec-count v) (quotient (f32vector-length (gl-vec-data v)) (gl-vec-width v)))

;; 函数式读：返回第 i 个 vec（一个新 f32vector，宽度个分量）
(define (vec-ref v i)
  (define w (gl-vec-width v))
  (define data (gl-vec-data v))
  (define out (make-f32vector w 0.0))
  (for ([j (in-range w)])
    (f32vector-set! out j (f32vector-ref data (+ (* i w) j))))
  out)

;; set! 式写：把第 i 个 vec 原地覆写为 w（零分配；w 须同宽）
(define (vec-set! v i w)
  (define width (gl-vec-width v))
  (unless (= (f32vector-length w) width)
    (error 'vec-set! "宽度不匹配：期望 ~a，实际 ~a" width (f32vector-length w)))
  (define data (gl-vec-data v))
  (for ([j (in-range width)])
    (f32vector-set! data (+ (* i width) j) (f32vector-ref w j))))

;; 上传：底层 f32vector（零拷贝）
(define (vec->f32vector v) (gl-vec-data v))

;; ---------- 矩阵（通用分派） ----------

;; 列向量 → list（f32vector/f64vector/list，长度须为 n）
(define (col->list who c n)
  (cond
    [(f32vector? c)
     (unless (= (f32vector-length c) n)
       (error who "列向量长度应为 ~a，实际 ~a" n (f32vector-length c)))
     (f32vector->list c)]
    [(f64vector? c)
     (unless (= (f64vector-length c) n)
       (error who "列向量长度应为 ~a，实际 ~a" n (f64vector-length c)))
     (f64vector->list c)]
    [(list? c)
     (unless (= (length c) n)
       (error who "列向量长度应为 ~a，实际 ~a" n (length c)))
     (map (lambda (x) (check-float who x)) c)]
    [else (error who "列应为 f32vector/f64vector/list，实际 ~s" c)]))

;; 对角矩阵（列主序展开）
(define (diag-list who n x)
  (define f (check-float who x))
  (for*/list ([c (in-range n)] [r (in-range n)])
    (if (= r c) f 0.0)))

;; 通用矩阵构造：who=mat2/mat3/mat4，n=2/3/4，args=构造参数
;;   (mat 1 标量)         → 对角
;;   (mat f32/f64[n*n])   → 拷贝 / f64→f32
;;   (mat n 个列向量)      → 拼列
;;   (mat n*n 个标量)      → 列主序
;;   零元或其它 → 报错（对齐 GLSL）
(define (make-mat who n args)
  (define total (* n n))
  (cond
    [(null? args)
     (error who "mat~a() 在 GLSL 中是未初始化，不支持零参数构造" n)]
    [(= (length args) 1)
     (define x (car args))
     (cond
       [(number? x) (apply f32vector (diag-list who n x))]
       [(and (f32vector? x) (= (f32vector-length x) total))
        (apply f32vector (f32vector->list x))]
       [(and (f64vector? x) (= (f64vector-length x) total))
        (apply f32vector (f64vector->list x))]
       [else (error who "mat~a 单参数应为标量或长度 ~a 的 f32/f64 向量，实际 ~s" n total x)])]
    [(= (length args) n)
     (apply f32vector (apply append (map (lambda (c) (col->list who c n)) args)))]
    [(= (length args) total)
     (apply f32vector (map (lambda (x) (check-float who x)) args))]
    [else
     (error who "mat~a 参数个数应为 1（对角/拷贝）、~a（列向量）或 ~a（标量），实际 ~a"
            n n total (length args))]))

(define (mat2 . args) (make-mat 'mat2 2 args))
(define (mat3 . args) (make-mat 'mat3 3 args))
(define (mat4 . args) (make-mat 'mat4 4 args))

;; ---------- 尺寸 / 步长 ----------

;; GLSL 类型名 → 元素数（CPU 有数据表示的类型）。
;; 标量也算 1 个元素（float/int/uint/bool 都是 4 字节），
;; 这样 glsl-stride-bytes 能算含标量的交错布局（如 pos+color+float）。
(define glsl-type-table
  '((float . 1) (int . 1) (uint . 1) (bool . 1)
    (vec2 . 2)  (vec3 . 3)  (vec4 . 4)
    (ivec2 . 2) (ivec3 . 3) (ivec4 . 4)
    (uvec2 . 2) (uvec3 . 3) (uvec4 . 4)
    (bvec2 . 2) (bvec3 . 3) (bvec4 . 4)
    (mat2 . 4)  (mat3 . 9)  (mat4 . 16)))

(define (glsl-size t)
  (define e (assq t glsl-type-table))
  (unless e (error 'glsl-size "未知 GLSL 类型（或无可表示的 CPU 类型）：~s" t))
  (cdr e))

;; 元素数 × 4（float/int/uint/bool 都是 4 字节）
(define (glsl-byte-size t) (* 4 (glsl-size t)))

;; 交错属性总元素数（stride 用；字节 = (* 4 (glsl-stride ...))）
(define (glsl-stride . types)
  (apply + (map glsl-size types)))

;; 交错属性的字节步长：若干类型依次排开后的总字节数。
;; 用法（glVertexAttribPointer 的 stride/offset）：
;;   stride = (glsl-stride-bytes 'vec3 'vec3)      ; 位置+法线交错 = 24 字节
;;   offset = (glsl-stride-bytes 'vec3)            ; 第 2 个属性跳过前面 12 字节
;; 同一函数既算"步长"也算"前缀偏移"，消除手写字节魔法数字。
(define (glsl-stride-bytes . types)
  (* 4 (apply glsl-stride types)))
