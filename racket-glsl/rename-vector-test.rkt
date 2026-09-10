#lang racket/base
;; 运行：racket racket-glsl/rename-vector-test.rkt
(require rackunit
         "rename-vector.rkt")

;; ffi vector 的 equal? 不比较内容，统一用 ->list 比
(define (lv v) (f32vector->list v))
(define (li v) (s32vector->list v))
(define (lu v) (u32vector->list v))

;; ---------- 向量构造 + 显式输入检查 ----------
(check-equal? (lv (vec3 1.0 2.0 3.0)) '(1.0 2.0 3.0))
(check-exn exn:fail? (lambda () (vec2 1 2)))            ; 整数 → 报错（不隐式转）
(check-exn exn:fail? (lambda () (vec3 1.0 2.0)))        ; 元数不对报错

(check-equal? (li (ivec3 1 2 3)) '(1 2 3))
(check-exn exn:fail? (lambda () (ivec2 1.0 2.0)))       ; 非整数 → 报错
(check-equal? (lu (uvec2 1 2)) '(1 2))

;; bvec：#t/#f 和数字都收，0/#f→0，非 0/#t→1
(check-equal? (lu (bvec3 #t #f #t)) '(1 0 1))
(check-equal? (lu (bvec2 0 5)) '(0 1))

;; ---------- 拼装 ----------
(check-equal? (lv (concat-vecs (vec2 -0.5 -0.5) (vec2 0.5 -0.5) (vec2 0.0 0.5)))
              '(-0.5 -0.5 0.5 -0.5 0.0 0.5))
(check-equal? (lv (concat-vecs)) '())
;; 原地拼装：写进预分配 dst，返回元素数
(define dst (make-f32vector 10 0.0))
(check-equal? (concat-vecs! dst (list (vec2 1.0 2.0) (vec3 3.0 4.0 5.0))) 5)
(check-equal? (lv dst) '(1.0 2.0 3.0 4.0 5.0 0.0 0.0 0.0 0.0 0.0))

;; ---------- vec：n 个同型向量 ----------
;; 静态构造 + 访问
(define vn (vec (vec2 -0.5 -0.5) (vec2 0.5 -0.5) (vec2 0.0 0.5)))
(check-true (vec? vn))
(check-equal? (vec-count vn) 3)
(check-equal? (vec-width vn) 2)
(check-equal? (lv (vec->f32vector vn)) '(-0.5 -0.5 0.5 -0.5 0.0 0.5))
(check-equal? (lv (vec-ref vn 1)) '(0.5 -0.5))        ; 函数式读：新切片
;; set! 式写（原地，零分配）
(vec-set! vn 1 (vec2 9.0 9.0))
(check-equal? (lv (vec->f32vector vn)) '(-0.5 -0.5 9.0 9.0 0.0 0.5))
;; 动态构造：预分配 n 个，填 template
(define dn (make-vec 1000 (vec3 0.0 0.0 0.0)))
(check-equal? (vec-count dn) 1000)
(check-equal? (vec-width dn) 3)
(check-equal? (lv (vec-ref dn 999)) '(0.0 0.0 0.0))
(vec-set! dn 0 (vec3 1.0 2.0 3.0))
(check-equal? (lv (vec-ref dn 0)) '(1.0 2.0 3.0))
;; 报错：空 / 宽度不一致 / set! 宽度不匹配
(check-exn exn:fail? (lambda () (vec)))
(check-exn exn:fail? (lambda () (vec (vec2 1.0 2.0) (vec3 1.0 2.0 3.0))))
(check-exn exn:fail? (lambda () (vec-set! vn 0 (vec3 1.0 2.0 3.0))))

;; ---------- mat：对角 ----------
(check-equal? (lv (mat2 1.0)) '(1.0 0.0 0.0 1.0))
(check-equal? (lv (mat4 2.0))
              '(2.0 0.0 0.0 0.0
                0.0 2.0 0.0 0.0
                0.0 0.0 2.0 0.0
                0.0 0.0 0.0 2.0))

;; ---------- mat：拷贝 / f64→f32 ----------
(check-equal? (lv (mat4 (f32vector 1.0 0.0 0.0 0.0  0.0 1.0 0.0 0.0  0.0 0.0 1.0 0.0  0.0 0.0 0.0 1.0)))
              '(1.0 0.0 0.0 0.0  0.0 1.0 0.0 0.0  0.0 0.0 1.0 0.0  0.0 0.0 0.0 1.0))
(check-equal? (lv (mat4 (f64vector 1.0 0.0 0.0 0.0  0.0 1.0 0.0 0.0  0.0 0.0 1.0 0.0  0.0 0.0 0.0 1.0)))
              '(1.0 0.0 0.0 0.0  0.0 1.0 0.0 0.0  0.0 0.0 1.0 0.0  0.0 0.0 0.0 1.0))

;; ---------- mat：列向量 ----------
(check-equal? (lv (mat2 (vec2 1.0 0.0) (vec2 0.0 1.0)))
              '(1.0 0.0 0.0 1.0))
(check-equal? (lv (mat4 (vec4 1.0 0.0 0.0 0.0) (vec4 0.0 1.0 0.0 0.0)
                        (vec4 0.0 0.0 1.0 0.0) (vec4 0.0 0.0 0.0 1.0)))
              '(1.0 0.0 0.0 0.0  0.0 1.0 0.0 0.0  0.0 0.0 1.0 0.0  0.0 0.0 0.0 1.0))

;; ---------- mat：16 标量（列主序） ----------
(check-equal? (lv (mat4 1.0 0.0 0.0 0.0  0.0 1.0 0.0 0.0  0.0 0.0 1.0 0.0  0.0 0.0 0.0 1.0))
              '(1.0 0.0 0.0 0.0  0.0 1.0 0.0 0.0  0.0 0.0 1.0 0.0  0.0 0.0 0.0 1.0))

;; ---------- mat：非法形式报错 ----------
(check-exn exn:fail? (lambda () (mat4)))                              ; 零元
(check-exn exn:fail? (lambda () (mat4 1.0 2.0 3.0 4.0)))             ; 4 标量（GLSL 也非法）
(check-exn exn:fail? (lambda () (mat4 1 0 0 0  0 1 0 0  0 0 1 0  0 0 0 1))) ; 整数标量
(check-exn exn:fail? (lambda () (mat4 (f32vector 1.0 2.0 3.0))))     ; 长度错

;; ---------- 尺寸 / 步长 ----------
(check-equal? (glsl-size 'vec3) 3)
(check-equal? (glsl-byte-size 'vec3) 12)
(check-equal? (glsl-stride 'vec3 'vec2) 5)
(check-exn exn:fail? (lambda () (glsl-size 'sampler2D)))    ; 无 CPU 表示

(displayln "rename-vector 全部测试通过")
