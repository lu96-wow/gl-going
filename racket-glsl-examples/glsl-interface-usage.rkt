#lang racket/base
;; ============================================================
;; glsl-interface 用法示例（纯反射，不需要 GPU/窗口）
;; 运行：racket racket-glsl/glsl-interface-usage.rkt
;;
;; 演示：拿到一个 shader 的类型化接口后，能读出什么、怎么用。
;; 平台只给「类型数据」；下面所有加工都是用户代码。
;; ============================================================

(require racket/list racket/string "../racket-glsl/rewrite.rkt")

(define shader
  (glsl (version 330 core)
        (struct Light (vec3 pos) (float i))
        (layout (location 0) in vec3 aPos)
        (layout (location 1) in vec2 aUV)
        (uniform mat4 uMVP)
        (uniform float uTime)
        (uniform Light uLight)
        (uniform sampler2D uTex)
        (layout (std140) (binding 0) uniform (block Camera (mat4 view) (mat4 proj)) cam)
        (out vec2 vUV)
        (define (main) void
          (set! vUV aUV)
          (set! gl_Position (vec4 aPos 1.0)))))

;; ---------- 入口：两行拿到全部声明 ----------
(define ifc  (glsl-program-interface shader))
(define vars (glsl-interface-vars ifc))

;; ---------- 常用小工具（用户自己写，平台不提供）----------
(define (qual? v q) (and (memq q (glsl-var-qualifiers v)) #t))
(define (layout-ref v key [default #f])
  (or (for/first ([it (in-list (glsl-var-layout v))] #:when (eq? (car it) key)) (cadr it))
      default))
(define (uniforms) (filter (lambda (v) (qual? v 'uniform)) vars))

;; ---------- ① 列出所有 uniform：名字 + 类型 + 字节 ----------
(displayln "== uniforms ==")
(for ([v (in-list (uniforms))])
  (printf "  ~a : ~a  (~a bytes)\n"
          (glsl-var-name v) (glsl-type-name (glsl-var-type v))
          (or (glsl-type-size (glsl-var-type v)) "?")))

;; ---------- ② 顶点属性：location + 名字 + 分量数 ----------
;;   分量数（vec2→2, vec3→3）就是 glVertexAttribPointer 的 size
(displayln "== attributes ==")
(define attrs (for/list ([v (in-list vars)]
                         #:when (and (qual? v 'in) (layout-ref v 'location)))
                v))
(for ([v (in-list (sort attrs < #:key (lambda (v) (layout-ref v 'location))))])
  (define ty (glsl-var-type v))
  (printf "  location ~a : ~a ~a  → size=~a, bytes=~a\n"
          (layout-ref v 'location) (glsl-type-name ty) (glsl-var-name v)
          (car (glsl-type-dims ty)) (glsl-type-size ty)))

;; ---------- ③ 交错布局的 stride / offset：用户自己算 ----------
(displayln "== interleaved layout（用户算，平台不管组合）==")
(let loop ([vs attrs] [off 0])
  (unless (null? vs)
    (define v (car vs))
    (printf "  ~a : offset=~a\n" (glsl-var-name v) off)
    (loop (cdr vs) (+ off (glsl-type-size (glsl-var-type v))))))
(printf "  stride = ~a 字节\n"
        (for/sum ([v (in-list attrs)]) (glsl-type-size (glsl-var-type v))))

;; ---------- ④ 递归看 struct / block 的成员 ----------
(displayln "== struct / block 成员 ==")
(define (show-type t [ind 0])
  (define pad (make-string ind #\space))
  (printf "~a~a ~a (size=~a)\n" pad (glsl-type-kind t) (glsl-type-name t)
          (or (glsl-type-size t) "?"))
  (for ([f (in-list (glsl-type-fields t))])
    (printf "~a  .~a : " pad (glsl-var-name f))
    (show-type (glsl-var-type f) (+ ind 4))))
(for ([v (in-list (uniforms))]
      #:when (memq (glsl-type-kind (glsl-var-type v)) '(struct block)))
  (printf "~a:\n" (glsl-var-name v))
  (show-type (glsl-var-type v) 2))

;; ---------- ⑤ 变成 hash / 表，交给后面的代码 ----------
(displayln "== by-name hash ==")
(define by-name (for/hash ([v (in-list vars)]) (values (glsl-var-name v) v)))
(printf "  uTime → ~a\n" (glsl-type-name (glsl-var-type (hash-ref by-name 'uTime))))

;; 只关心 sampler2D 的 uniform（上传纹理前要用）
(printf "  samplers = ~a\n"
        (for/list ([v (in-list (uniforms))]
                   #:when (eq? (glsl-type-name (glsl-var-type v)) 'sampler2D))
          (glsl-var-name v)))
