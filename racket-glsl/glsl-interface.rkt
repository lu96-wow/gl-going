#lang racket/base
;; ============================================================
;; glsl-interface.rkt —— GLSL 类型模型 + 一个 shader 的接口反射
;;
;; 本模块只做两件事，且不做组合（不合并多个 shader、不生成 setter、不碰 GL）：
;;   ① 用一种结构表达**所有** GLSL 类型：
;;        scalar / vector / matrix / sampler / image / array / struct / block / void
;;   ② 把 shader 声明出来的东西（uniform/in/out/const/buffer/shared 及其 struct/block）
;;      表达成统一的「名字 + 类型 + 限定符 + layout」。
;;
;; 统一的两把数据结构：
;;   glsl-type  —— 类型（kind 区分；array 用 elem/len，struct/block 用 fields）
;;   glsl-var   —— 一条声明（接口变量 与 struct/block 成员**共用**同一结构）
;; ============================================================

(require racket/list racket/string racket/match)

(provide
 ;; ---- 类型 ----
 glsl-type? glsl-type
 glsl-type-kind glsl-type-name glsl-type-base glsl-type-dims
 glsl-type-len glsl-type-elem glsl-type-size glsl-type-fields
 glsl-scalar? glsl-vector? glsl-matrix? glsl-sampler? glsl-image?
 glsl-array? glsl-struct? glsl-block? glsl-void?
 ;; 数组/结构/块的构造（尺寸一并算好）
 glsl-array-type glsl-struct-type glsl-block-type
 ;; ---- 声明（接口变量 / 结构成员共用）----
 glsl-var? glsl-var
 glsl-var-name glsl-var-type glsl-var-qualifiers glsl-var-layout
 ;; ---- 接口 ----
 glsl-interface? glsl-interface glsl-interface-vars
 ;; ---- 内建类型 ----
 builtin-glsl-type builtin-glsl-type? builtin-glsl-type-names
 ;; ---- datum 往返（宏展开期 ↔ 运行期）----
 glsl-type->datum datum->glsl-type
 glsl-var->datum datum->glsl-var
 glsl-interface->datum datum->glsl-interface)

;; ============================================================
;; 数据结构
;; ============================================================

;; 一个 GLSL 类型。
;;   kind   'scalar 'vector 'matrix 'sampler 'image 'array 'struct 'block 'void
;;   name   声明里写的类型名符号：'float 'vec4 'mat3 'sampler2D 'Light 'Camera；数组固定 'array
;;   base   元素基类型符号：'float 'int 'uint 'bool 'double；采样器/图像为其采样基类型；其余 #f
;;   dims   向量 (list 分量数)；矩阵 (list 列数 行数)；其余 '()
;;   len    数组长度（整数）或 #f（未定长）；其余 #f
;;   elem   数组元素类型（glsl-type）；其余 #f
;;   size   字节数（自然布局，非 std140）；未知 #f
;;   fields struct/block 的成员（(listof glsl-var)）；其余 '()
(struct glsl-type (kind name base dims len elem size fields) #:transparent)

;; 一条声明：接口变量、struct/block 成员都用它。
;;   name        符号
;;   type        glsl-type
;;   qualifiers  (listof symbol)  如 '(uniform) '(flat out) '(const)
;;   layout      (listof (or symbol (list symbol value)))  如 '((location 0)) '((std140) (binding 0))
(struct glsl-var (name type qualifiers layout) #:transparent)

;; 一段 shader 源码声明的全部接口变量（按源码出现顺序）
(struct glsl-interface (vars) #:transparent)

;; ============================================================
;; 内建类型：名字 → glsl-type
;; ============================================================

(define scalar-names '(float int uint bool double void atomic_uint))
(define vector-names
  (for*/list ([p '("" "i" "u" "b" "d")] [n '(2 3 4)]) (string->symbol (format "~avec~a" p n))))
(define matrix-square-names
  (for*/list ([p '("" "d")] [n '(2 3 4)]) (string->symbol (format "~amat~a" p n))))
(define matrix-rect-names
  (for*/list ([p '("" "d")] [c '(2 3 4)] [r '(2 3 4)]) (string->symbol (format "~amat~ax~a" p c r))))
(define sampler-names
  '(sampler1D sampler2D sampler3D samplerCube
    sampler2DArray samplerCubeArray sampler2DMS samplerBuffer
    sampler1DShadow sampler2DShadow samplerCubeShadow
    sampler2DArrayShadow samplerCubeArrayShadow
    isampler1D isampler2D isampler3D isamplerCube isampler2DArray
    isamplerCubeArray isampler2DMS isamplerBuffer
    usampler1D usampler2D usampler3D usamplerCube usampler2DArray
    usamplerCubeArray usampler2DMS usamplerBuffer))
(define image-names
  '(image1D image2D image3D imageCube image2DArray imageBuffer
    iimage1D iimage2D iimage3D iimageCube iimage2DArray iimageBuffer
    uimage1D uimage2D uimage3D uimageCube uimage2DArray uimageBuffer))

;; 全部内建类型名（重写层用它判断「这个符号是不是类型」）
(define builtin-glsl-type-names
  (append scalar-names vector-names matrix-square-names matrix-rect-names
          sampler-names image-names))

(define (base-component-bytes base) (if (eq? base 'double) 8 4))

(define (base-of-prefix prefix)
  (case prefix [("") 'float] [("i") 'int] [("u") 'uint] [("b") 'bool] [("d") 'double]
    [else #f]))

;; vec4 / ivec3 / uvec2 / bvec4 / dvec4
(define (parse-vector name)
  (define s (symbol->string name))
  (define n (string->number (substring s (sub1 (string-length s)))))
  (define base (base-of-prefix (substring s 0 (- (string-length s) 4))))
  (glsl-type 'vector name base (list n) #f #f (* n (base-component-bytes base)) '()))

;; mat4 / mat3x2 / dmat4 / dmat2x3
(define (parse-matrix name)
  (define s (symbol->string name))
  (define dm? (string-prefix? s "dmat"))
  (define rest (substring s (if dm? 4 3)))
  (define dims
    (if (string-contains? rest "x")
        (map string->number (string-split rest "x"))
        (let ([n (string->number rest)]) (list n n))))
  (define base (if dm? 'double 'float))
  (glsl-type 'matrix name base dims #f #f
             (* (car dims) (cadr dims) (base-component-bytes base)) '()))

(define (sampler-base name)
  (define s (symbol->string name))
  (cond [(string-prefix? s "isampler") 'int]
        [(string-prefix? s "usampler") 'uint]
        [else 'float]))

(define (image-base name)
  (define s (symbol->string name))
  (cond [(string-prefix? s "iimage") 'int]
        [(string-prefix? s "uimage") 'uint]
        [else 'float]))

;; 符号 → glsl-type；不是内建类型返回 #f（可能是 struct 名）
(define (builtin-glsl-type sym)
  (and (symbol? sym)
       (cond
         [(eq? sym 'void) (glsl-type 'void 'void #f '() #f #f #f '())]
         [(eq? sym 'atomic_uint) (glsl-type 'scalar 'atomic_uint 'uint '() #f #f 4 '())]
         [(memq sym '(float int uint bool double))
          (glsl-type 'scalar sym sym '() #f #f (base-component-bytes sym) '())]
         [(memq sym vector-names) (parse-vector sym)]
         [(memq sym matrix-square-names) (parse-matrix sym)]
         [(memq sym matrix-rect-names) (parse-matrix sym)]
         [(memq sym sampler-names) (glsl-type 'sampler sym (sampler-base sym) '() #f #f #f '())]
         [(memq sym image-names) (glsl-type 'image sym (image-base sym) '() #f #f #f '())]
         [else #f])))

(define (builtin-glsl-type? sym) (and (builtin-glsl-type sym) #t))

;; ============================================================
;; 类型谓词
;; ============================================================

(define (glsl-scalar? t) (eq? (glsl-type-kind t) 'scalar))
(define (glsl-vector? t) (eq? (glsl-type-kind t) 'vector))
(define (glsl-matrix? t) (eq? (glsl-type-kind t) 'matrix))
(define (glsl-sampler? t) (eq? (glsl-type-kind t) 'sampler))
(define (glsl-image? t) (eq? (glsl-type-kind t) 'image))
(define (glsl-array? t) (eq? (glsl-type-kind t) 'array))
(define (glsl-struct? t) (eq? (glsl-type-kind t) 'struct))
(define (glsl-block? t) (eq? (glsl-type-kind t) 'block))
(define (glsl-void? t) (eq? (glsl-type-kind t) 'void))

;; ============================================================
;; 数组 / 结构 / 块 的构造（尺寸一并算好）
;; ============================================================

(define (sum-member-size fields)
  (for/fold ([acc 0]) ([f (in-list fields)])
    (define sz (glsl-type-size (glsl-var-type f)))
    (and acc sz (+ acc sz))))

;; 数组：elem 类型 + 长度（#f = 未定长）
(define (glsl-array-type elem len)
  (glsl-type 'array 'array #f '() len elem
             (and len (glsl-type-size elem) (* len (glsl-type-size elem))) '()))

(define (glsl-struct-type name fields)
  (glsl-type 'struct name #f '() #f #f (sum-member-size fields) fields))

(define (glsl-block-type name fields)
  (glsl-type 'block name #f '() #f #f (sum-member-size fields) fields))

;; ============================================================
;; datum 往返：宏展开期算好 → quote 进代码 → 运行期重建
;;   type-datum = (kind name base dims len elem-datum size (var-datum ...))
;;   var-datum  = (name type-datum qualifiers layout)
;;   interface-datum = (listof var-datum)
;; ============================================================

(define (glsl-type->datum t)
  (list (glsl-type-kind t) (glsl-type-name t) (glsl-type-base t) (glsl-type-dims t)
        (glsl-type-len t)
        (and (glsl-type-elem t) (glsl-type->datum (glsl-type-elem t)))
        (glsl-type-size t)
        (map glsl-var->datum (glsl-type-fields t))))

(define (datum->glsl-type d)
  (match d
    [(list kind name base dims len elem size fields)
     (glsl-type kind name base dims len
                (and elem (datum->glsl-type elem)) size
                (map datum->glsl-var fields))]))

(define (glsl-var->datum v)
  (list (glsl-var-name v) (glsl-type->datum (glsl-var-type v))
        (glsl-var-qualifiers v) (glsl-var-layout v)))

(define (datum->glsl-var d)
  (match d
    [(list name type qualifiers layout)
     (glsl-var name (datum->glsl-type type) qualifiers layout)]))

(define (glsl-interface->datum i)
  (map glsl-var->datum (glsl-interface-vars i)))

(define (datum->glsl-interface d)
  (glsl-interface (map datum->glsl-var d)))
