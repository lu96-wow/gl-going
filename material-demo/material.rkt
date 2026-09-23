#lang racket/base
;; ============================================================
;; material.rkt —— 把「program + 它的 uniform」绑成一个对象（真实 GL 版）
;;
;; 它把三件“手动”变成“自动”：
;;   ① CPU 手动写 uniform 名字          → 从 shader 源码的声明里自动读出
;;   ② GPU 手动 glGetUniformLocation    → 建材质时一次性自动查好，藏进对象
;;   ③ 手动挑 glUniform1f/2f/4f/Matrix… → 按 shader 声明的类型自动分派
;;
;; 必须在 with-gl-context（GL 上下文当前）里调用。
;; ============================================================

(require racket/list racket/string opengl
         "../racket-glsl/tool.rkt"          ; build-program/list
         "../racket-glsl/glsl-program.rkt") ; glsl-program-forms / glsl-form-text

(provide material? material make-material
         material-use! material-set! material-replay! with-material
         material-uniforms shader-uniform-decls)

;; name=材质名 program=链接好的 GL program
;; uniforms=名字→类型  locs=名字→location（建材质时查好）  saved=名字→最近的值（可 replay）
(struct material (name program uniforms locs saved) #:transparent)

;; ---------- ① 从 shader 的 form 里自动读 uniform 声明 ----------
;; (uniform vec4 uColor)                        → ("uColor" . "vec4")
;; (layout (location 0) uniform sampler2D uTex) → ("uTex" . "sampler2D")
;; UBO block：(uniform (block Camera …) cam) → 跳过（成员得用 "cam.view"，暂不支持）
(define (form-uniform-decl d)
  (and (pair? d)
       (let ([i (index-of d 'uniform)])
         (and i (< (+ i 2) (length d))
              (let ([ty (list-ref d (add1 i))]
                    [nm (list-ref d (+ i 2))])
                (and (symbol? ty) (symbol? nm)
                     (cons (symbol->string nm) (symbol->string ty))))))))

(define (shader-uniform-decls prog)
  (for/list ([f (glsl-program-forms prog)]
             #:when (form-uniform-decl (glsl-form-text f)))
    (form-uniform-decl (glsl-form-text f))))

;; ---------- ② 建材质：编译链接 + 自动查 loc ----------
;; stages = (list (list 阶段类型 glsl-program) ...)，和 build-program/list 同形
(define (make-material name stages)
  (define glsls (map cadr stages))
  (define program (build-program/list stages))
  (define decls (append-map shader-uniform-decls glsls))
  (define uniforms (for/hash ([d (in-list decls)]) (values (car d) (cdr d))))
  (define locs (for/hash ([(nm _) (in-hash uniforms)])
                 (values nm (glGetUniformLocation program nm))))
  (material name program uniforms locs (make-hash)))

(define (material-use! m) (glUseProgram (material-program m)))

;; ---------- ③ 自动分派：按 shader 声明的类型挑 glUniform* ----------
;; 名字可以写符号 'uOffset 或字符串 "uOffset"；不属于这个材质 → 立刻报错
;; （而不是像裸 GL 那样静默忽略）
(define (material-set! m u v)
  (define key (if (symbol? u) (symbol->string u) u))
  (define ty (hash-ref (material-uniforms m) key
                       (lambda ()
                         (error 'material-set!
                                "材质 `~a' 没有 uniform `~a'（它有的是：~a）"
                                (material-name m) key
                                (string-join (sort (hash-keys (material-uniforms m)) string<?) ", ")))))
  (define loc (hash-ref (material-locs m) key))
  (define (f x) (real->double-flonum x))
  (cond
    [(string=? ty "float")     (glUniform1f loc (f v))]
    [(string=? ty "int")       (glUniform1i loc (inexact->exact v))]
    [(string=? ty "bool")      (glUniform1i loc (if v 1 0))]
    [(string=? ty "vec2")      (apply glUniform2f loc (map f v))]
    [(string=? ty "vec3")      (apply glUniform3f loc (map f v))]
    [(string=? ty "vec4")      (apply glUniform4f loc (map f v))]
    [(string=? ty "mat4")      (glUniformMatrix4fv loc 1 #f v)]   ; v = f32vector[16]
    [(string=? ty "sampler2D") (glUniform1i loc v)]
    [else (error 'material-set! "暂不支持的 uniform 类型 ~a（~a）" ty key)])
  (hash-set! (material-saved m) key v))

;; 把上次设过的值重放一遍（切回来时省事）
(define (material-replay! m)
  (for ([(u v) (in-hash (material-saved m))]) (material-set! m u v)))

;; 作用域：切 program，跑 body
(define-syntax-rule (with-material m body ...)
  (begin (material-use! m) body ...))
