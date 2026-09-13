#lang racket/base
;; =========================================================
;; 05-glsl-types/04-literals-ops.rkt —— 第四步：字面量纪律 + 运算符
;; 运行：racket 05-glsl-types/04-literals-ops.rkt    点 X = 退出
;; =========================================================

;; 上一步：会用 swizzle 重排分量。本步补上两个"写表达式"时最容易踩的坑。
;;
;; 本步新增（2 组，同属"写表达式"这一件事）：
;;   ① 字面量纪律 —— 浮点必须带小数点；★★ 整数除法 1/2 = 0，不是 0.5！★★
;;   ② 运算符 —— 算术(+ - * /)逐分量；比较(< > = !=)产出 bool；三元 if 选值
;;
;; ★字面量纪律（这是 GLSL 新手第一大坑）：
;;   1.0 / 0.5 / -2.0    是 float；  1 / 2 / -2  是 int。
;;   float 和 int 是两种类型。整数相除做的是"整除"：
;;      1 / 2     → 0   （int ÷ int，结果还是 int，小数部分被丢掉）
;;      1.0 / 2.0 → 0.5 （float ÷ float）
;;   所以 GLSL 里凡是"小数"，字面量一定要写小数点。
;;
;; ★运算符（都按"分量"逐个算，这正是向量类型的好处）：
;;   (+ a b) (- a b) (* a b) (/ a b)   逐分量加减乘除
;;   (< a b) (> a b) (= a b) (!= a b)  逐分量比较 → bool
;;     ★注意：DSL 里相等写 (= a b)，展开后才是 GLSL 的 a == b（= 是赋值，== 才是比较）
;;   (and a b) (or a b) (not a)        逻辑（bool 上的运算）
;;   三元（表达式版的"如果"，是运算符不是语句）：
;;     (if 条件 真值 假值)   →  条件 ? 真值 : 假值
;;   语句版的 if/for 等控制流留到后面的控制流课——本课只用表达式。
;;
;; 本步视觉（一次把上面全展示）：
;;   红 = 1/2      → 整数除法 = 0，所以整个画面**没有红色**（若误以为 0.5 会偏橙）
;;   绿 = 1.0/2.0  → 0.5，整幅画面有一层中等绿
;;   蓝 = 三元判断 → 右半边(p.x>0)=1.0 亮蓝，左半边=0.0 黑
;;   结果：左半边纯绿，右半边绿+蓝=青——中间一条竖直分界线就是三元在切。

(require "../02-triangle/04-gui-tool.rkt")   ; make-window（带视口）
(require "../racket-glsl/rewrite.rkt")        ; (glsl ...) 宏 + glsl-program-src
(require "../racket-glsl/rename-vector.rkt")  ; vec / vec4 / glsl-stride-bytes
(require "../racket-glsl/tool.rkt")           ; build-program / use-program

(define vert-src
  (glsl (version 330 core)
        (layout (location 0) in vec2 aPos)
        (layout (location 1) in vec2 aUV)
        (out vec2 vUV)
        (define (main) void
          (set! vUV aUV)
          (set! gl_Position (vec4 aPos 0.0 1.0)))))

;; 片元着色器（本步主角，逐行读）：
;;   (vec2 p (- (* vUV 2.0) 1.0))   算术逐分量：u,v 都 *2 再 -1 → p 落在 -1..1
;;   (float side (if (> (x p) 0.0) 1.0 0.0))
;;       (> ...) 比较 p.x>0 → bool；三元按 bool 选 1.0 或 0.0（右亮左黑）
;;   (vec4 (/ 1 2) (/ 1.0 2.0) side 1.0)
;;       红=1/2=0；绿=1.0/2.0=0.5；蓝=side
(define frag-src
  (glsl (version 330 core)
        (in vec2 vUV)
        (out vec4 FragColor)
        (define (main) void
          (vec2 p (- (* vUV 2.0) 1.0))
          (float side (if (> (x p) 0.0) 1.0 0.0))
          (set! FragColor (vec4 (/ 1 2) (/ 1.0 2.0) side 1.0)))))

(printf "片元着色器展开为：\n~a\n" (glsl-program-src frag-src))

(define verts
  (vec (vec4 -1.0 -1.0  0.0 0.0)
       (vec4  1.0 -1.0  1.0 0.0)
       (vec4  1.0  1.0  1.0 1.0)
       (vec4 -1.0 -1.0  0.0 0.0)
       (vec4  1.0  1.0  1.0 1.0)
       (vec4 -1.0  1.0  0.0 1.0)))

(define (draw)
  (gl-clear-color 0.10 0.12 0.20 1.0)
  (gl-clear gl-color-buffer-bit)
  (use-program prog)
  (gl-bind-vertex-array vao)
  (gl-draw-arrays gl-triangles 0 6))

(define-values (frame canvas)
  (make-window #:title "05-04 字面量与运算符" #:width 400 #:height 400 #:draw draw))

(define prog
  (send canvas with-gl-context
    (lambda () (build-program (gl-vertex-shader vert-src) (gl-fragment-shader frag-src)))))
(define vao
  (send canvas with-gl-context
    (lambda ()
      (define data (vec->f32vector verts))
      (define vbo (u32vector-ref (gl-gen-buffers 1) 0))
      (gl-bind-buffer gl-array-buffer vbo)
      (gl-buffer-data gl-array-buffer (gl-vector-sizeof data) data gl-static-draw)
      (define v (u32vector-ref (gl-gen-vertex-arrays 1) 0))
      (gl-bind-vertex-array v)
      (gl-vertex-attrib-pointer 0 2 gl-float #f (glsl-stride-bytes 'vec2 'vec2) 0)
      (gl-enable-vertex-attrib-array 0)
      (gl-vertex-attrib-pointer 1 2 gl-float #f
                                (glsl-stride-bytes 'vec2 'vec2)
                                (glsl-stride-bytes 'vec2))
      (gl-enable-vertex-attrib-array 1)
      (gl-bind-vertex-array 0)
      v)))

(send frame show #t)
