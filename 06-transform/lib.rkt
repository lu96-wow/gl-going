#lang racket/base
;; =========================================================
;; lib.rkt —— GL/GLSL 工具库（第一版）
;;
;; 本文件在 02-triangle/03-build-program.rkt 一步创建：
;;   把 02-shader-load.rkt 裸写的 7 个 gl* 调用收成 build-program。
;; 02-triangle/04-vbo.rkt 一步添加：
;;   转发 rename-vector —— 拿到 vec2/vec3/vec4 构造器和 concat-vecs
;;   （顶点数据统一用 vec2 写，不再裸写 f32vector）。
;; 后面每一课会复制本文件，并在需要时往里加功能。
;; =========================================================
;;
;; 本课（06-transform）从 05-primitives 复制，并新增矩阵工具：
;;   前 3 步裸写矩阵，第 4 步（04-lib.rkt）把 m4-translate / m4-rot-z /
;;   m4-scale / m4-mult / m4-identity 收进本文件；第 6 步（06-demo.rkt）
;;   把 05-ortho.rkt 裸写的 m4-ortho 也收进来。
;;   矩阵约定：列主序 f64vector[16]（元素 (r行,c列) 存下标 c*4+r），
;;   与 GL 的 uniform mat4 一致；上传时用 rename-vector 的 (mat4 ...) 转 f32。
;; =========================================================

(require opengl ffi/vector)                  ; gl* 常量；s32vector（glShaderSource 用）
(require "../racket-glsl/rewrite.rkt")       ; (glsl ...) 宏 + glsl-pretty
(require "../racket-glsl/rename-vector.rkt") ; vec2/vec3/vec4、concat-vecs、mat4

(provide build-program
         m4-identity m4-translate m4-rot-z m4-scale m4-mult m4-ortho
         (all-from-out "../racket-glsl/rewrite.rkt")        ; (glsl ...) 宏、glsl-pretty
         (all-from-out "../racket-glsl/rename-vector.rkt")) ; vec2…、concat-vecs、mat4…

;; 把一段 GLSL 文本编译成一个"着色器对象"（02-shader-load.rkt 讲的原始过程）
(define (compile-shader type src)
  (define shader (glCreateShader type))
  (glShaderSource shader 1 (vector src) (s32vector (string-length src)))
  (glCompileShader shader)
  shader)

;; 编译 + 链接两段着色器，返回程序编号
;; （= 02-shader-load.rkt 的 compile-shader + link-program 收成的函数）
(define (build-program vs-src fs-src)
  (define vs (compile-shader GL_VERTEX_SHADER vs-src))
  (define fs (compile-shader GL_FRAGMENT_SHADER fs-src))
  (define prog (glCreateProgram))
  (glAttachShader prog vs)
  (glAttachShader prog fs)
  (glLinkProgram prog)
  prog)

;; =========================================================
;; m4-*：4×4 矩阵工具（列主序 f64vector[16]，元素 (r行,c列) 存下标 c*4+r）
;; 前 3 步裸写、第 4 步收进这里；第 5 步加 m4-ortho。
;; 数学用 f64 保证精度，上传前用 rename-vector 的 (mat4 ...) 转 f32。
;; =========================================================

(define (m4-identity)
  (f64vector 1.0 0.0 0.0 0.0
             0.0 1.0 0.0 0.0
             0.0 0.0 1.0 0.0
             0.0 0.0 0.0 1.0))

(define (m4-translate tx ty)
  (f64vector 1.0 0.0 0.0 0.0
             0.0 1.0 0.0 0.0
             0.0 0.0 1.0 0.0
             tx  ty  0.0 1.0))

(define (m4-rot-z deg)
  (define r (* (/ (acos -1.0) 180.0) deg))
  (define c (cos r))
  (define s (sin r))
  (f64vector c     s     0.0 0.0
             (- s) c     0.0 0.0
             0.0   0.0   1.0 0.0
             0.0   0.0   0.0 1.0))

(define (m4-scale sx sy)
  (f64vector sx  0.0 0.0 0.0
             0.0 sy  0.0 0.0
             0.0 0.0 1.0 0.0
             0.0 0.0 0.0 1.0))

;; A·B（先作用 B，再作用 A）
(define (m4-mult A B)
  (define R (make-f64vector 16 0.0))
  (for* ([c (in-range 4)] [r (in-range 4)] [k (in-range 4)])
    (f64vector-set! R (+ (* 4 c) r)
                    (+ (f64vector-ref R (+ (* 4 c) r))
                       (* (f64vector-ref A (+ (* 4 k) r))
                          (f64vector-ref B (+ (* 4 c) k))))))
  R)

;; 正交投影：把 [l,r]×[b,t]（深度 [n,f]）映射到 NDC。
;; 像素世界（左上原点、y 向下）用 (m4-ortho 0 w h 0 -1 1)。
(define (m4-ortho l r b t n f)
  (define rl (- r l)) (define tb (- t b)) (define fn (- f n))
  (f64vector (/ 2.0 rl) 0.0 0.0 0.0
             0.0 (/ 2.0 tb) 0.0 0.0
             0.0 0.0 (/ -2.0 fn) 0.0
             (- (/ (+ r l) rl)) (- (/ (+ t b) tb)) (- (/ (+ f n) fn)) 1.0))
