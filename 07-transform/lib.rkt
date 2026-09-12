#lang racket/base
;; =========================================================
;; lib.rkt —— GL/GLSL 工具库（第一版）
;;
;; 本文件从 03-pipeline 复制：build-program 直接当黑盒用
;;   （编译 + 链接的通用版；03 课 06 步收进 lib，本课先用起来）。
;; 02-triangle/02-vbo.rkt 一步添加：
;;   转发 rename-vector —— 拿到 vec2/vec3/vec4 构造器和 concat-vecs
;;   （顶点数据统一用 vec2 写，不再裸写 f32vector）。
;; 后面每一课会复制本文件，并在需要时往里加功能。
;; =========================================================
;;
;; 本课（07-transform）从 06-primitives 复制，并新增矩阵工具：
;;   前 3 步裸写矩阵，第 4 步（04-lib.rkt）把 mat4-translate / mat4-rot-z /
;;   mat4-scale / mat4-mult / mat4-identity 收进本文件；第 6 步（06-demo.rkt）
;;   把 05-ortho.rkt 裸写的 mat4-ortho 也收进来。
;;   矩阵约定：列主序 mat4 = f32vector[16]（元素 (r行,c列) 存下标 c*4+r），
;;   与 GL 的 uniform mat4 一致；直接上传，零转换。
;; =========================================================

(require opengl ffi/vector)                  ; gl* 常量；s32vector（glShaderSource 用）
(require "../racket-glsl/rewrite.rkt")       ; (glsl ...) 宏 + glsl-pretty
(require "../racket-glsl/rename-vector.rkt") ; vec2/vec3/vec4、concat-vecs、mat4

(provide build-program
         mat4-identity mat4-translate mat4-rot-z mat4-scale mat4-mult mat4-ortho
         (all-from-out "../racket-glsl/rewrite.rkt")        ; (glsl ...) 宏、glsl-pretty
         (all-from-out "../racket-glsl/rename-vector.rkt")) ; vec2…、concat-vecs、mat4…

;; 把一段 GLSL 文本编译成一个"着色器对象"（03 课 03 步裸写讲的原始过程）
;; 取着色器信息日志（编译失败时打印，帮读者定位错误行）
(define (shader-info-log shader)
  (define len (glGetShaderiv shader GL_INFO_LOG_LENGTH))
  (define-values (actual log) (glGetShaderInfoLog shader len))
  (bytes->string/utf-8 log #\? 0 actual))

;; 取程序信息日志（链接失败时打印）
(define (program-info-log prog)
  (define len (glGetProgramiv prog GL_INFO_LOG_LENGTH))
  (define-values (actual log) (glGetProgramInfoLog prog len))
  (bytes->string/utf-8 log #\? 0 actual))

(define (compile-shader type src)
  (define shader (glCreateShader type))
  (glShaderSource shader 1 (vector src) (s32vector (string-length src)))
  (glCompileShader shader)
  ;; ★编译失败：打印 GLSL 报错日志并停在这里（否则只会黑屏、毫无提示）
  (when (zero? (glGetShaderiv shader GL_COMPILE_STATUS))
    (error 'compile-shader "着色器编译失败：\n~a" (shader-info-log shader)))
  shader)

;; 编译 + 链接任意阶段为一个程序（每段 = (阶段类型 源码)）：
;;   (build-program (GL_VERTEX_SHADER vs-src) (GL_FRAGMENT_SHADER fs-src))
;;   (build-program (GL_VERTEX_SHADER vs) (GL_GEOMETRY_SHADER gs) (GL_FRAGMENT_SHADER fs))
;; 阶段类型：GL_VERTEX_SHADER / GL_TESS_CONTROL_SHADER / GL_TESS_EVALUATION_SHADER
;;          / GL_GEOMETRY_SHADER / GL_FRAGMENT_SHADER。
;; （= 03 课讲的 compile-shader + link-program 收成的通用版）
(define (build-program . stages)
  (define prog (glCreateProgram))
  (for ([s stages])
    (glAttachShader prog (compile-shader (car s) (cadr s))))
  (glLinkProgram prog)
  ;; ★链接失败：打印日志并停在这里
  (when (zero? (glGetProgramiv prog GL_LINK_STATUS))
    (error 'build-program "程序链接失败：\n~a" (program-info-log prog)))
  prog)

;; =========================================================
;; mat4-*：4×4 矩阵工具（列主序 mat4 = f32vector[16]，元素 (r行,c列) 存下标 c*4+r）
;; 前 3 步裸写、第 4 步收进这里；第 5 步加 mat4-ortho。
;; 数学用 f32，与 GL 的 float 一致，上传零转换。
;; =========================================================

(define (mat4-identity)
  (mat4 1.0 0.0 0.0 0.0
             0.0 1.0 0.0 0.0
             0.0 0.0 1.0 0.0
             0.0 0.0 0.0 1.0))

(define (mat4-translate tx ty)
  (mat4 1.0 0.0 0.0 0.0
             0.0 1.0 0.0 0.0
             0.0 0.0 1.0 0.0
             tx  ty  0.0 1.0))

(define (mat4-rot-z deg)
  (define r (* (/ (acos -1.0) 180.0) deg))
  (define c (cos r))
  (define s (sin r))
  (mat4 c     s     0.0 0.0
             (- s) c     0.0 0.0
             0.0   0.0   1.0 0.0
             0.0   0.0   0.0 1.0))

(define (mat4-scale sx sy)
  (mat4 sx  0.0 0.0 0.0
             0.0 sy  0.0 0.0
             0.0 0.0 1.0 0.0
             0.0 0.0 0.0 1.0))

;; A·B（先作用 B，再作用 A）
(define (mat4-mult A B)
  (define R (make-f32vector 16 0.0))
  (for* ([c (in-range 4)] [r (in-range 4)] [k (in-range 4)])
    (f32vector-set! R (+ (* 4 c) r)
                    (+ (f32vector-ref R (+ (* 4 c) r))
                       (* (f32vector-ref A (+ (* 4 k) r))
                          (f32vector-ref B (+ (* 4 c) k))))))
  R)

;; 正交投影：把 [l,r]×[b,t]（深度 [n,f]）映射到 NDC。
;; 像素世界（左上原点、y 向下）用 (mat4-ortho 0 w h 0 -1 1)。
(define (mat4-ortho l r b t n f)
  (define rl (- r l)) (define tb (- t b)) (define fn (- f n))
  (mat4 (/ 2.0 rl) 0.0 0.0 0.0
             0.0 (/ 2.0 tb) 0.0 0.0
             0.0 0.0 (/ -2.0 fn) 0.0
             (- (/ (+ r l) rl)) (- (/ (+ t b) tb)) (- (/ (+ f n) fn)) 1.0))
