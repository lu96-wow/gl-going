#lang racket/base
;; =========================================================
;; lib.rkt —— GL/GLSL 工具库（第一版）
;;
;; 本文件从 02-triangle 复制；本课 03/04/06 步把 build-program 拆开重写：
;;   03 步裸写 compile-shader，04 步裸写 link-program，06 步收成“任意阶段”通用版
;;   （每段 = (阶段类型 源码)，一次可编译链接 顶点+细分+几何+片元 任意组合）。
;; 其余（shader-info-log / program-info-log）与 02 相同。
;; =========================================================

(require opengl ffi/vector)                  ; gl* 常量；s32vector（glShaderSource 用）
(require "../../racket-glsl/rewrite.rkt")       ; (glsl ...) 宏 + glsl-pretty
(require "../../racket-glsl/rename-vector.rkt") ; vec2/vec3/vec4、concat-vecs

(provide build-program
         (all-from-out "../../racket-glsl/rewrite.rkt")        ; (glsl ...) 宏、glsl-pretty
         (all-from-out "../../racket-glsl/rename-vector.rkt")) ; vec2…、concat-vecs、f32vector…

;; 把一段 GLSL 文本编译成一个"着色器对象"（本课 03 步裸写讲的原始过程）
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
