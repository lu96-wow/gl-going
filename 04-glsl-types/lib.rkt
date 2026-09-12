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
;; 本课（04-glsl-types）从 03-pipeline 原样复制，未改动：
;;   本课是纯着色器语言课，不引入新 GL API，build-program / vec2… 继续用。
;; =========================================================

(require opengl ffi/vector)                  ; gl* 常量；s32vector（glShaderSource 用）
(require "../racket-glsl/rewrite.rkt")       ; (glsl ...) 宏 + glsl-pretty
(require "../racket-glsl/rename-vector.rkt") ; vec2/vec3/vec4、concat-vecs

(provide build-program
         (all-from-out "../racket-glsl/rewrite.rkt")        ; (glsl ...) 宏、glsl-pretty
         (all-from-out "../racket-glsl/rename-vector.rkt")) ; vec2…、concat-vecs、f32vector…

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
