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
;; 本课（03-glsl-types）从 02-triangle 原样复制，未改动：
;;   本课是纯着色器语言课，不引入新 GL API，build-program / vec2… 继续用。
;; =========================================================

(require opengl ffi/vector)                  ; gl* 常量；s32vector（glShaderSource 用）
(require "../racket-glsl/rewrite.rkt")       ; (glsl ...) 宏 + glsl-pretty
(require "../racket-glsl/rename-vector.rkt") ; vec2/vec3/vec4、concat-vecs

(provide build-program
         (all-from-out "../racket-glsl/rewrite.rkt")        ; (glsl ...) 宏、glsl-pretty
         (all-from-out "../racket-glsl/rename-vector.rkt")) ; vec2…、concat-vecs、f32vector…

;; 把一段 GLSL 文本编译成一个"着色器对象"（02-shader-load.rkt 讲的原始过程）
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
    (error 'build-program "着色器编译失败：\n~a" (shader-info-log shader)))
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
  ;; ★链接失败：打印日志并停在这里
  (when (zero? (glGetProgramiv prog GL_LINK_STATUS))
    (error 'build-program "程序链接失败：\n~a" (program-info-log prog)))
  prog)
