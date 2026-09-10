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
;; 本课（04-animate）从 03-glsl-types 原样复制，未改动：
;;   uniform/timer 的 glGetUniformLocation、glUniform1f、timer% 都是
;;   gl*/GUI 标准调用，直接内联在步骤文件里，不需要收进 lib。
;; =========================================================

(require opengl ffi/vector)                  ; gl* 常量；s32vector（glShaderSource 用）
(require "../racket-glsl/rewrite.rkt")       ; (glsl ...) 宏 + glsl-pretty
(require "../racket-glsl/rename-vector.rkt") ; vec2/vec3/vec4、concat-vecs

(provide build-program
         (all-from-out "../racket-glsl/rewrite.rkt")        ; (glsl ...) 宏、glsl-pretty
         (all-from-out "../racket-glsl/rename-vector.rkt")) ; vec2…、concat-vecs、f32vector…

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
