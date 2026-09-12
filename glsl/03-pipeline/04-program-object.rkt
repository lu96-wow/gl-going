#lang racket/base
;; =========================================================
;; 03-pipeline/04-program-object.rkt —— 第四步：程序对象（链接）
;; 运行：racket glsl/03-pipeline/04-program-object.rkt
;; =========================================================
;; 上一步：一段 GLSL 文本 → 一个着色器对象（半成品）。本步把它拼成能跑的
;; "程序"。
;;
;; ★程序对象 = 把若干着色器对象链接成一个整体。三段 gl* 调用：
;;   glCreateProgram()           造一个空程序对象
;;   glAttachShader(prog, s)     把着色器对象挂进去（顶点+片元都挂）
;;   glLinkProgram(prog)         链接：把各段之间的 in/out 接起来、生成可执行体
;;
;; ★为什么顶点+片元必须一起链接：两段要能对接（顶点 out 的类型/名字要和
;;   片元 in 对得上），链接时驱动检查这个对接，错了就报链接错误。
;;
;; ★本步编译了顶点+片元、链接成一个程序，但还没 glUseProgram、也没画——
;;   所以画面仍是清屏色。链接成功 ≠ 生效（生效是下一步）。
;; =========================================================

(require "lib-gui.rkt")
(require "../../racket-glsl/rewrite.rkt")
(require ffi/vector)

(define vert-src
  (glsl (version 330 core)
        (layout (location 0) in vec2 aPos)
        (define (main) void
          (set! gl_Position (vec4 aPos 0.0 1.0)))))
(define frag-src
  (glsl (version 330 core)
        (out vec4 FragColor)
        (define (main) void
          (set! FragColor (vec4 1.0 0.35 0.2 1.0)))))

(define (shader-info-log shader)
  (define len (glGetShaderiv shader GL_INFO_LOG_LENGTH))
  (define-values (actual log) (glGetShaderInfoLog shader len))
  (bytes->string/utf-8 log #\? 0 actual))

(define (program-info-log prog)
  (define len (glGetProgramiv prog GL_INFO_LOG_LENGTH))
  (define-values (actual log) (glGetProgramInfoLog prog len))
  (bytes->string/utf-8 log #\? 0 actual))

(define (compile-shader type src)
  (define shader (glCreateShader type))
  (glShaderSource shader 1 (vector src) (s32vector (string-length src)))
  (glCompileShader shader)
  (when (zero? (glGetShaderiv shader GL_COMPILE_STATUS))
    (error 'compile-shader "着色器编译失败：\n~a" (shader-info-log shader)))
  shader)

;; 链接：若干着色器对象 → 一个程序（本步主角）
(define (link-program . shaders)
  (define prog (glCreateProgram))                             ; ① 空程序
  (for-each (lambda (s) (glAttachShader prog s)) shaders)     ; ② 挂各段
  (glLinkProgram prog)                                        ; ③ 链接
  (when (zero? (glGetProgramiv prog GL_LINK_STATUS))          ; ★查状态
    (error 'link-program "程序链接失败：\n~a" (program-info-log prog)))
  prog)

(define (draw)
  (glClearColor 0.10 0.12 0.20 1.0)
  (glClear GL_COLOR_BUFFER_BIT))

(define-values (frame canvas)
  (make-window #:title "03-04 程序对象" #:width 400 #:height 300 #:draw draw))

(send canvas with-gl-context
      (lambda ()
        (define vs (compile-shader GL_VERTEX_SHADER vert-src))
        (define fs (compile-shader GL_FRAGMENT_SHADER frag-src))
        (define prog (link-program vs fs))
        (printf "程序链接成功，程序编号 = ~a（还没启用，下一步）\n" prog)))

(send frame show #t)
