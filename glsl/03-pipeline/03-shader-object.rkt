#lang racket/base
;; =========================================================
;; 03-pipeline/03-shader-object.rkt —— 第三步：着色器对象（编译）
;; 运行：racket glsl/03-pipeline/03-shader-object.rkt
;; =========================================================
;; 上一步：GLSL 是一段"运行时才编译"的源码。本步裸写"编译"这一步。
;;
;; ★着色器对象 = 编译后的中间产物。三段 gl* 调用：
;;   glCreateShader(type)          造一个空着色器对象，type 说它是哪一段
;;   glShaderSource(shader, ...)   把源码文本喂进去
;;   glCompileShader(shader)       编译成 GPU 能跑的字节码
;;
;; ★为什么必须查编译状态：glCompileShader 是异步的——调用返回≠成功。
;;   不查，写错一行 GLSL 只会黑屏、毫无提示。查 glGetShaderiv(GL_COMPILE_STATUS)
;;   + 打印 glGetShaderInfoLog，才能看到"第几行、什么错"。
;;
;; ★本步只编译一段顶点着色器，还没有片元、没有链接、没有画——所以画面
;;   仍是清屏色。重点是看懂"源码 → 着色器对象"这一跳。
;; =========================================================

(require "lib-gui.rkt")
(require "../../racket-glsl/rewrite.rkt")
(require ffi/vector)   ; s32vector（glShaderSource 的签名要求）

(define vert-src
  (glsl (version 330 core)
        (layout (location 0) in vec2 aPos)
        (define (main) void
          (set! gl_Position (vec4 aPos 0.0 1.0)))))

;; 取 info log：编译失败时它就是 GLSL 报错信息（新手最重要的调试手段）
(define (shader-info-log shader)
  (define len (glGetShaderiv shader GL_INFO_LOG_LENGTH))
  (define-values (actual log) (glGetShaderInfoLog shader len))
  (bytes->string/utf-8 log #\? 0 actual))

;; 编译：一段 GLSL 文本 → 一个着色器对象（本步主角）
(define (compile-shader type src)
  (define shader (glCreateShader type))                       ; ① 造空对象
  ;; ② 喂源码：glShaderSource 的签名要求"字符串向量 + 每段长度"，所以绕一下
  (glShaderSource shader 1 (vector src) (s32vector (string-length src)))
  (glCompileShader shader)                                    ; ③ 编译
  ;; ★查状态：失败就打印 GLSL 报错并停在这里
  (when (zero? (glGetShaderiv shader GL_COMPILE_STATUS))
    (error 'compile-shader "着色器编译失败：\n~a" (shader-info-log shader)))
  shader)

(define (draw)
  (glClearColor 0.10 0.12 0.20 1.0)
  (glClear GL_COLOR_BUFFER_BIT))

(define-values (frame canvas)
  (make-window #:title "03-03 着色器对象" #:width 400 #:height 300 #:draw draw))

;; 在 GL 上下文里编译；成功后打印着色器对象编号（一个整数句柄）
(send canvas with-gl-context
      (lambda ()
        (define vs (compile-shader GL_VERTEX_SHADER vert-src))
        (printf "顶点着色器编译成功，着色器对象编号 = ~a\n" vs)))

(send frame show #t)
;; 一个着色器对象只是"半成品"——它单独不能跑。下一步把顶点+片元两段
;; 链接成一个完整的程序。
