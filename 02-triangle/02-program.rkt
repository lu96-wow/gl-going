#lang racket/base
;; =========================================================
;; 02-triangle/02-program.rkt —— 把着色器编译链接成"程序"
;; 运行：racket 02-triangle/02-program.rkt
;; =========================================================

;; 上一步写好了两段 GLSL 文本。GPU 不认文本，要先变成"能跑的程序"。
;;
;; 概念：从文本到程序分两步（类比 C 的 编译 → 链接）：
;;   ① 编译：每段 GLSL → 一个"着色器对象"（只代表一个阶段，半成品）
;;   ② 链接：顶点 + 片元两个着色器对象 → 一个"程序对象"（完整管线，能跑）
;; 为什么要两步？两段各自独立编译，哪段错了报哪段；链接再检查两段之间的
;; 接口（顶点输出 ↔ 片元输入）是否对得上。
;;
;; 这两步的 gl-* 细节（gl-create-shader、gl-shader-source、状态检查、日志…）
;; 已收进 racket-glsl/tool.rkt，本步直接调用 build-program，不裸写。

(require "../01-window/04-gui-tool.rkt")   ; 01 课的窗口工具（编译只需上下文，还不需要视口）
(require "../racket-glsl/rewrite.rkt")  ; (glsl ...) 宏
(require "../racket-glsl/tool.rkt")     ; build-program

;; 两段着色器（同 01-shader.rkt，只列代码，不再解释）。
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

;; 所有 gl-* 调用需要"当前 GL 上下文"（01 课：上下文挂在画布上），所以先建画布。
(define-values (frame canvas)
  (make-window #:title "02-02 编译链接"))

;; build-program：把每段 (阶段类型 源码) 编译 + 链接成一个程序对象。
;;   gl-vertex-shader   —— GL 常量，表示"这一段是顶点着色器"
;;   gl-fragment-shader —— GL 常量，表示"这一段是片元着色器"
;; 失败会自动抛错并带 GLSL/链接日志，不用自己查状态。
(send canvas with-gl-context
  (lambda ()
    (define prog (build-program (gl-vertex-shader vert-src)
                                (gl-fragment-shader frag-src)))
    (printf "着色器编译链接成功，程序编号 = ~a\n" prog)))

;; 本步不画，编译链接完打印成功就结束。
