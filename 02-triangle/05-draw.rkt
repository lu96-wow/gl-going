#lang racket/base
;; =========================================================
;; 02-triangle/05-draw.rkt —— 画出来！第一个三角形
;; 运行：racket 02-triangle/05-draw.rkt    点 X = 退出
;; =========================================================

;; 程序有了（02 步），顶点数据 + VAO 有了（03 步）。最后一步：画。
;; 新增两个：
;;   use-program  —— 启用程序（tool.rkt 里 glUseProgram 的包装）
;;   glDrawArrays —— 真正画

(require "04-gui-tool.rkt")         ; 本课的窗口工具（带视口）
(require "../racket-glsl/rewrite.rkt")        ; (glsl ...) 宏
(require "../racket-glsl/rename-vector.rkt")  ; vec / vec2 / vec->f32vector / u32vector-ref
(require "../racket-glsl/tool.rkt")           ; build-program / use-program

;; 着色器（同 01/02，片元固定橙色）。
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

;; 顶点数据（同 03）。
(define verts
  (vec (vec2 -0.5 -0.5)
       (vec2  0.5 -0.5)
       (vec2  0.0  0.5)))

;; 每帧画什么。注意：make-window 已自动"清屏"，这里只画内容。
(define (draw)
  (use-program prog)          ; 用哪个程序
  (glBindVertexArray vao)     ; 绑上 VAO（拿到"数据说明书"）
  ;; GL_TRIANGLES = 每 3 个顶点一组三角形；从第 0 个顶点起，画 3 个
  (glDrawArrays GL_TRIANGLES 0 3))

(define-values (frame canvas)
  (make-window #:title "02-05 第一个三角形" #:draw draw))

;; 初始化：程序 + VBO + VAO（同 02、03 步）。
;; 注意 draw 会引用 prog / vao，但 draw 到窗口显示后才被调用，所以写在后面也合法。
(define prog
  (send canvas with-gl-context
    (lambda ()
      (build-program (GL_VERTEX_SHADER vert-src)
                     (GL_FRAGMENT_SHADER frag-src)))))
(define vao
  (send canvas with-gl-context
    (lambda ()
      (define data (vec->f32vector verts))
      (define vbo (u32vector-ref (glGenBuffers 1) 0))
      (glBindBuffer GL_ARRAY_BUFFER vbo)
      (glBufferData GL_ARRAY_BUFFER (gl-vector-sizeof data) data GL_STATIC_DRAW)
      (define v (u32vector-ref (glGenVertexArrays 1) 0))
      (glBindVertexArray v)
      (glVertexAttribPointer 0 2 GL_FLOAT #f 8 0)
      (glEnableVertexAttribArray 0)
      (glBindVertexArray 0)
      v)))

(send frame show #t)
