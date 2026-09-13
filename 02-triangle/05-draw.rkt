#lang racket/base
;; =========================================================
;; 02-triangle/05-draw.rkt —— 画出来！第一个三角形
;; 运行：racket 02-triangle/05-draw.rkt    点 X = 退出
;; =========================================================

;; 程序有了（02 步），顶点数据 + VAO 有了（03 步）。最后一步：画。
;; 新增两个：
;;   use-program  —— 启用程序（tool.rkt 里 gl-use-program 的包装）
;;   gl-draw-arrays —— 真正画

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

;; 每帧画什么：先清屏，再画三角形。清屏由 draw 自己负责（make-window 只给骨架）。
;; ★为什么每帧都重新 use-program / bind-vertex-array？它们只是"设状态"（换驱动里
;;   的当前指针），不搬数据、不分配内存，开销可忽略；虽然本例只有一个程序/VAO，
;;   设一次就够，但让 draw 自包含是习惯：以后画多个物体时，每个物体画前绑好
;;   自己需要的，就不怕别的物体动过状态。
(define (draw)
  (gl-clear-color 0.10 0.12 0.20 1.0)  ; 清屏（03 步：先记色，再擦）
  (gl-clear gl-color-buffer-bit)
  (use-program prog)          ; 用哪个程序
  (gl-bind-vertex-array vao)     ; 绑上 VAO（拿到"数据说明书"）
  ;; gl-triangles = 每 3 个顶点一组三角形；从第 0 个顶点起，画 3 个
  (gl-draw-arrays gl-triangles 0 3))

(define-values (frame canvas)
  (make-window #:title "02-05 第一个三角形" #:draw draw))

;; 初始化：程序 + VBO + VAO（同 02、03 步）。
;; 注意 draw 会引用 prog / vao，但 draw 到窗口显示后才被调用，所以写在后面也合法。
(define prog
  (send canvas with-gl-context
    (lambda ()
      (build-program (gl-vertex-shader vert-src)
                     (gl-fragment-shader frag-src)))))
(define vao
  (send canvas with-gl-context
    (lambda ()
      (define data (vec->f32vector verts))
      (define vbo (u32vector-ref (gl-gen-buffers 1) 0))
      (gl-bind-buffer gl-array-buffer vbo)
      (gl-buffer-data gl-array-buffer (gl-vector-sizeof data) data gl-static-draw)
      (define v (u32vector-ref (gl-gen-vertex-arrays 1) 0))
      (gl-bind-vertex-array v)
      (gl-vertex-attrib-pointer 0 2 gl-float #f 8 0)
      (gl-enable-vertex-attrib-array 0)
      (gl-bind-vertex-array 0)
      v)))

(send frame show #t)
