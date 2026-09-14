#lang racket/base
;; =========================================================
;; 07-primitives/05-ebo.rkt —— 第五步：EBO 索引缓冲
;; 运行：racket 07-primitives/05-ebo.rkt    点 X = 退出
;; =========================================================

;; 上一步：strip 用 4 个顶点画出了四边形。但 strip 只对"一条带子"管用——
;;   更复杂的网格（球面、模型）里三角形乱七八糟地共享顶点，strip 绕不过来。
;;   本步学通用解法：EBO 索引缓冲。
;;
;; 本步新增（3 个 gl* 调用，同属"索引缓冲"这一件事）：
;;   gl-bind-buffer gl-element-array-buffer —— 绑"索引缓冲"（和顶点缓冲不同槽）
;;   gl-buffer-data（上传索引）               —— 索引存在这里
;;   gl-draw-elements                        —— 不按顶点顺序画，而是按索引取顶点画
;;
;; ★问题（05 课埋的伏笔）：四边形 = 2 个三角形，用 gl-triangles 要 6 个顶点，
;;   但角只有 4 个——对角线两个端点各存了两次，数据重复。
;; ★解法：把"唯一的顶点"存一遍（4 个），再存一张"编号表"（6 个索引）：
;;   索引 0 1 2 = 三角形①，0 2 3 = 三角形②。GPU 按编号去取顶点，重复的角
;;   就共享了。数据更小，GPU 还能缓存最近用过的顶点。
;; ★EBO 要绑在 VAO 里：gl-element-array-buffer 的绑定会被 VAO 记住，之后
;;   绑 VAO 时索引缓冲自动跟着走（和属性指针一起，成为 VAO 的一部分）。
;;
;; 本步视觉：和 04 步一模一样的四边形（4 角 4 色），但数据组织不同——
;;   04 是"4 顶点按 strip 顺序"，本步是"4 顶点 + 6 个编号"。画面相同，
;;   区别只在数据。
;; =========================================================

(require "../02-triangle/04-gui-tool.rkt")   ; make-window（带视口）
(require "../racket-glsl/rewrite.rkt")        ; (glsl ...) 宏
(require "../racket-glsl/rename-vector.rkt")  ; vec2/vec3/concat-vecs/glsl-size/glsl-stride-bytes + u16vector
(require "../racket-glsl/tool.rkt")           ; build-program / use-program

(define vert-src
  (glsl (version 330 core)
        (layout (location 0) in vec2 aPos)
        (layout (location 1) in vec3 aColor)
        (out vec3 vColor)
        (define (main) void
          (set! vColor aColor)
          (set! gl_Position (vec4 aPos 0.0 1.0)))))

(define frag-src
  (glsl (version 330 core)
        (in vec3 vColor)
        (out vec4 FragColor)
        (define (main) void
          (set! FragColor (vec4 vColor 1.0)))))

;; 4 个角（左下、右下、右上、左上），只存一次。
(define verts
  (concat-vecs (vec2 -0.3 -0.3) (vec3 1.0 0.3 0.3)   ; 0 左下 红
               (vec2  0.3 -0.3) (vec3 0.3 1.0 0.3)   ; 1 右下 绿
               (vec2  0.3  0.3) (vec3 0.3 0.3 1.0)   ; 2 右上 蓝
               (vec2 -0.3  0.3) (vec3 1.0 0.9 0.3))) ; 3 左上 黄

;; 索引：三角形① = 0-1-2，三角形② = 0-2-3。u16 = 16 位无符号整数（0..65535）。
(define idx (u16vector 0 1 2  0 2 3))

(define (draw)
  (gl-clear-color 0.10 0.12 0.20 1.0)
  (gl-clear gl-color-buffer-bit)
  (use-program prog)
  (gl-bind-vertex-array vao)
  ;; ★按索引画：gl-triangles、6 个索引、索引是 16 位无符号、从第 0 个索引开始
  (gl-draw-elements gl-triangles 6 gl-unsigned-short 0))

(define-values (frame canvas)
  (make-window #:title "07-05 EBO 索引缓冲" #:width 400 #:height 400 #:draw draw))

(define prog
  (send canvas with-gl-context
    (lambda () (build-program (gl-vertex-shader vert-src) (gl-fragment-shader frag-src)))))
(define vao
  (send canvas with-gl-context
    (lambda ()
      (define vbo (u32vector-ref (gl-gen-buffers 1) 0))
      (gl-bind-buffer gl-array-buffer vbo)
      (gl-buffer-data gl-array-buffer (gl-vector-sizeof verts) verts gl-static-draw)
      (define v (u32vector-ref (gl-gen-vertex-arrays 1) 0))
      (gl-bind-vertex-array v)
      (gl-vertex-attrib-pointer 0 (glsl-size 'vec2) gl-float #f (glsl-stride-bytes 'vec2 'vec3) 0)
      (gl-enable-vertex-attrib-array 0)
      (gl-vertex-attrib-pointer 1 (glsl-size 'vec3) gl-float #f
                                (glsl-stride-bytes 'vec2 'vec3)
                                (glsl-stride-bytes 'vec2))
      (gl-enable-vertex-attrib-array 1)
      ;; ★索引缓冲要在 VAO 还绑着的时候绑——VAO 会把它一起记住
      (define ebo (u32vector-ref (gl-gen-buffers 1) 0))
      (gl-bind-buffer gl-element-array-buffer ebo)
      (gl-buffer-data gl-element-array-buffer (gl-vector-sizeof idx) idx gl-static-draw)
      (gl-bind-vertex-array 0)
      v)))

(send frame show #t)
