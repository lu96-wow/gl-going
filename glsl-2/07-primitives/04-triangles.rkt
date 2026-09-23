#lang racket/base
;; =========================================================
;; 07-primitives/04-triangles.rkt —— 第四步：三角形图元 + 省顶点
;; 运行：racket 07-primitives/04-triangles.rkt    点 X = 退出
;; =========================================================

;; 上一步：会画线。本步回到三角形——02 课只用了 gl-triangles，本步看全三种。
;;
;; 本步新增（1 组，同属"三角形图元"这一件事）：
;;   gl-triangles      —— 每 3 个顶点一组，独立三角形
;;   gl-triangle-strip —— 后一个顶点 + 前两个顶点组成下一个三角形（本步主角）
;;   gl-triangle-fan   —— 第一个顶点是"扇心"，与每对后续顶点组成三角形
;;
;; ★为什么有 strip/fan（"省顶点"的数学）：
;;   四边形 = 2 个三角形。用 gl-triangles 要 6 个顶点（05 课那个四边形就是，
;;   对角线两个角各存两次）。用 gl-triangle-strip 只要 4 个顶点：
;;     v0 v1 v2 v3 → 三角形① = (v0,v1,v2)，三角形② = (v2,v1,v3)
;;   两个三角形自动共享对角线，相邻三角形共用一条边——顶点数从 6 降到 4。
;;
;; 本步视觉：一个四边形，4 个顶点 4 种颜色，用 gl-triangle-strip 画。
;;   你能看到对角线上颜色连续过渡——两个三角形共享的那条边（v1-v2）。
;;   ★对照：把 draw 里改成 gl-triangles 并只画 3 个顶点，会只出半个四边形。
;; =========================================================

(require "../02-triangle/04-gui-tool.rkt")   ; make-window（带视口）
(require "../racket-glsl/rewrite.rkt")        ; (glsl ...) 宏
(require "../racket-glsl/rename-vector.rkt")  ; vec2/vec3/concat-vecs/glsl-size/glsl-stride-bytes
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

;; 4 个顶点（左下、右下、左上、右上），strip 的顺序保证两个三角形拼成四边形。
;; 4 种颜色让你看清"哪条边是共享的"。
(define verts
  (concat-vecs (vec2 -0.3 -0.3) (vec3 1.0 0.3 0.3)   ; v0 左下 红
               (vec2  0.3 -0.3) (vec3 0.3 1.0 0.3)   ; v1 右下 绿
               (vec2 -0.3  0.3) (vec3 0.3 0.3 1.0)   ; v2 左上 蓝
               (vec2  0.3  0.3) (vec3 1.0 0.9 0.3))) ; v3 右上 黄

(define (draw)
  (gl-clear-color 0.10 0.12 0.20 1.0)
  (gl-clear gl-color-buffer-bit)
  (use-program prog)
  (gl-bind-vertex-array vao)
  (gl-draw-arrays gl-triangle-strip 0 4))    ; ★4 个顶点画出 2 个三角形

(define-values (frame canvas)
  (make-window #:title "07-04 三角形条带" #:width 400 #:height 400 #:draw draw))

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
      (gl-bind-vertex-array 0)
      v)))

(send frame show #t)
