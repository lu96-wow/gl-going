#lang racket/base
;; =========================================================
;; 06-primitives/04-triangles.rkt —— 第四步：三角形图元 + 省顶点
;; 运行：racket glsl/06-primitives/04-triangles.rkt    点 X = 退出
;; =========================================================
;; 上一步：会画线。本步回到三角形——02 课只用了 GL_TRIANGLES，本步看全三种。
;;
;; 本步新增（1 组，同属"三角形图元"这一件事）：
;;   GL_TRIANGLES      —— 每 3 个顶点一组，独立三角形
;;   GL_TRIANGLE_STRIP —— 后一个顶点 + 前两个顶点组成下一个三角形（本步主角）
;;   GL_TRIANGLE_FAN   —— 第一个顶点是"扇心"，与每对后续顶点组成三角形
;;
;; ★为什么有 STRIP/FAN（"省顶点"的数学）：
;;   四边形 = 2 个三角形。用 GL_TRIANGLES 要 6 个顶点（04 课那个四边形就是，
;;   对角线两个角各存两次）。用 GL_TRIANGLE_STRIP 只要 4 个顶点：
;;     v0 v1 v2 v3 → 三角形① = (v0,v1,v2)，三角形② = (v2,v1,v3)
;;   两个三角形自动共享对角线，相邻三角形共用一条边——顶点数从 6 降到 4。
;;
;; 本步视觉：一个四边形，4 个顶点 4 种颜色，用 GL_TRIANGLE_STRIP 画。
;;   你能看到对角线上颜色连续过渡——两个三角形共享的那条边（v1-v2）。
;;   ★对照：把 draw 里改成 GL_TRIANGLES 并只画 3 个顶点，会只出半个四边形。
;; =========================================================

(require "lib-gui.rkt")
(require "lib.rkt")

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

;; 4 个顶点（左下、右下、左上、右上），STRIP 的顺序保证两个三角形拼成四边形。
;; 4 种颜色让你看清"哪条边是共享的"。
(define verts
  (concat-vecs (vec2 -0.3 -0.3) (vec3 1.0 0.3 0.3)   ; v0 左下 红
               (vec2  0.3 -0.3) (vec3 0.3 1.0 0.3)   ; v1 右下 绿
               (vec2 -0.3  0.3) (vec3 0.3 0.3 1.0)   ; v2 左上 蓝
               (vec2  0.3  0.3) (vec3 1.0 0.9 0.3))) ; v3 右上 黄

(define (draw)
  (glClearColor 0.07 0.08 0.14 1.0)
  (glClear GL_COLOR_BUFFER_BIT)
  (glUseProgram prog)
  (glBindVertexArray vao)
  (glDrawArrays GL_TRIANGLE_STRIP 0 4))    ; ★4 个顶点画出 2 个三角形

(define-values (frame canvas)
  (make-window #:title "06-04 三角形条带" #:width 400 #:height 400 #:draw draw))

(define prog (send canvas with-gl-context (lambda () (build-program (GL_VERTEX_SHADER vert-src) (GL_FRAGMENT_SHADER frag-src)))))
(define vao
  (send canvas with-gl-context
        (lambda ()
          (define vbo (u32vector-ref (glGenBuffers 1) 0))
          (glBindBuffer GL_ARRAY_BUFFER vbo)
          (glBufferData GL_ARRAY_BUFFER (gl-vector-sizeof verts) verts GL_STATIC_DRAW)
          (define v (u32vector-ref (glGenVertexArrays 1) 0))
          (glBindVertexArray v)
          (glVertexAttribPointer 0 (glsl-size 'vec2) GL_FLOAT #f (glsl-stride-bytes 'vec2 'vec3) 0)
          (glEnableVertexAttribArray 0)
          (glVertexAttribPointer 1 (glsl-size 'vec3) GL_FLOAT #f (glsl-stride-bytes 'vec2 'vec3) (glsl-stride-bytes 'vec2))
          (glEnableVertexAttribArray 1)
          (glBindVertexArray 0)
          v)))

(send frame show #t)
