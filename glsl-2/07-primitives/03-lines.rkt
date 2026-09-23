#lang racket/base
;; =========================================================
;; 07-primitives/03-lines.rkt —— 第三步：线图元（lines / line-strip / line-loop）
;; 运行：racket 07-primitives/03-lines.rkt    点 X = 退出
;; =========================================================

;; 上一步：会画点。本步画线，并看清三种"线图元"的连法差别。
;;
;; 本步新增（1 组，同属"线图元"这一件事）：
;;   gl-lines      —— 每 2 个顶点一组，画一段线段（各组互不相连）
;;   gl-line-strip —— 顶点顺次相连，画一条折线（本步主角）
;;   gl-line-loop  —— 同 strip，但最后再把末点连回首点，闭合
;;
;; ★图形原理：线图元 = "顶点流"按不同规则连。同一串顶点：
;;   v0 v1 v2 v3 v4
;;     gl-lines      → v0-v1、v2-v3 各自成段（v1-v2 之间断开）
;;     gl-line-strip → v0-v1-v2-v3-v4 一条折线
;;     gl-line-loop  → v0-v1-v2-v3-v4-v0 折线 + 闭合
;;
;; 本步视觉：7 个顶点高低起伏，用 gl-line-strip 连成一条波浪折线。
;;   ★动手试：把下面 draw 里的 gl-line-strip 改成 gl-lines，再看——
;;   折线会变成"每两个一组"的断线段；改成 gl-line-loop 则首尾连上。
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

;; 7 个顶点，y 一上一下，x 从左到右；颜色统一用亮青，读成"一条线"。
(define verts
  (concat-vecs (vec2 -0.9 -0.35) (vec3 0.3 0.8 1.0)
               (vec2 -0.6  0.35) (vec3 0.3 0.8 1.0)
               (vec2 -0.3 -0.35) (vec3 0.3 0.8 1.0)
               (vec2  0.0  0.35) (vec3 0.3 0.8 1.0)
               (vec2  0.3 -0.35) (vec3 0.3 0.8 1.0)
               (vec2  0.6  0.35) (vec3 0.3 0.8 1.0)
               (vec2  0.9 -0.35) (vec3 0.3 0.8 1.0)))

(define (draw)
  (gl-clear-color 0.10 0.12 0.20 1.0)
  (gl-clear gl-color-buffer-bit)
  (use-program prog)
  (gl-bind-vertex-array vao)
  (gl-draw-arrays gl-line-strip 0 7))    ; ★顺次连成折线

(define-values (frame canvas)
  (make-window #:title "07-03 线" #:width 400 #:height 400 #:draw draw))

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
