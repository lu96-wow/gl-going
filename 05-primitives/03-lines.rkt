#lang racket/base
;; =========================================================
;; 05-primitives/03-lines.rkt —— 第三步：线图元（GL_LINES / LINE_STRIP / LINE_LOOP）
;; 运行：racket 05-primitives/03-lines.rkt    点 X = 退出
;; =========================================================
;; 上一步：会画点。本步画线，并看清三种"线图元"的连法差别。
;;
;; 本步新增（1 组，同属"线图元"这一件事）：
;;   GL_LINES      —— 每 2 个顶点一组，画一段线段（各组互不相连）
;;   GL_LINE_STRIP —— 顶点顺次相连，画一条折线（本步主角）
;;   GL_LINE_LOOP  —— 同 STRIP，但最后再把末点连回首点，闭合
;;
;; ★图形原理：线图元 = "顶点流"按不同规则连。同一串顶点：
;;   v0 v1 v2 v3 v4
;;     GL_LINES      → v0-v1、v2-v3 各自成段（v1-v2 之间断开）
;;     GL_LINE_STRIP → v0-v1-v2-v3-v4 一条折线
;;     GL_LINE_LOOP  → v0-v1-v2-v3-v4-v0 折线 + 闭合
;;
;; 本步视觉：7 个顶点高低起伏，用 GL_LINE_STRIP 连成一条波浪折线。
;;   ★动手试：把下面 draw 里的 GL_LINE_STRIP 改成 GL_LINES，再看——
;;   折线会变成"每两个一组"的断线段；改成 GL_LINE_LOOP 则首尾连上。
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
  (glClearColor 0.07 0.08 0.14 1.0)
  (glClear GL_COLOR_BUFFER_BIT)
  (glUseProgram prog)
  (glBindVertexArray vao)
  (glDrawArrays GL_LINE_STRIP 0 7))    ; ★顺次连成折线

(define-values (frame canvas)
  (make-window #:title "05-03 线" #:width 400 #:height 400 #:draw draw))

(define prog (send canvas with-gl-context (lambda () (build-program vert-src frag-src))))
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
