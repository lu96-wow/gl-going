#lang racket/base
;; =========================================================
;; 03-pipeline/06-build-program.rkt —— 第六步：收进 lib，一次编译任意阶段
;; 运行：racket glsl/03-pipeline/06-build-program.rkt
;; =========================================================
;; 前两步裸写的 compile-shader + link-program 每次都一样——纯重复。
;; 本步把它们收进本文件夹 lib.rkt 的 build-program，之后直接调用。
;;
;; ★通用签名（每段 = (阶段类型 源码)）：
;;   (build-program (GL_VERTEX_SHADER vs-src) (GL_FRAGMENT_SHADER fs-src))
;;
;;   阶段类型可任意组合，一次编译链接"整个管线"为一个程序：
;;   (build-program (GL_VERTEX_SHADER vs) (GL_GEOMETRY_SHADER gs) (GL_FRAGMENT_SHADER fs))
;;   (build-program (GL_VERTEX_SHADER vs) (GL_TESS_CONTROL_SHADER tcs)
;;                  (GL_TESS_EVALUATION_SHADER tes) (GL_FRAGMENT_SHADER fs))
;;   本教程当前只用 顶点+片元 两段（③④可选段以后讲），但 build-program
;;   从本步起就已经能装下全部阶段。
;;
;; ★节奏（整门课通用）：① 裸写一遍新机制（前两步）② 发现重复 → 收进 lib
;;   ③ 后面的课直接调用，聚焦本课真正的新东西。
;; =========================================================

(require "lib-gui.rkt")
(require "lib.rkt")

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

(define verts (vec (vec2 -0.5 -0.5) (vec2 0.5 -0.5) (vec2 0.0 0.5)))

(define (draw)
  (glClearColor 0.10 0.12 0.20 1.0)
  (glClear GL_COLOR_BUFFER_BIT)
  (glUseProgram prog)
  (glBindVertexArray vao)
  (glDrawArrays GL_TRIANGLES 0 3))

(define-values (frame canvas)
  (make-window #:title "03-06 build-program 通用版" #:width 400 #:height 300 #:draw draw))

(define prog
  (send canvas with-gl-context
        (lambda ()
          ;; 每段显式写出阶段类型：一眼看清"这个程序由哪些段组成"
          (build-program (GL_VERTEX_SHADER vert-src)
                         (GL_FRAGMENT_SHADER frag-src)))))
(define vao
  (send canvas with-gl-context
        (lambda ()
          (define vbo (u32vector-ref (glGenBuffers 1) 0))
          (glBindBuffer GL_ARRAY_BUFFER vbo)
          (glBufferData GL_ARRAY_BUFFER (gl-vector-sizeof (vec->f32vector verts))
                        (vec->f32vector verts) GL_STATIC_DRAW)
          (define v (u32vector-ref (glGenVertexArrays 1) 0))
          (glBindVertexArray v)
          (glVertexAttribPointer 0 2 GL_FLOAT #f 8 0)
          (glEnableVertexAttribArray 0)
          (glBindVertexArray 0)
          v)))

(send frame show #t)
