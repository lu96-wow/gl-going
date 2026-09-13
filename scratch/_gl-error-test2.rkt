#lang racket/base
;; 类型不匹配：gl_Position 是 vec4，却直接赋 vec2 的 aPos
(require "../01-window/04-gui-tool.rkt")
(require "../racket-glsl/rewrite.rkt")
(require "../racket-glsl/tool.rkt")
(define vert-src
  (glsl (version 330 core)
        (layout (location 0) in vec2 aPos)
        (define (main) void
          (set! gl_Position aPos))))
(define frag-src
  (glsl (version 330 core)
        (out vec4 FragColor)
        (define (main) void
          (set! FragColor (vec4 1.0 0.35 0.2 1.0)))))
(define-values (frame canvas) (make-window #:title "错误测试2"))
(send canvas with-gl-context
  (lambda () (build-program (gl-vertex-shader vert-src) (gl-fragment-shader frag-src))))
