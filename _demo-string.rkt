#lang racket/base
;; 裸字符串着色器（没有 (glsl ...) 源映射）：拼错 aPos → aPs
(require "01-window/04-gui-tool.rkt" "racket-glsl/tool.rkt")
(define vs
  "#version 330 core\nlayout(location = 0) in vec2 aPos;\nvoid main() { gl_Position = vec4(aPs, 0.0, 1.0); }")
(define-values (frame canvas) (make-window #:title "裸字符串报错"))
(send canvas with-gl-context (lambda () (compile-shader gl-vertex-shader vs)))
