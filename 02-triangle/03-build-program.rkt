#lang racket/base
;; =========================================================
;; 02-triangle/03-build-program.rkt —— 第三步：把编译链接收进 lib.rkt
;; 运行：racket 02-triangle/03-build-program.rkt
;; =========================================================
;; 上一步：glCreateShader → glLinkProgram 那 7 个调用每次都一样——纯重复。
;; 本步把它们收进本文件夹 lib.rkt 的 build-program（本步创建），之后直接调用。
;;
;; build-program 的实现 = 上一步的 compile-shader + link-program，见 lib.rkt。
;; 本步起 lib.rkt 还转发 (glsl ...) 宏、glsl-pretty、f32vector 等——之后每课
;; 只需 require lib-gui.rkt + lib.rkt 两处。
;;
;; ★节奏（整门课通用）：
;;   ① 裸写一遍新机制（上一步）
;;   ② 发现它重复 → 收进 lib（本步）
;;   ③ 后面的课直接调用，聚焦本课真正的新东西
;; =========================================================

(require "lib-gui.rkt")
(require "lib.rkt")   ; build-program + (glsl ...) + f32vector…

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

(define (draw)
  (glClearColor 0.10 0.12 0.20 1.0)
  (glClear GL_COLOR_BUFFER_BIT))

(define-values (frame canvas)
  (make-window #:title "02-03 用 build-program" #:width 400 #:height 300 #:draw draw))

(send canvas with-gl-context
      (lambda ()
        (define prog (build-program vert-src frag-src))  ; 上一步的 7 行 → 1 行
        (glUseProgram prog)))
(printf "着色器程序编译链接成功\n")

(send frame show #t)
