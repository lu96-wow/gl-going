#lang racket/base
;; =========================================================
;; 03-pipeline/05-use-program.rkt —— 第五步：glUseProgram 才是开关
;; 运行：racket 03-pipeline/05-use-program.rkt
;; =========================================================
;; 上一步：程序链接成功，但"成功 ≠ 生效"。本步讲"生效"。
;;
;; ★glUseProgram(prog) = 把 prog 设为"当前程序"。这是唯一让程序生效的函数。
;;   GL 是状态机：glUseProgram 之后，所有 glDraw* 都走这个程序的着色器，
;;   直到下一次 glUseProgram 换别的。加载/链接只是"装好"，use 才是"插上电"。
;;
;; ★uniform 顺序：glUniform* 写给"当前程序"的 uniform，所以必须先 use 再传。
;;
;; ★本步演示：编译链接两个程序（一个橙色、一个青色），每帧按时间在两者间
;;   glUseProgram 切换——同一个三角形，颜色来回变，直观看到"换程序 = 换着色器"。
;; =========================================================

(require "lib-gui.rkt")
(require "../racket-glsl/rewrite.rkt")
(require "../racket-glsl/rename-vector.rkt") ; vec / vec2 / vec->f32vector（顶点数据）
(require ffi/vector)

(define vert-src
  (glsl (version 330 core)
        (layout (location 0) in vec2 aPos)
        (define (main) void
          (set! gl_Position (vec4 aPos 0.0 1.0)))))
;; 两个片元着色器：只有颜色不同
(define frag-orange
  (glsl (version 330 core)
        (out vec4 FragColor)
        (define (main) void
          (set! FragColor (vec4 1.0 0.35 0.2 1.0)))))
(define frag-cyan
  (glsl (version 330 core)
        (out vec4 FragColor)
        (define (main) void
          (set! FragColor (vec4 0.2 0.75 0.95 1.0)))))

(define (shader-info-log shader)
  (define len (glGetShaderiv shader GL_INFO_LOG_LENGTH))
  (define-values (actual log) (glGetShaderInfoLog shader len))
  (bytes->string/utf-8 log #\? 0 actual))
(define (program-info-log prog)
  (define len (glGetProgramiv prog GL_INFO_LOG_LENGTH))
  (define-values (actual log) (glGetProgramInfoLog prog len))
  (bytes->string/utf-8 log #\? 0 actual))
(define (compile-shader type src)
  (define shader (glCreateShader type))
  (glShaderSource shader 1 (vector src) (s32vector (string-length src)))
  (glCompileShader shader)
  (when (zero? (glGetShaderiv shader GL_COMPILE_STATUS))
    (error 'compile-shader "着色器编译失败：\n~a" (shader-info-log shader)))
  shader)
(define (link-program . stages)  ; stages = ((阶段类型 源码) ...) → 程序
  (define prog (glCreateProgram))
  (for ([s stages])
    (glAttachShader prog (compile-shader (car s) (cadr s))))
  (glLinkProgram prog)
  (when (zero? (glGetProgramiv prog GL_LINK_STATUS))
    (error 'link-program "程序链接失败：\n~a" (program-info-log prog)))
  prog)

(define verts (vec (vec2 -0.5 -0.5) (vec2 0.5 -0.5) (vec2 0.0 0.5)))

(define start-ms (current-inexact-milliseconds))

;; 每帧：按时间在橙/青两个程序间切换——换程序 = 换着色器
(define (draw)
  (define ms (current-inexact-milliseconds))
  (define use-orange? (< (modulo (- ms start-ms) 2000) 1000)) ; 每秒切一次
  (glClearColor 0.10 0.12 0.20 1.0)
  (glClear GL_COLOR_BUFFER_BIT)
  (glUseProgram (if use-orange? prog-orange prog-cyan))  ; ★切换就在这里
  (glBindVertexArray vao)
  (glDrawArrays GL_TRIANGLES 0 3))

(define-values (frame canvas)
  (make-window #:title "03-05 glUseProgram 切换" #:width 400 #:height 300 #:draw draw))

(define prog-orange
  (send canvas with-gl-context
        (lambda ()
          (link-program (GL_VERTEX_SHADER vert-src) (GL_FRAGMENT_SHADER frag-orange)))))
(define prog-cyan
  (send canvas with-gl-context
        (lambda ()
          (link-program (GL_VERTEX_SHADER vert-src) (GL_FRAGMENT_SHADER frag-cyan)))))
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
