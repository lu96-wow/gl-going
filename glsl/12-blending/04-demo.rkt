#lang racket/base
;; =========================================================
;; 12-blending/04-demo.rkt —— 第四步：综合，半透明玻璃场景
;; 运行：racket glsl/12-blending/04-demo.rkt     B = 混合开/关   点X = 退出
;; =========================================================
;; 本课前三步：alpha+混合公式(01)、顺序坑(02)、深度写入坑(03)。
;; 本步**不引入新语法**，把老教程 10-blending 的成品拼出来。
;;
;; 场景：三颗不透明立方体 + 它们前面一块绿色半透明"玻璃"。
;;   正确的透明画法 = 先画不透明（写深度）→ 再画透明（关深度写入）。
;;   B 键关混合：alpha 被无视，玻璃变实心，立刻挡住后面。
;; =========================================================

(require "lib-gui.rkt")
(require "lib.rkt")

(define start-ms (current-inexact-milliseconds))
(define blend-on? (box #t))

(define vert-src
  (glsl (version 330 core)
        (layout (location 0) in vec3 aPos)
        (uniform mat4 uMVP)
        (define (main) void
          (set! gl_Position (* uMVP (vec4 aPos 1.0))))))

(define frag-src
  (glsl (version 330 core)
        (uniform vec3 uTint)
        (uniform float uAlpha)
        (out vec4 FragColor)
        (define (main) void
          (set! FragColor (vec4 uTint uAlpha)))))

(define (on-char e)
  (define code (send e get-key-code))
  (when (not (eq? code 'release))
    (cond
      [(or (eq? code #\b) (eq? code #\B))
       (set-box! blend-on? (not (unbox blend-on?)))
       (printf (if (unbox blend-on?) "混合 开（玻璃透出后面）~%" "混合 关（玻璃变实心）~%"))]
      [(eq? code 'escape) (exit 0)])))

(define (draw)
  (define t (/ (- (current-inexact-milliseconds) start-ms) 1000.0))
  (define-values (w h) (send canvas get-gl-client-size))
  (define aspect (/ (exact->inexact w) (exact->inexact h)))
  (define P (mat4-perspective 45.0 aspect 0.1 100.0))
  (define V (mat4-look-at 0.0 2.6 7.5  0.0 0.6 0.0  0.0 1.0 0.0))

  (if (unbox blend-on?)
      (begin (glEnable GL_BLEND)
             (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA))
      (glDisable GL_BLEND))
  (glClearColor 0.06 0.07 0.12 1.0)
  (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
  (glUseProgram prog)
  (glBindVertexArray vao)

  (define (cube m tint a)
    (glUniform3f loc-tint (list-ref tint 0) (list-ref tint 1) (list-ref tint 2))
    (glUniform1f loc-alpha a)
    (glUniformMatrix4fv loc-mvp 1 #f (mat4-mult (mat4-mult P V) m))
    (glDrawElements GL_TRIANGLES 36 GL_UNSIGNED_SHORT 0))

  ;; ---- 第一遍：不透明物体（写深度）----
  (glDepthMask #t)
  (cube (mat4-mult (mat4-translate -1.9 0.5 0.0) (mat4-mult (mat4-rot-y (* t 40.0)) (mat4-scale 0.5 0.5 0.5)))
        '(0.85 0.30 0.30) 1.0)
  (cube (mat4-mult (mat4-translate  1.9 0.5 0.0) (mat4-mult (mat4-rot-y (* t 40.0)) (mat4-scale 0.5 0.5 0.5)))
        '(0.30 0.60 0.95) 1.0)
  (cube (mat4-mult (mat4-translate 0.0 0.6 0.0) (mat4-mult (mat4-rot-y (* t -30.0)) (mat4-scale 0.5 0.5 0.5)))
        '(0.70 0.72 0.80) 1.0)

  ;; ---- 第二遍：半透明玻璃板（关深度写入）----
  (glDepthMask #f)
  (cube (mat4-mult (mat4-translate 0.0 0.9 1.2) (mat4-scale 1.1 1.1 0.02))
        '(0.30 0.90 0.60) 0.35)
  (glDepthMask #t))

(define-values (frame canvas)
  (make-window #:title "12-04 半透明玻璃（综合）"
               #:width 800 #:height 600 #:draw draw #:on-char on-char))

(define prog (send canvas with-gl-context (lambda () (build-program (GL_VERTEX_SHADER vert-src) (GL_FRAGMENT_SHADER frag-src)))))
(define loc-mvp   (send canvas with-gl-context (lambda () (glGetUniformLocation prog "uMVP"))))
(define loc-tint  (send canvas with-gl-context (lambda () (glGetUniformLocation prog "uTint"))))
(define loc-alpha (send canvas with-gl-context (lambda () (glGetUniformLocation prog "uAlpha"))))
(define vao
  (send canvas with-gl-context
        (lambda ()
          (glEnable GL_DEPTH_TEST)
          (define vbo (u32vector-ref (glGenBuffers 1) 0))
          (glBindBuffer GL_ARRAY_BUFFER vbo)
          (glBufferData GL_ARRAY_BUFFER (gl-vector-sizeof cube-verts) cube-verts GL_STATIC_DRAW)
          (define v (u32vector-ref (glGenVertexArrays 1) 0))
          (glBindVertexArray v)
          (glVertexAttribPointer 0 (glsl-size 'vec3) GL_FLOAT #f (glsl-stride-bytes 'vec3 'vec3) 0)
          (glEnableVertexAttribArray 0)
          (define ebo (u32vector-ref (glGenBuffers 1) 0))
          (glBindBuffer GL_ELEMENT_ARRAY_BUFFER ebo)
          (glBufferData GL_ELEMENT_ARRAY_BUFFER (gl-vector-sizeof cube-idx) cube-idx GL_STATIC_DRAW)
          (glBindVertexArray 0)
          v)))

(define ticker
  (new timer% (interval 16)
       (notify-callback (lambda () (send canvas refresh)))))

(send frame show #t)
(send canvas focus)
