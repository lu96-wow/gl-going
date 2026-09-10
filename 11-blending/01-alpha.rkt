#lang racket/base
;; =========================================================
;; 11-blending/01-alpha.rkt —— 第一步：alpha + 混合公式
;; 运行：racket 11-blending/01-alpha.rkt     B = 混合开/关   点X = 退出
;; =========================================================
;; 前面所有 FragColor 的第 4 个分量（alpha）都写死 1.0。本步用它做"半透明"。
;;
;; 本步新增（2 个）：
;;   ① alpha —— 颜色的第 4 分量 = "不透明度"（1=不透明，0=全透）
;;   ② glBlendFunc —— 混合公式：把新画的颜色和缓冲里已有的颜色按比例掺在一起
;;
;; ★混合公式（本步核心）：
;;   out = src.rgb × src.a  +  dst.rgb × (1 − src.a)
;;   src = 新画的片元（比如玻璃），dst = 缓冲里已有的（玻璃后面的东西）。
;;   glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA) 就是指定这两个系数：
;;   源用 src.a（自己的不透明度），目标用 1−src.a（剩下的透过率）。
;;   α=1 → 全用自己的颜色（不透明）；α=0.35 → 35% 自己 + 65% 后面的。
;;
;; ★注意：混合发生在片元着色器输出**之后**的固定阶段——GLSL 只负责输出
;;   alpha 是多少，能不能混、怎么混是 glEnable(GL_BLEND) + glBlendFunc 的事。
;;
;; 本步视觉：一个不透明立方体 + 它前面一块绿色半透明"玻璃板"。B 键关混合：
;;   alpha 被无视，玻璃变实心、挡住后面的立方体。
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
        (uniform vec3 uTint)    ; 物体颜色（统一色，便于换不同玻璃色）
        (uniform float uAlpha)  ; 不透明度
        (out vec4 FragColor)
        (define (main) void
          (set! FragColor (vec4 uTint uAlpha)))))   ; ★第 4 分量 = 不透明度

(define (on-char e)
  (define code (send e get-key-code))
  (when (not (eq? code 'release))
    (cond
      [(or (eq? code #\b) (eq? code #\B))
       (set-box! blend-on? (not (unbox blend-on?)))
       (printf (if (unbox blend-on?) "混合 开（玻璃透出后面）~%" "混合 关（画成实心）~%"))]
      [(eq? code 'escape) (exit 0)])))

(define (draw)
  (define t (/ (- (current-inexact-milliseconds) start-ms) 1000.0))
  (define-values (w h) (send canvas get-gl-client-size))
  (define aspect (/ (exact->inexact w) (exact->inexact h)))
  (define P (m4-perspective 45.0 aspect 0.1 100.0))
  (define V (m4-look-at 0.0 0.0 5.0  0.0 0.0 0.0  0.0 1.0 0.0))

  (if (unbox blend-on?)
      (begin (glEnable GL_BLEND)
             (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA))   ; ★混合公式
      (glDisable GL_BLEND))

  (glClearColor 0.06 0.07 0.12 1.0)
  (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
  (glUseProgram prog)
  (glBindVertexArray vao)

  ;; 第一遍：不透明立方体（正常画，写深度）
  (glUniform1f loc-alpha 1.0)
  (glUniform3f loc-tint 0.70 0.72 0.80)
  (define M (m4-mult (m4-rot-y (* t 40.0)) (m4-rot-x (* t 30.0))))
  (glUniformMatrix4fv loc-mvp 1 #f (mat4 (m4-mult (m4-mult P V) M)))
  (glDrawElements GL_TRIANGLES 36 GL_UNSIGNED_SHORT 0)

  ;; 第二遍：半透明玻璃板（单位立方体把 z 压扁成薄板，挡在立方体前）
  (glUniform1f loc-alpha 0.35)
  (glUniform3f loc-tint 0.30 0.90 0.60)
  (define G (m4-mult (m4-translate 0.0 0.0 1.5)
                     (m4-scale 1.2 1.2 0.02)))
  (glUniformMatrix4fv loc-mvp 1 #f (mat4 (m4-mult (m4-mult P V) G)))
  (glDrawElements GL_TRIANGLES 36 GL_UNSIGNED_SHORT 0))

(define-values (frame canvas)
  (make-window #:title "11-01 alpha 与混合公式"
               #:width 600 #:height 600 #:draw draw #:on-char on-char))

(define prog (send canvas with-gl-context (lambda () (build-program vert-src frag-src))))
(define loc-mvp   (send canvas with-gl-context (lambda () (glGetUniformLocation prog "uMVP"))))
(define loc-tint  (send canvas with-gl-context (lambda () (glGetUniformLocation prog "uTint"))))
(define loc-alpha (send canvas with-gl-context (lambda () (glGetUniformLocation prog "uAlpha"))))
(define vao
  (send canvas with-gl-context
        (lambda ()
          (glEnable GL_DEPTH_TEST)
          (define vbo (u32vector-ref (glGenBuffers 1) 0))
          (glBindBuffer GL_ARRAY_BUFFER vbo)
          ;; cube-verts 是 pos+color；这里只用前 3 个 float（位置），颜色由 uTint 给
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
