#lang racket/base
;; =========================================================
;; 12-blending/02-order.rkt —— 第二步：渲染顺序坑（画家算法）
;; 运行：racket 12-blending/02-order.rkt     O = 切换顺序   点X = 退出
;; =========================================================
;; 上一步：一块玻璃。本步看两个重叠的半透明物体——顺序错了会穿帮。
;;
;; 本步新增（1 个）：
;;   渲染顺序 —— 先画不透明、再画透明；多个透明要按"离相机远→近"排
;;
;; ★为什么顺序重要：混合公式 out = src.a×src + (1−src.a)×dst 是**不对称**的，
;;   "红盖蓝"和"蓝盖红"结果不同。而且透明物要用到"背后已经画好的颜色"，
;;   所以背后的必须先画（画家算法：从远往近画）。
;;   正确：不透明 → 远玻璃(红) → 近玻璃(蓝)  → 重叠区是"蓝盖红"
;;   错误：不透明 → 近玻璃(蓝) → 远玻璃(红)  → 重叠区是"红盖蓝"（穿帮）
;;
;; 本步视觉：一块红色远玻璃 + 一块蓝色近玻璃，中间重叠。按 O 切换顺序，
;;   看重叠区的颜色变红还是变蓝——那就是"谁盖在谁上面"。
;; =========================================================

(require "lib-gui.rkt")
(require "lib.rkt")

(define start-ms (current-inexact-milliseconds))
(define far-first? (box #t))   ; #t = 远→近（正确），#f = 近→远（错误）

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
      [(or (eq? code #\o) (eq? code #\O))
       (set-box! far-first? (not (unbox far-first?)))
       (printf (if (unbox far-first?) "顺序：远→近（正确）~%" "顺序：近→远（错误，红盖蓝）~%"))]
      [(eq? code 'escape) (exit 0)])))

(define (draw)
  (define t (/ (- (current-inexact-milliseconds) start-ms) 1000.0))
  (define-values (w h) (send canvas get-gl-client-size))
  (define aspect (/ (exact->inexact w) (exact->inexact h)))
  (define P (mat4-perspective 45.0 aspect 0.1 100.0))
  (define V (mat4-look-at 0.0 0.0 5.0  0.0 0.0 0.0  0.0 1.0 0.0))

  (glEnable GL_BLEND)
  (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
  (glClearColor 0.06 0.07 0.12 1.0)
  (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
  (glUseProgram prog)
  (glBindVertexArray vao)

  ;; 不透明立方体（远处背景，写深度）
  (glUniform1f loc-alpha 1.0)
  (glUniform3f loc-tint 0.70 0.72 0.80)
  (define M (mat4-mult (mat4-translate 0.0 0.0 -2.0) (mat4-scale 0.8 0.8 0.8)))
  (glUniformMatrix4fv loc-mvp 1 #f (mat4-mult (mat4-mult P V) M))
  (glDrawElements GL_TRIANGLES 36 GL_UNSIGNED_SHORT 0)

  ;; 透明玻璃：关深度写入（本步重点在顺序，深度写入坑下一步讲）
  (glDepthMask #f)
  (glUniform1f loc-alpha 0.5)
  (define (glass tint z sz)
    (glUniform3f loc-tint (list-ref tint 0) (list-ref tint 1) (list-ref tint 2))
    (define G (mat4-mult (mat4-translate 0.0 0.0 z) (mat4-scale sz sz 0.02)))
    (glUniformMatrix4fv loc-mvp 1 #f (mat4-mult (mat4-mult P V) G))
    (glDrawElements GL_TRIANGLES 36 GL_UNSIGNED_SHORT 0))
  ;; 远玻璃红色（大一点，能看到边）、近玻璃蓝色（小一点，重叠区看顺序）
  (if (unbox far-first?)
      (begin (glass '(0.95 0.30 0.30) -1.0 1.3)   ; 远→近：正确
             (glass '(0.30 0.50 0.95)  1.0 1.0))
      (begin (glass '(0.30 0.50 0.95)  1.0 1.0)   ; 近→远：错误
             (glass '(0.95 0.30 0.30) -1.0 1.3)))
  (glDepthMask #t))

(define-values (frame canvas)
  (make-window #:title "12-02 渲染顺序（O 切换）"
               #:width 600 #:height 600 #:draw draw #:on-char on-char))

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
