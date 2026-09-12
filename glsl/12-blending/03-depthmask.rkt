#lang racket/base
;; =========================================================
;; 12-blending/03-depthmask.rkt —— 第三步：深度写入坑（glDepthMask）
;; 运行：racket glsl/12-blending/03-depthmask.rkt     D = 切换深度写入   点X = 退出
;; =========================================================
;; 上一步：会排顺序了。但还有一个坑：透明物**写不写深度**。
;;
;; 本步新增（1 个）：
;;   glDepthMask —— 关掉深度写入（画透明物时通常设 GL_FALSE）
;;
;; ★为什么：深度测试默认"画过就写深度"。如果一块透明玻璃先画、并写下了
;;   自己的深度，那么后面画的、离相机更远的透明物就会被深度测试挡住——
;;   "更远"自然过不了"更近"的深度 → 背后的透明物整个消失，混不进来。
;;   所以画透明时关掉深度写入：透明物仍然"被深度测试挡住别人"（不写深度，
;;   只读别人的），但自己不挡后面更远的东西。
;;
;; 本步视觉：近处蓝玻璃 + 远处红玻璃（都在一块立方体背景前）。按 D 切换：
;;   深度写入开 → 蓝玻璃写了深度，红玻璃被挡住、消失（错误）
;;   深度写入关 → 红玻璃透出来（正确）
;; =========================================================

(require "lib-gui.rkt")
(require "lib.rkt")

(define start-ms (current-inexact-milliseconds))
(define depth-mask? (box #f))   ; #f = 关深度写入（正确），#t = 开（错误）

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
      [(or (eq? code #\d) (eq? code #\D))
       (set-box! depth-mask? (not (unbox depth-mask?)))
       (printf (if (unbox depth-mask?)
                   "深度写入 开（红玻璃被挡、消失——错误）~%"
                   "深度写入 关（红玻璃透出来——正确）~%"))]
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

  ;; 不透明立方体（背景，正常写深度）
  (glDepthMask #t)
  (glUniform1f loc-alpha 1.0)
  (glUniform3f loc-tint 0.70 0.72 0.80)
  (define M (mat4-mult (mat4-translate 0.0 0.0 -2.0) (mat4-scale 0.8 0.8 0.8)))
  (glUniformMatrix4fv loc-mvp 1 #f (mat4-mult (mat4-mult P V) M))
  (glDrawElements GL_TRIANGLES 36 GL_UNSIGNED_SHORT 0)

  ;; 透明玻璃：★深度写入按 D 切换
  (glDepthMask (unbox depth-mask?))
  (glUniform1f loc-alpha 0.5)
  (define (glass tint z sz)
    (glUniform3f loc-tint (list-ref tint 0) (list-ref tint 1) (list-ref tint 2))
    (define G (mat4-mult (mat4-translate 0.0 0.0 z) (mat4-scale sz sz 0.02)))
    (glUniformMatrix4fv loc-mvp 1 #f (mat4-mult (mat4-mult P V) G))
    (glDrawElements GL_TRIANGLES 36 GL_UNSIGNED_SHORT 0))
  ;; 先画近的蓝玻璃，再画远的红玻璃（暴露深度写入坑）
  (glass '(0.30 0.50 0.95)  1.0 1.2)   ; 近
  (glass '(0.95 0.30 0.30) -1.0 1.4)   ; 远（在蓝玻璃后面）
  (glDepthMask #t))

(define-values (frame canvas)
  (make-window #:title "12-03 深度写入（D 切换）"
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
