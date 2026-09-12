#lang racket/base
;; =========================================================
;; 09-camera/05-demo.rkt —— 第五步：综合，完整的轨道相机
;; 运行：racket 09-camera/05-demo.rkt
;; 操作：左键拖 = 环绕   滚轮 = 拉近拉远   R = 复位   ESC/点X = 退出
;; =========================================================
;; 本课前四步：视图矩阵(01)、球坐标轨道(02)、鼠标交互(03)、收进 lib(04)。
;; 本步**不引入新语法**，把老教程 07-camera 的成品拼出来。
;;
;; 场景：网格地面 + 中央翻滚立方体 + 三颗散落的小立方体。
;; 拖动鼠标环绕、滚轮拉近拉远——你会直观体会到：
;;   ★物体没动，是"你的眼睛"在绕。CPU 每帧只改 V，场景代码完全不知道相机。
;; =========================================================

(require "lib-gui.rkt")
(require "lib.rkt")

(define PI (acos -1.0))
(define start-ms (current-inexact-milliseconds))

(define vert-src
  (glsl (version 330 core)
        (layout (location 0) in vec3 aPos)
        (layout (location 1) in vec3 aColor)
        (uniform mat4 uMVP)
        (out vec3 vColor)
        (define (main) void
          (set! vColor aColor)
          (set! gl_Position (* uMVP (vec4 aPos 1.0))))))

(define frag-src
  (glsl (version 330 core)
        (in vec3 vColor)
        (out vec4 FragColor)
        (define (main) void
          (set! FragColor (vec4 vColor 1.0)))))

(define grid (grid-verts 5.0 0.5))
(define grid-count (quotient (f32vector-length grid) 6))

(define yaw (box 30.0)) (define pitch (box 20.0)) (define dist (box 9.0))
(define drag? (box #f)) (define last-px (box 0.0)) (define last-py (box 0.0))

(define (on-event e)
  (define et (send e get-event-type))
  (cond
    [(send e button-down? 'left)
     (set-box! drag? #t)
     (set-box! last-px (send e get-x))
     (set-box! last-py (send e get-y))]
    [(send e button-up? 'left) (set-box! drag? #f)]
    [(eq? et 'motion)
     (when (unbox drag?)
       (define px (send e get-x))
       (define py (send e get-y))
       (define sens 0.25)
       (set-box! yaw (+ (unbox yaw) (* (- px (unbox last-px)) sens)))
       (set-box! pitch (max -85.0 (min 85.0
                                        (+ (unbox pitch) (* (- (unbox last-py) py) sens)))))
       (set-box! last-px px)
       (set-box! last-py py))]))

(define (on-char e)
  (define code (send e get-key-code))
  (when (not (eq? code 'release))
    (cond
      ;; ★滚轮是"键盘事件"（key-event%），不是鼠标事件：在 on-char 里处理，
      ;;   key-code 是 'wheel-up / 'wheel-down。
      [(eq? code 'wheel-up)   (set-box! dist (max 2.5 (min 20.0 (* (unbox dist) 0.88))))]
      [(eq? code 'wheel-down) (set-box! dist (max 2.5 (min 20.0 (* (unbox dist) 1.12))))]
      [(or (eq? code #\r) (eq? code #\R))
       (set-box! yaw 30.0) (set-box! pitch 20.0) (set-box! dist 9.0)]
      [(eq? code 'escape) (exit 0)])))

(define (draw)
  (define t (/ (- (current-inexact-milliseconds) start-ms) 1000.0))
  (define-values (w h) (send canvas get-gl-client-size))
  (define aspect (/ (exact->inexact w) (exact->inexact h)))
  (define P (mat4-perspective 45.0 aspect 0.1 100.0))
  (define ph (* (/ PI 180.0) (unbox pitch)))
  (define ya (* (/ PI 180.0) (unbox yaw)))
  (define ex (* (unbox dist) (cos ph) (sin ya)))
  (define ey (* (unbox dist) (sin ph)))
  (define ez (* (unbox dist) (cos ph) (cos ya)))
  (define V (mat4-look-at ex ey ez  0.0 0.0 0.0  0.0 1.0 0.0))

  (glClearColor 0.07 0.08 0.14 1.0)
  (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
  (glUseProgram prog)

  ;; 统一入口：给定模型矩阵画立方体
  (define (draw-cube m)
    (glUniformMatrix4fv loc-mvp 1 #f (mat4-mult (mat4-mult P V) m))
    (glBindVertexArray vao-cube)
    (glDrawElements GL_TRIANGLES 36 GL_UNSIGNED_SHORT 0))

  ;; 地面网格
  (glUniformMatrix4fv loc-mvp 1 #f (mat4-mult P V))
  (glBindVertexArray vao-grid)
  (glDrawArrays GL_LINES 0 grid-count)

  ;; 中央翻滚立方体（缩到 0.8，避免旋转时吞掉旁边小立方体）
  (draw-cube (mat4-mult (mat4-translate 0.0 1.0 0.0)
                      (mat4-mult (mat4-mult (mat4-rot-y (* t 60.0)) (mat4-rot-x (* t 40.0)))
                               (mat4-scale 0.8 0.8 0.8))))
  ;; 三颗散落小立方体
  (for ([p (list (list 2.4 0.5 -1.7) (list -2.5 0.6 -1.3) (list 1.7 0.4 2.1))])
    (draw-cube (mat4-mult (mat4-translate (car p) (cadr p) (caddr p))
                        (mat4-mult (mat4-rot-y (* t 90.0)) (mat4-scale 0.5 0.5 0.5))))))

(define-values (frame canvas)
  (make-window #:title "09-05 完整轨道相机"
               #:width 800 #:height 600
               #:draw draw
               #:on-event on-event
               #:on-char on-char))

(define prog (send canvas with-gl-context (lambda () (build-program (GL_VERTEX_SHADER vert-src) (GL_FRAGMENT_SHADER frag-src)))))
(define loc-mvp (send canvas with-gl-context (lambda () (glGetUniformLocation prog "uMVP"))))
(define vao-grid
  (send canvas with-gl-context
        (lambda ()
          (glEnable GL_DEPTH_TEST)
          (define vbo (u32vector-ref (glGenBuffers 1) 0))
          (glBindBuffer GL_ARRAY_BUFFER vbo)
          (glBufferData GL_ARRAY_BUFFER (gl-vector-sizeof grid) grid GL_STATIC_DRAW)
          (define v (u32vector-ref (glGenVertexArrays 1) 0))
          (glBindVertexArray v)
          (glVertexAttribPointer 0 (glsl-size 'vec3) GL_FLOAT #f (glsl-stride-bytes 'vec3 'vec3) 0)
          (glEnableVertexAttribArray 0)
          (glVertexAttribPointer 1 (glsl-size 'vec3) GL_FLOAT #f (glsl-stride-bytes 'vec3 'vec3) (glsl-stride-bytes 'vec3))
          (glEnableVertexAttribArray 1)
          (glBindVertexArray 0)
          v)))
(define vao-cube
  (send canvas with-gl-context
        (lambda ()
          (define vbo (u32vector-ref (glGenBuffers 1) 0))
          (glBindBuffer GL_ARRAY_BUFFER vbo)
          (glBufferData GL_ARRAY_BUFFER (gl-vector-sizeof cube-verts) cube-verts GL_STATIC_DRAW)
          (define v (u32vector-ref (glGenVertexArrays 1) 0))
          (glBindVertexArray v)
          (glVertexAttribPointer 0 (glsl-size 'vec3) GL_FLOAT #f (glsl-stride-bytes 'vec3 'vec3) 0)
          (glEnableVertexAttribArray 0)
          (glVertexAttribPointer 1 (glsl-size 'vec3) GL_FLOAT #f (glsl-stride-bytes 'vec3 'vec3) (glsl-stride-bytes 'vec3))
          (glEnableVertexAttribArray 1)
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
