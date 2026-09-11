#lang racket/base
;; =========================================================
;; 12-instancing/02-animate.rkt —— 第二步：每实例多属性 + 起浪
;; 运行：racket 12-instancing/02-animate.rkt    点 X = 退出
;; =========================================================
;; 上一步：每个实例只有一个偏移属性。本步让每个实例带上**自己的颜色和相位**，
;; 方阵就"活"起来了。
;;
;; 本步新增（1 组，同属"实例数组"这一件事）：
;;   每实例多属性 —— 位置(3) + 颜色(3) + 相位(1) = 每行 7 个 float
;;   ★stride 陷阱：实例数组每行 7 float，属性 stride 必须 = 7×4 = 28 字节。
;;     如果 CPU 写的数据行和 stride 对不上，读出来的偏移全是串位的乱值，
;;     画面会莫名堆成几根线/几个点。这是实例化最容易踩的坑。
;;
;; 本步视觉：10×10 立方体，每实例按"时间 + 自己的相位"上下起浪，颜色随
;;   网格位置渐变（红随 x、绿随 z）。
;; =========================================================

(require "lib-gui.rkt")
(require "lib.rkt")

(define PI (acos -1.0))
(define start-ms (current-inexact-milliseconds))
(define N 100)

(define vert-src
  (glsl (version 330 core)
        (layout (location 0) in vec3 aPos)     ; 共享
        (layout (location 1) in vec3 aOffset)  ; 每实例：位置
        (layout (location 2) in vec3 aColor)   ; 每实例：颜色
        (layout (location 3) in float aPhase)  ; 每实例：相位
        (uniform mat4 uVP)
        (uniform float uTime)
        (out vec3 vColor)
        (define (main) void
          (set! vColor aColor)
          (float bob (* 0.35 (sin (+ (* uTime 2.0) aPhase))))   ; ★各自起浪
          (vec3 p (+ (+ (* aPos 0.45) aOffset) (vec3 0.0 bob 0.0)))
          (set! gl_Position (* uVP (vec4 p 1.0))))))

(define frag-src
  (glsl (version 330 core)
        (in vec3 vColor)
        (out vec4 FragColor)
        (define (main) void
          (set! FragColor (vec4 vColor 1.0)))))

(define pos8
  (vec (vec3 -1.0 -1.0  1.0) (vec3  1.0 -1.0  1.0) (vec3  1.0  1.0  1.0) (vec3 -1.0  1.0  1.0)
       (vec3 -1.0 -1.0 -1.0) (vec3  1.0 -1.0 -1.0) (vec3  1.0  1.0 -1.0) (vec3 -1.0  1.0 -1.0)))
(define idx
  (u16vector 0 1 2  0 2 3  5 4 7  5 7 6  1 5 6  1 6 2
             4 0 3  4 3 7  3 2 6  3 6 7  4 5 1  4 1 0))

;; 实例 struct：与 shader 的 3 个实例 attribute 一一对应
;; （= GLSL 的 struct instance；字段名/类型写一次，数据 + stride/offset 都从它推导）
(glsl-struct instance
  (vec3 offset)
  (vec3 color)
  (float phase))

(define inst
  (apply concat-vecs
         (for/list ([i (in-range N)])
           (define ix (exact->inexact (quotient i 10)))
           (define iz (exact->inexact (remainder i 10)))
           (instance->f32vector
            (instance (vec3 (- (* ix 1.1) 4.95) 0.0 (- (* iz 1.1) 4.95))
                      (vec3 (+ 0.15 (* 0.5 (/ ix 9.0)))
                            (+ 0.25 (* 0.55 (/ iz 9.0)))
                            0.9)
                      (+ (* 1.7 ix) (* 2.3 iz)))))))

(define (draw)
  (define t (/ (- (current-inexact-milliseconds) start-ms) 1000.0))
  (define-values (w h) (send canvas get-gl-client-size))
  (define aspect (/ (exact->inexact w) (exact->inexact h)))
  (define P (mat4-perspective 45.0 aspect 0.1 100.0))
  (define V (mat4-look-at 0.0 8.0 13.0  0.0 0.0 0.0  0.0 1.0 0.0))

  (glClearColor 0.05 0.06 0.10 1.0)
  (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
  (glUseProgram prog)
  (glUniformMatrix4fv loc-vp 1 #f (mat4-mult P V))
  (glUniform1f loc-time t)
  (glBindVertexArray vao)
  (glDrawElementsInstanced GL_TRIANGLES 36 GL_UNSIGNED_SHORT 0 N))

(define-values (frame canvas)
  (make-window #:title "12-02 每实例属性（起浪）" #:width 800 #:height 600 #:draw draw))

(define prog (send canvas with-gl-context (lambda () (build-program vert-src frag-src))))
(define loc-vp   (send canvas with-gl-context (lambda () (glGetUniformLocation prog "uVP"))))
(define loc-time (send canvas with-gl-context (lambda () (glGetUniformLocation prog "uTime"))))
(define vao
  (send canvas with-gl-context
        (lambda ()
          (glEnable GL_DEPTH_TEST)
          (define v (u32vector-ref (glGenVertexArrays 1) 0))
          (glBindVertexArray v)
          ;; 共享几何
          (define vb (u32vector-ref (glGenBuffers 1) 0))
          (glBindBuffer GL_ARRAY_BUFFER vb)
          (glBufferData GL_ARRAY_BUFFER (gl-vector-sizeof (vec->f32vector pos8)) (vec->f32vector pos8) GL_STATIC_DRAW)
          (glVertexAttribPointer 0 (glsl-size 'vec3) GL_FLOAT #f (glsl-stride-bytes 'vec3) 0)
          (glEnableVertexAttribArray 0)
          (define eb (u32vector-ref (glGenBuffers 1) 0))
          (glBindBuffer GL_ELEMENT_ARRAY_BUFFER eb)
          (glBufferData GL_ELEMENT_ARRAY_BUFFER (gl-vector-sizeof idx) idx GL_STATIC_DRAW)
          ;; 实例数组：★stride 必须 = 7×4 = 28 字节，和数据行一致
          (define ib (u32vector-ref (glGenBuffers 1) 0))
          (glBindBuffer GL_ARRAY_BUFFER ib)
          (glBufferData GL_ARRAY_BUFFER (gl-vector-sizeof inst) inst GL_STATIC_DRAW)
          ;; ★实例数组每行 7 float（偏移 xyz + 颜色 rgb + 相位 w），stride/size/offset 从 instance 推导
          (glVertexAttribPointer 1 (instance-field-size 'offset) GL_FLOAT #f (instance-stride) (instance-field-offset 'offset))
          (glVertexAttribDivisor 1 1)
          (glEnableVertexAttribArray 1)
          (glVertexAttribPointer 2 (instance-field-size 'color) GL_FLOAT #f (instance-stride) (instance-field-offset 'color))
          (glVertexAttribDivisor 2 1)
          (glEnableVertexAttribArray 2)
          (glVertexAttribPointer 3 (instance-field-size 'phase) GL_FLOAT #f (instance-stride) (instance-field-offset 'phase))
          (glVertexAttribDivisor 3 1)
          (glEnableVertexAttribArray 3)
          (glBindVertexArray 0)
          v)))

(define ticker
  (new timer% (interval 16)
       (notify-callback (lambda () (send canvas refresh)))))

(send frame show #t)
