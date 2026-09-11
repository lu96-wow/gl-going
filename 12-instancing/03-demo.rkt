#lang racket/base
;; =========================================================
;; 12-instancing/03-demo.rkt —— 第三步：综合，起浪的实例方阵
;; 运行：racket 12-instancing/03-demo.rkt    点 X = 退出
;; =========================================================
;; 本课前两步：实例化基础(01)、每实例多属性与起浪(02)。
;; 本步**不引入新语法**，把老教程 11-instancing 的成品拼出来。
;;
;; 场景：10×10 = 100 个立方体排成方阵，每个按自己的相位起浪、颜色随位置
;;   渐变，相机缓缓环绕。全部用**一次 glDrawElementsInstanced** 画完——
;;   这就是"大量相同物体"的标准做法。
;; =========================================================

(require "lib-gui.rkt")
(require "lib.rkt")

(define PI (acos -1.0))
(define start-ms (current-inexact-milliseconds))
(define N 100)

(define vert-src
  (glsl (version 330 core)
        (layout (location 0) in vec3 aPos)
        (layout (location 1) in vec3 aOffset)
        (layout (location 2) in vec3 aColor)
        (layout (location 3) in float aPhase)
        (uniform mat4 uVP)
        (uniform float uTime)
        (out vec3 vColor)
        (define (main) void
          (set! vColor aColor)
          (float bob (* 0.35 (sin (+ (* uTime 2.0) aPhase))))
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
;; （= GLSL 的 struct Instance；字段名/类型写一次，数据 + stride/offset 都从它推导）
(define-glsl-struct Instance
  (offset vec3)
  (color  vec3)
  (phase  float))

(define inst
  (apply concat-vecs
         (for/list ([i (in-range N)])
           (define ix (exact->inexact (quotient i 10)))
           (define iz (exact->inexact (remainder i 10)))
           (Instance->f32vector
            (Instance (vec3 (- (* ix 1.1) 4.95) 0.0 (- (* iz 1.1) 4.95))
                      (vec3 (+ 0.15 (* 0.5 (/ ix 9.0)))
                            (+ 0.25 (* 0.55 (/ iz 9.0)))
                            0.9)
                      (+ (* 1.7 ix) (* 2.3 iz)))))))

(define (draw)
  (define t (/ (- (current-inexact-milliseconds) start-ms) 1000.0))
  (define-values (w h) (send canvas get-gl-client-size))
  (define aspect (/ (exact->inexact w) (exact->inexact h)))
  (define P (mat4-perspective 45.0 aspect 0.1 100.0))
  ;; 相机缓缓环绕
  (define ca (* (/ PI 180.0) (* t 12.0)))
  (define V (mat4-look-at (* 11.0 (sin ca)) 7.0 (* 11.0 (cos ca))
                        0.0 0.0 0.0  0.0 1.0 0.0))

  (glClearColor 0.05 0.06 0.10 1.0)
  (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
  (glUseProgram prog)
  (glUniformMatrix4fv loc-vp 1 #f (mat4-mult P V))
  (glUniform1f loc-time t)
  (glBindVertexArray vao)
  (glDrawElementsInstanced GL_TRIANGLES 36 GL_UNSIGNED_SHORT 0 N))

(define-values (frame canvas)
  (make-window #:title "12-03 实例化（综合）" #:width 800 #:height 600 #:draw draw))

(define prog (send canvas with-gl-context (lambda () (build-program vert-src frag-src))))
(define loc-vp   (send canvas with-gl-context (lambda () (glGetUniformLocation prog "uVP"))))
(define loc-time (send canvas with-gl-context (lambda () (glGetUniformLocation prog "uTime"))))
(define vao
  (send canvas with-gl-context
        (lambda ()
          (glEnable GL_DEPTH_TEST)
          (define v (u32vector-ref (glGenVertexArrays 1) 0))
          (glBindVertexArray v)
          (define vb (u32vector-ref (glGenBuffers 1) 0))
          (glBindBuffer GL_ARRAY_BUFFER vb)
          (glBufferData GL_ARRAY_BUFFER (gl-vector-sizeof (vec->f32vector pos8)) (vec->f32vector pos8) GL_STATIC_DRAW)
          (glVertexAttribPointer 0 (glsl-size 'vec3) GL_FLOAT #f (glsl-stride-bytes 'vec3) 0)
          (glEnableVertexAttribArray 0)
          (define eb (u32vector-ref (glGenBuffers 1) 0))
          (glBindBuffer GL_ELEMENT_ARRAY_BUFFER eb)
          (glBufferData GL_ELEMENT_ARRAY_BUFFER (gl-vector-sizeof idx) idx GL_STATIC_DRAW)
          (define ib (u32vector-ref (glGenBuffers 1) 0))
          (glBindBuffer GL_ARRAY_BUFFER ib)
          (glBufferData GL_ARRAY_BUFFER (gl-vector-sizeof inst) inst GL_STATIC_DRAW)
          ;; ★实例数组每行 7 float（偏移 xyz + 颜色 rgb + 相位 w），stride/size/offset 从 Instance 推导
          (glVertexAttribPointer 1 (Instance-field-size 'offset) GL_FLOAT #f (Instance-stride) (Instance-field-offset 'offset))
          (glVertexAttribDivisor 1 1)
          (glEnableVertexAttribArray 1)
          (glVertexAttribPointer 2 (Instance-field-size 'color) GL_FLOAT #f (Instance-stride) (Instance-field-offset 'color))
          (glVertexAttribDivisor 2 1)
          (glEnableVertexAttribArray 2)
          (glVertexAttribPointer 3 (Instance-field-size 'phase) GL_FLOAT #f (Instance-stride) (Instance-field-offset 'phase))
          (glVertexAttribDivisor 3 1)
          (glEnableVertexAttribArray 3)
          (glBindVertexArray 0)
          v)))

(define ticker
  (new timer% (interval 16)
       (notify-callback (lambda () (send canvas refresh)))))

(send frame show #t)
