#lang racket/base
;; =========================================================
;; 06-transform/03-order.rkt —— 第三步：矩阵乘法 + T·R·S 顺序
;; 运行：racket 06-transform/03-order.rkt    点 X = 退出
;; =========================================================
;; 前两步：平移/旋转/缩放各用一个矩阵。本步把它们**乘成一个矩阵**，并看清
;; 乘法的顺序陷阱。
;;
;; 本步新增（2 个，同属"复合变换"这一件事）：
;;   矩阵乘法 —— 把多个变换合成一个矩阵
;;   顺序不交换 —— T·R·S（先缩放再旋转最后平移）才是对的
;;
;; ★矩阵乘法 = 变换的复合：A·B 作用在顶点上是"先 B 后 A"（从右往左读）。
;;   M = T·R·S 表示：先 S 缩放 → 再 R 旋转 → 最后 T 平移。口诀：
;;   "先缩放，再旋转，最后平移"。
;;
;; ★为什么不交换（本步的核心）：矩阵乘法不满足交换律，A·B ≠ B·A。
;;   两个方块用同一组 T/R/S，只是乘法顺序不同：
;;     左 = T·R·S（对）  ：先缩放旋转（绕原点）→ 再平移到目标点
;;                         → 方块**绕自己中心**转
;;     右 = R·T·S（错）  ：先缩放 → 再平移 → 最后旋转（绕原点！）
;;                         → 方块被"甩出去"，绕屏幕原点画圈
;;   同样的三个矩阵，顺序反了行为完全不同——这就是矩阵不交换的直观后果。
;;
;; 本步视觉：左方块原地自转（对），右方块绕原点公转（错）。
;; =========================================================

(require "lib-gui.rkt")
(require "lib.rkt")

(define PI (acos -1.0))
(define start-ms (current-inexact-milliseconds))

(define vert-src
  (glsl (version 330 core)
        (layout (location 0) in vec2 aPos)
        (uniform mat4 uMVP)
        (define (main) void
          (set! gl_Position (* uMVP (vec4 aPos 0.0 1.0))))))

(define frag-src
  (glsl (version 330 core)
        (uniform vec3 uColor)
        (out vec4 FragColor)
        (define (main) void
          (set! FragColor (vec4 uColor 1.0)))))

;; 平移 / 旋转 / 缩放（前两步裸写过的，本步一起用）
(define (m4-translate tx ty)
  (f64vector 1.0 0.0 0.0 0.0   0.0 1.0 0.0 0.0   0.0 0.0 1.0 0.0   tx ty 0.0 1.0))
(define (m4-rot-z deg)
  (define r (* (/ PI 180.0) deg))
  (define c (cos r)) (define s (sin r))
  (f64vector c s 0.0 0.0   (- s) c 0.0 0.0   0.0 0.0 1.0 0.0   0.0 0.0 0.0 1.0))
(define (m4-scale sx sy)
  (f64vector sx 0.0 0.0 0.0   0.0 sy 0.0 0.0   0.0 0.0 1.0 0.0   0.0 0.0 0.0 1.0))

;; 矩阵乘法 A·B（裸写，本步主角）。列主序：元素 (r 行, c 列) 存下标 c*4+r。
;; R[c][r] = Σ_k A[k][r] · B[c][k]（即 A 的第 r 行 × B 的第 c 列）。
(define (m4-mult A B)
  (define R (make-f64vector 16 0.0))
  (for* ([c (in-range 4)] [r (in-range 4)] [k (in-range 4)])
    (f64vector-set! R (+ (* 4 c) r)
                    (+ (f64vector-ref R (+ (* 4 c) r))
                       (* (f64vector-ref A (+ (* 4 k) r))
                          (f64vector-ref B (+ (* 4 c) k))))))
  R)

(define verts (vec (vec2 -0.5 -0.5) (vec2 0.5 -0.5) (vec2 0.5 0.5) (vec2 -0.5 0.5)))
(define idx   (u16vector 0 1 2  0 2 3))

;; 画一个方块：给定合成矩阵 M 和颜色
(define (draw-square M r g b)
  (glUniformMatrix4fv loc-mvp 1 #f (mat4 M))
  (glUniform3f loc-color r g b)
  (glDrawElements GL_TRIANGLES 6 GL_UNSIGNED_SHORT 0))

(define (draw)
  (define t (/ (- (current-inexact-milliseconds) start-ms) 1000.0))
  (define ang (* t 90.0))
  (glClearColor 0.07 0.08 0.14 1.0)
  (glClear GL_COLOR_BUFFER_BIT)
  (glUseProgram prog)
  (glBindVertexArray vao)

  ;; 左：T·R·S（先缩放旋转再平移）→ 绕自己中心转
  (define T-L (m4-translate -0.5 0.0))
  (define R   (m4-rot-z ang))
  (define S   (m4-scale 0.5 0.5))
  (draw-square (m4-mult T-L (m4-mult R S)) 0.30 0.65 0.95)

  ;; 右：R·T·S（旋转在平移之后）→ 绕原点甩出去
  (define T-R (m4-translate 0.5 0.0))
  (draw-square (m4-mult R (m4-mult T-R S)) 0.95 0.70 0.30))

(define-values (frame canvas)
  (make-window #:title "06-03 顺序（T·R·S vs R·T·S）" #:width 400 #:height 400 #:draw draw))

(define prog (send canvas with-gl-context (lambda () (build-program vert-src frag-src))))
(define loc-mvp   (send canvas with-gl-context (lambda () (glGetUniformLocation prog "uMVP"))))
(define loc-color (send canvas with-gl-context (lambda () (glGetUniformLocation prog "uColor"))))
(define vao
  (send canvas with-gl-context
        (lambda ()
          (define vbo (u32vector-ref (glGenBuffers 1) 0))
          (glBindBuffer GL_ARRAY_BUFFER vbo)
          (glBufferData GL_ARRAY_BUFFER (gl-vector-sizeof (vec->f32vector verts)) (vec->f32vector verts) GL_STATIC_DRAW)
          (define v (u32vector-ref (glGenVertexArrays 1) 0))
          (glBindVertexArray v)
          (glVertexAttribPointer 0 (glsl-size 'vec2) GL_FLOAT #f (glsl-stride-bytes 'vec2) 0)
          (glEnableVertexAttribArray 0)
          (define ebo (u32vector-ref (glGenBuffers 1) 0))
          (glBindBuffer GL_ELEMENT_ARRAY_BUFFER ebo)
          (glBufferData GL_ELEMENT_ARRAY_BUFFER (gl-vector-sizeof idx) idx GL_STATIC_DRAW)
          (glBindVertexArray 0)
          v)))

(define ticker
  (new timer% (interval 16)
       (notify-callback (lambda () (send canvas refresh)))))

(send frame show #t)
