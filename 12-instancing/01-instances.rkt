#lang racket/base
;; =========================================================
;; 12-instancing/01-instances.rkt —— 第一步：实例化基础
;; 运行：racket 12-instancing/01-instances.rkt    点 X = 退出
;; =========================================================
;; 画 100 个相同的立方体，普通做法是 100 次 draw call（每次都重新走一遍
;; CPU→GPU 的状态切换、重新上传数据）。本步让它变成 **1 次**。
;;
;; 本步新增（2 个，同属"实例化"这一件事）：
;;   ① glVertexAttribDivisor —— 属性"每个实例取一次"（默认是每个顶点取一次）
;;   ② glDrawElementsInstanced —— 把"画一次"变成"画 N 次"
;;
;; ★原理：CPU→GPU 是窄通道，能一次传完的绝不传 100 次。实例化的做法：
;;   几何（立方体的 8 个角 + 36 索引）只存一份，所有实例共享；
;;   "每个实例不同的部分"（这里是位置偏移）做成一个**实例数组**；
;;   GPU 画第 i 个实例时，自动取实例数组的第 i 行。
;;   属性除数 divisor：0 = 每顶点取一次（默认，位置/法线/uv 都是）；
;;                     1 = 每实例取一次（本课新增的 aOffset）。
;;
;; 本步视觉：10×10 = 100 个立方体排成方阵，一次 glDrawElementsInstanced 画完。
;; =========================================================

(require "lib-gui.rkt")
(require "lib.rkt")

(define N 100)   ; 10×10 个实例

(define vert-src
  (glsl (version 330 core)
        (layout (location 0) in vec3 aPos)      ; 共享：每顶点一份
        (layout (location 1) in vec3 aOffset)   ; 实例：每实例一份
        (uniform mat4 uVP)
        (define (main) void
          (vec3 p (+ (* aPos 0.45) aOffset))    ; 缩到 0.45，再摆到实例自己的位置
          (set! gl_Position (* uVP (vec4 p 1.0))))))

(define frag-src
  (glsl (version 330 core)
        (out vec4 FragColor)
        (define (main) void
          (set! FragColor (vec4 0.55 0.72 0.92 1.0)))))

;; 共享网格：单位立方体 8 顶点（只存一次），用 vec（8 个 vec3）写。
(define pos8
  (vec (vec3 -1.0 -1.0  1.0) (vec3  1.0 -1.0  1.0) (vec3  1.0  1.0  1.0) (vec3 -1.0  1.0  1.0)
       (vec3 -1.0 -1.0 -1.0) (vec3  1.0 -1.0 -1.0) (vec3  1.0  1.0 -1.0) (vec3 -1.0  1.0 -1.0)))
(define idx
  (u16vector 0 1 2  0 2 3  5 4 7  5 7 6  1 5 6  1 6 2
             4 0 3  4 3 7  3 2 6  3 6 7  4 5 1  4 1 0))

;; 实例数组：每实例 1 个 vec3 = 位置偏移(x, 0, z)，10×10 网格
(define inst
  (apply concat-vecs
         (for/list ([i (in-range N)])
           (define ix (exact->inexact (quotient i 10)))
           (define iz (exact->inexact (remainder i 10)))
           (vec3 (- (* ix 1.1) 4.95) 0.0 (- (* iz 1.1) 4.95)))))

(define (draw)
  (define-values (w h) (send canvas get-gl-client-size))
  (define aspect (/ (exact->inexact w) (exact->inexact h)))
  (define P (mat4-perspective 45.0 aspect 0.1 100.0))
  (define V (mat4-look-at 0.0 8.0 13.0  0.0 0.0 0.0  0.0 1.0 0.0))

  (glClearColor 0.05 0.06 0.10 1.0)
  (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
  (glUseProgram prog)
  (glUniformMatrix4fv loc-vp 1 #f (mat4-mult P V))
  (glBindVertexArray vao)
  ;; ★一次调用画 100 个（图元, 索引数, 索引类型, 起点, 实例数）
  (glDrawElementsInstanced GL_TRIANGLES 36 GL_UNSIGNED_SHORT 0 N))

(define-values (frame canvas)
  (make-window #:title "12-01 实例化基础（100 个立方体）" #:width 800 #:height 600 #:draw draw))

(define prog (send canvas with-gl-context (lambda () (build-program vert-src frag-src))))
(define loc-vp (send canvas with-gl-context (lambda () (glGetUniformLocation prog "uVP"))))
(define vao
  (send canvas with-gl-context
        (lambda ()
          (glEnable GL_DEPTH_TEST)
          (define v (u32vector-ref (glGenVertexArrays 1) 0))
          (glBindVertexArray v)
          ;; 共享几何：aPos 每顶点取一次（divisor 默认 0）
          (define vb (u32vector-ref (glGenBuffers 1) 0))
          (glBindBuffer GL_ARRAY_BUFFER vb)
          (glBufferData GL_ARRAY_BUFFER (gl-vector-sizeof (vec->f32vector pos8)) (vec->f32vector pos8) GL_STATIC_DRAW)
          (glVertexAttribPointer 0 (glsl-size 'vec3) GL_FLOAT #f (glsl-stride-bytes 'vec3) 0)
          (glEnableVertexAttribArray 0)
          ;; 索引
          (define eb (u32vector-ref (glGenBuffers 1) 0))
          (glBindBuffer GL_ELEMENT_ARRAY_BUFFER eb)
          (glBufferData GL_ELEMENT_ARRAY_BUFFER (gl-vector-sizeof idx) idx GL_STATIC_DRAW)
          ;; 实例数组：aOffset 每实例取一次
          (define ib (u32vector-ref (glGenBuffers 1) 0))
          (glBindBuffer GL_ARRAY_BUFFER ib)
          (glBufferData GL_ARRAY_BUFFER (gl-vector-sizeof inst) inst GL_STATIC_DRAW)
          (glVertexAttribPointer 1 (glsl-size 'vec3) GL_FLOAT #f (glsl-stride-bytes 'vec3) 0)
          (glVertexAttribDivisor 1 1)     ; ★每实例取一次
          (glEnableVertexAttribArray 1)
          (glBindVertexArray 0)
          v)))

(send frame show #t)
