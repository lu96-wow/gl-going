#lang racket/base
;; =========================================================
;; 07-transform/04-lib.rkt —— 第四步：把矩阵工具收进 lib.rkt
;; 运行：racket 07-transform/04-lib.rkt    点 X = 退出
;; =========================================================
;; 前 3 步：平移/旋转/缩放/乘法 四个矩阵函数每次都要重新裸写一遍——纯重复。
;; 本步把它们收进 lib.rkt（本步创建，见同文件夹 lib.rkt 的 mat4-* 部分），
;; 之后每课直接用 mat4-translate / mat4-rot-z / mat4-scale / mat4-mult / mat4-identity。
;;
;; ★节奏（整门课通用，这里再走一遍）：
;;   ① 裸写一遍新机制（前 3 步，矩阵的数字亲手写过才懂）
;;   ② 发现它重复 → 收进 lib（本步）
;;   ③ 后面的课直接调用，聚焦本课真正的新东西
;;
;; ★约定（收进 lib 时定死，后面都遵守）：
;;   矩阵 = 列主序 mat4 = f32vector[16]（元素 (r行,c列) 存下标 c*4+r），数学用 f32
;;   保精度；上传前用 (mat4 ...) 直接上传（GL 要 float）。
;;
;; 本步演示和 03 步完全一样（左 = T·R·S 对，右 = R·T·S 错），只是代码变短了：
;;   03 步要自己写 4 个矩阵函数，本步直接用 lib 里的。
;; =========================================================

(require "lib-gui.rkt")
(require "lib.rkt")     ; 现在里面有 mat4-* 了

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

(define verts (vec (vec2 -0.5 -0.5) (vec2 0.5 -0.5) (vec2 0.5 0.5) (vec2 -0.5 0.5)))
(define idx   (u16vector 0 1 2  0 2 3))

(define (draw-square M r g b)
  (glUniformMatrix4fv loc-mvp 1 #f M)
  (glUniform3f loc-color r g b)
  (glDrawElements GL_TRIANGLES 6 GL_UNSIGNED_SHORT 0))

(define (draw)
  (define t (/ (- (current-inexact-milliseconds) start-ms) 1000.0))
  (define R (mat4-rot-z (* t 90.0)))
  (define S (mat4-scale 0.5 0.5))
  (glClearColor 0.07 0.08 0.14 1.0)
  (glClear GL_COLOR_BUFFER_BIT)
  (glUseProgram prog)
  (glBindVertexArray vao)
  ;; 左：T·R·S（先缩放旋转再平移）→ 绕自己中心转
  (draw-square (mat4-mult (mat4-translate -0.5 0.0) (mat4-mult R S)) 0.30 0.65 0.95)
  ;; 右：R·T·S（旋转在平移之后）→ 绕原点甩出去
  (draw-square (mat4-mult R (mat4-mult (mat4-translate 0.5 0.0) S)) 0.95 0.70 0.30))

(define-values (frame canvas)
  (make-window #:title "07-04 用 lib 的 mat4-*" #:width 400 #:height 400 #:draw draw))

(define prog (send canvas with-gl-context (lambda () (build-program (GL_VERTEX_SHADER vert-src) (GL_FRAGMENT_SHADER frag-src)))))
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
