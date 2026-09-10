#lang racket/base
;; =========================================================
;; 06-transform/06-demo.rkt —— 第六步：综合，像素世界里的旋转与公转
;; 运行：racket 06-transform/06-demo.rkt    点 X = 退出
;; =========================================================
;; 本课前五步：平移(01)、旋转缩放(02)、矩阵乘法与顺序(03)、收进 lib(04)、
;; 正交投影像素世界(05)。本步**不引入新语法**，把老教程 05-transform 的
;; 成品拼出来，顺带把 05 步裸写的 m4-ortho 也收进了 lib.rkt。
;;
;; 画面（800×600 像素世界）：
;;   中央大矩形：绕自己中心自转
;;   右上角小方块：绕大矩形中心公转 + 自己自转
;;   中心小红点：标记大矩形中心
;;
;; 每画一个物体，就是给它拼一个模型矩阵 M = T·R·S（先缩放再旋转最后平移），
;; 再乘上投影 P，一次上传。所有"位置/朝向/大小"都在这一个矩阵里。
;; =========================================================

(require "lib-gui.rkt")
(require "lib.rkt")     ; 现在 m4-* 全在 lib 里了（含 m4-ortho）

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

;; 单位方块：中心在原点、边长 1
(define verts (vec (vec2 -0.5 -0.5) (vec2 0.5 -0.5) (vec2 0.5 0.5) (vec2 -0.5 0.5)))
(define idx   (u16vector 0 1 2  0 2 3))

(define (draw)
  (define t (/ (- (current-inexact-milliseconds) start-ms) 1000.0))
  (define-values (w h) (send canvas get-gl-client-size))
  (define P (m4-ortho 0.0 (exact->inexact w) (exact->inexact h) 0.0 -1.0 1.0))

  ;; 画一个"单位方块"：给中心(cx,cy)、半宽半高(hx,hy)、角度、颜色
  (define (quad cx cy hx hy ang r g b)
    (define S (m4-scale (* 2.0 hx) (* 2.0 hy)))
    (define R (m4-rot-z ang))
    (define T (m4-translate cx cy))
    (define M (m4-mult T (m4-mult R S)))   ; T·R·S（先缩放再旋转最后平移）
    (glUniformMatrix4fv loc-mvp 1 #f (mat4 (m4-mult P M)))
    (glUniform3f loc-color r g b)
    (glDrawElements GL_TRIANGLES 6 GL_UNSIGNED_SHORT 0))

  (glClearColor 0.07 0.08 0.14 1.0)
  (glClear GL_COLOR_BUFFER_BIT)
  (glUseProgram prog)
  (glBindVertexArray vao)

  ;; 中央大矩形：绕自己中心转
  (quad (/ w 2.0) (/ h 2.0) 150.0 90.0 (* t 55.0) 0.30 0.65 0.95)
  ;; 右上角小方块：公转（绕大矩形中心）+ 自转
  (define a (* t 95.0))
  (define rad (* (/ PI 180.0) a))
  (define ox (+ (/ w 2.0) (* 200.0 (cos rad))))
  (define oy (+ (/ h 2.0) (* 150.0 (sin rad))))
  (quad ox oy 34.0 34.0 (* t 200.0) 0.95 0.70 0.30)
  ;; 大矩形中心的小标记
  (quad (/ w 2.0) (/ h 2.0) 6.0 6.0 0.0 1.0 0.3 0.3))

(define-values (frame canvas)
  (make-window #:title "06-06 旋转与公转（综合）" #:width 800 #:height 600 #:draw draw))

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
