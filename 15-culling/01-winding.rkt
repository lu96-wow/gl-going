#lang racket/base
;; =========================================================
;; 15-culling/01-winding.rkt —— 第一步：绕序 + 背面剔除
;; 运行：racket 15-culling/01-winding.rkt     C = 剔除开/关   点X = 退出
;; =========================================================
;; 封闭实体的"背面"永远看不见，画它纯浪费。本步学怎么在画之前就认出背面。
;;
;; 本步新增（2 个）：
;;   ① 绕序（winding order）—— 三角形顶点的 顺/逆时针 决定它是正面还是背面
;;   ② glEnable(GL_CULL_FACE) —— 开启背面剔除
;;
;; ★原理（为什么绕序能认出背面）：
;;   屏幕上一个三角形的顶点是顺时针(CW)还是逆时针(CCW)，取决于"从哪边看"——
;;   从外面看是逆时针，绕到背面看同一个三角形就变成顺时针。GPU 光栅化时
;;   一算绕序就知道片元在正面还是背面，背面的直接扔掉、不画。
;;   这就是"在画之前剔除"——省掉整条片元着色器管线，是纯性能优化。
;;   ★绕序错不会报错，只会"物体消失"——新手黑屏最常见的原因之一。
;;
;; 本步视觉：两张单面"纸片"。左(CCW，正常)可见，右(CW，故意造错)消失。
;;   C 键关剔除 → 右纸片出现（但那些注定看不见的面也被画了，白费）。
;; =========================================================

(require "lib-gui.rkt")
(require "lib.rkt")

(define cull-on? (box #t))

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

;; 一张纸片 = 两个三角形（索引 0 1 2、0 2 3），顶点顺序决定绕序
(define (quad-verts corners color)
  (apply concat-vecs
         (apply append
                (for/list ([c corners])
                  (list (vec3 (car c) (cadr c) 0.0) color)))))
(define idx (u16vector 0 1 2  0 2 3))

;; 左纸片：角走 左下→右下→右上→左上 = 逆时针(CCW) = 正面
(define ccw-corners (list (list -1.1 -0.75) (list 1.1 -0.75)
                          (list 1.1 0.75) (list -1.1 0.75)))
;; 右纸片：角走 0→3→2→1 = 顺时针(CW) = 背面
(define cw-corners (list (list-ref ccw-corners 0) (list-ref ccw-corners 3)
                         (list-ref ccw-corners 2) (list-ref ccw-corners 1)))

(define (on-char e)
  (define code (send e get-key-code))
  (when (not (eq? code 'release))
    (cond
      [(or (eq? code #\c) (eq? code #\C))
       (set-box! cull-on? (not (unbox cull-on?)))
       (printf (if (unbox cull-on?) "剔除 开（右纸片消失）~%" "剔除 关（右纸片出现，背面也在画）~%"))]
      [(eq? code 'escape) (exit 0)])))

(define (draw)
  (define-values (w h) (send canvas get-gl-client-size))
  (define aspect (/ (exact->inexact w) (exact->inexact h)))
  (define P (mat4-perspective 45.0 aspect 0.1 100.0))
  (define V (mat4-translate 0.0 0.0 -5.0))

  (if (unbox cull-on?) (glEnable GL_CULL_FACE) (glDisable GL_CULL_FACE))  ; ★剔除开关
  (glClearColor 0.08 0.09 0.14 1.0)
  (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
  (glUseProgram prog)
  (define (draw-quad x yaw vao)
    (glUniformMatrix4fv loc-mvp 1 #f
                         (mat4-mult (mat4-mult P V)
                                       (mat4-mult (mat4-translate x 0.0 0.0) (mat4-rot-y yaw))))
    (glBindVertexArray vao)
    (glDrawElements GL_TRIANGLES 6 GL_UNSIGNED_SHORT 0))
  (draw-quad -1.7 -15.0 vao-ccw)   ; 左：CCW，可见
  (draw-quad  1.7  15.0 vao-cw))   ; 右：CW，被剔除

(define-values (frame canvas)
  (make-window #:title "15-01 绕序与背面剔除（C 切换）"
               #:width 800 #:height 600 #:draw draw #:on-char on-char))

(define prog (send canvas with-gl-context (lambda () (build-program vert-src frag-src))))
(define loc-mvp (send canvas with-gl-context (lambda () (glGetUniformLocation prog "uMVP"))))

(define (make-vao verts)
  (send canvas with-gl-context
        (lambda ()
          (glEnable GL_DEPTH_TEST)
          (define vbo (u32vector-ref (glGenBuffers 1) 0))
          (glBindBuffer GL_ARRAY_BUFFER vbo)
          (glBufferData GL_ARRAY_BUFFER (gl-vector-sizeof verts) verts GL_STATIC_DRAW)
          (define v (u32vector-ref (glGenVertexArrays 1) 0))
          (glBindVertexArray v)
          (glVertexAttribPointer 0 (glsl-size 'vec3) GL_FLOAT #f (glsl-stride-bytes 'vec3 'vec3) 0)
          (glEnableVertexAttribArray 0)
          (glVertexAttribPointer 1 (glsl-size 'vec3) GL_FLOAT #f (glsl-stride-bytes 'vec3 'vec3) (glsl-stride-bytes 'vec3))
          (glEnableVertexAttribArray 1)
          (define ebo (u32vector-ref (glGenBuffers 1) 0))
          (glBindBuffer GL_ELEMENT_ARRAY_BUFFER ebo)
          (glBufferData GL_ELEMENT_ARRAY_BUFFER (gl-vector-sizeof idx) idx GL_STATIC_DRAW)
          (glBindVertexArray 0)
          v)))
(define vao-ccw (make-vao (quad-verts ccw-corners (vec3 0.20 0.55 0.95))))
(define vao-cw  (make-vao (quad-verts cw-corners  (vec3 0.95 0.55 0.20))))

(send frame show #t)
(send canvas focus)
