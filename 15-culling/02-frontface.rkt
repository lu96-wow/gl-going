#lang racket/base
;; =========================================================
;; 15-culling/02-frontface.rkt —— 第二步：glFrontFace（正面只是约定）
;; 运行：racket 15-culling/02-frontface.rkt     F = 翻转正面绕序   点X = 退出
;; =========================================================
;; 上一步：默认"逆时针 = 正面"，右纸片(顺时针)被剔掉。但"哪种绕序算正面"
;; 其实只是**约定**——本步把它翻过来。
;;
;; 本步新增（1 个）：
;;   glFrontFace —— 声明哪种绕序算正面（GL_CCW 或 GL_CW）
;;
;; ★为什么要能翻：不同的建模软件、不同的网格，绕序约定可能不一样。GL 默认
;;   逆时针(CCW)为正面；如果你的网格恰好是顺时针的，不必重新生成数据，直接
;;   glFrontFace(GL_CW) 告诉 GPU"这次顺时针算正面"。绕序没有对错，只有
;;   "你声明的是什么、数据就是什么"。
;;
;; 本步视觉：还是那两张纸片。按 F 翻转正面定义：
;;   正面=CCW → 左(CCW)可见、右(CW)消失
;;   正面=CW  → 左消失、右出现（"正面"的定义反了，可见性就对调）
;; =========================================================

(require "lib-gui.rkt")
(require "lib.rkt")

(define front-ccw? (box #t))

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

(define (quad-verts corners color)
  (apply concat-vecs
         (apply append
                (for/list ([c corners])
                  (list (vec3 (car c) (cadr c) 0.0) color)))))
(define idx (u16vector 0 1 2  0 2 3))

(define ccw-corners (list (list -1.1 -0.75) (list 1.1 -0.75)
                          (list 1.1 0.75) (list -1.1 0.75)))
(define cw-corners (list (list-ref ccw-corners 0) (list-ref ccw-corners 3)
                         (list-ref ccw-corners 2) (list-ref ccw-corners 1)))

(define (on-char e)
  (define code (send e get-key-code))
  (when (not (eq? code 'release))
    (cond
      [(or (eq? code #\f) (eq? code #\F))
       (set-box! front-ccw? (not (unbox front-ccw?)))
       (printf (if (unbox front-ccw?) "正面 = 逆时针 CCW（左显右隐）~%"
                   "正面 = 顺时针 CW（对调：左隐右显）~%"))]
      [(eq? code 'escape) (exit 0)])))

(define (draw)
  (define-values (w h) (send canvas get-gl-client-size))
  (define aspect (/ (exact->inexact w) (exact->inexact h)))
  (define P (mat4-perspective 45.0 aspect 0.1 100.0))
  (define V (mat4-translate 0.0 0.0 -5.0))

  (glEnable GL_CULL_FACE)
  (glFrontFace (if (unbox front-ccw?) GL_CCW GL_CW))   ; ★正面绕序的约定
  (glClearColor 0.08 0.09 0.14 1.0)
  (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
  (glUseProgram prog)
  (define (draw-quad x yaw vao)
    (glUniformMatrix4fv loc-mvp 1 #f
                         (mat4-mult (mat4-mult P V)
                                       (mat4-mult (mat4-translate x 0.0 0.0) (mat4-rot-y yaw))))
    (glBindVertexArray vao)
    (glDrawElements GL_TRIANGLES 6 GL_UNSIGNED_SHORT 0))
  (draw-quad -1.7 -15.0 vao-ccw)
  (draw-quad  1.7  15.0 vao-cw))

(define-values (frame canvas)
  (make-window #:title "15-02 正面绕序（F 切换）"
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
