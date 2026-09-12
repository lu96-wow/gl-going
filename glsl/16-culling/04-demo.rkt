#lang racket/base
;; =========================================================
;; 16-culling/04-demo.rkt —— 第四步：综合，背面剔除演示
;; 运行：racket glsl/16-culling/04-demo.rkt
;;   C = 剔除开/关   F = 正面绕序 CCW↔CW   R = 纸片自转开/关   ESC/点X = 退出
;; =========================================================
;; 本课前三步：绕序与剔除(01)、glFrontFace(02)、可见性谱系(03)。
;; 本步**不引入新语法**，把老教程 14-culling 的成品拼出来——三键自由切换，
;; 把"绕序决定正反面"这件事玩明白。
;;
;;   左纸片(CCW，正常) 蓝    右纸片(CW，故意造错) 橙
;;   ① 初始（剔除开、正面=CCW）：左可见、右消失
;;   ② C 关剔除：右出现 → 那些注定看不见的面也在被画（浪费）
;;   ③ F 翻转正面定义：右出现、左消失 → "正面"只是约定
;;   ④ R 自转：纸片转到背面朝相机时消失
;; =========================================================

(require "lib-gui.rkt")
(require "lib.rkt")

(define start-ms (current-inexact-milliseconds))
(define cull-on? (box #t))
(define front-ccw? (box #t))
(define spin? (box #f))

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
      [(or (eq? code #\c) (eq? code #\C))
       (set-box! cull-on? (not (unbox cull-on?)))
       (printf (if (unbox cull-on?) "剔除 开（右纸片消失）~%" "剔除 关（右纸片出现，背面也在画）~%"))]
      [(or (eq? code #\f) (eq? code #\F))
       (set-box! front-ccw? (not (unbox front-ccw?)))
       (printf (if (unbox front-ccw?) "正面 = 逆时针 CCW（左显右隐）~%"
                   "正面 = 顺时针 CW（对调：左隐右显）~%"))]
      [(or (eq? code #\r) (eq? code #\R))
       (set-box! spin? (not (unbox spin?)))
       (printf (if (unbox spin?) "自转 开（转到背面就消失）~%" "自转 关~%"))]
      [(eq? code 'escape) (exit 0)])))

(define (draw)
  (define t (/ (- (current-inexact-milliseconds) start-ms) 1000.0))
  (define-values (w h) (send canvas get-gl-client-size))
  (define aspect (/ (exact->inexact w) (exact->inexact h)))
  (define P (mat4-perspective 45.0 aspect 0.1 100.0))
  (define V (mat4-translate 0.0 0.0 -5.0))

  (if (unbox cull-on?) (glEnable GL_CULL_FACE) (glDisable GL_CULL_FACE))
  (glFrontFace (if (unbox front-ccw?) GL_CCW GL_CW))
  (glClearColor 0.08 0.09 0.14 1.0)
  (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
  (glUseProgram prog)
  (define (draw-quad x yaw vao)
    (glUniformMatrix4fv loc-mvp 1 #f
                         (mat4-mult (mat4-mult P V)
                                       (mat4-mult (mat4-translate x 0.0 0.0) (mat4-rot-y yaw))))
    (glBindVertexArray vao)
    (glDrawElements GL_TRIANGLES 6 GL_UNSIGNED_SHORT 0))
  (draw-quad -1.7 (if (unbox spin?) (* t 45.0) -15.0) vao-ccw)
  (draw-quad  1.7 15.0 vao-cw))

(define-values (frame canvas)
  (make-window #:title "16-04 背面剔除（综合）"
               #:width 800 #:height 600 #:draw draw #:on-char on-char))

(define prog (send canvas with-gl-context (lambda () (build-program (GL_VERTEX_SHADER vert-src) (GL_FRAGMENT_SHADER frag-src)))))
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

(define ticker
  (new timer% (interval 16)
       (notify-callback (lambda () (send canvas refresh)))))

(send frame show #t)
(send canvas focus)
