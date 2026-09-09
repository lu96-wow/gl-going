#lang racket/base
;; =========================================================
;; 14-culling.rkt —— 背面剔除：绕序（winding）与正面
;; 运行：racket 14-culling.rkt
;;   C = 背面剔除 开/关    F = 正面绕序 CCW↔CW    R = 纸片自转 开/关
;;   ESC = 退出
;; =========================================================
;; 性能思路：封闭实体（立方体/地形）的"背面"永远看不见——画它纯浪费。
;; 如何在画之前就知道哪面是背面？看三角形的**绕序**：
;;
;;   新 API：
;;     glEnable(GL_CULL_FACE)    开启剔除（默认剔除背面 GL_BACK）
;;     glFrontFace(GL_CCW / GL_CW)  声明"哪种绕序算正面"
;;
;; 图形原理：屏幕上一个三角形的顶点是顺时针还是逆时针(CCW)由"从哪边看"
;; 决定——从外面看是逆时针，翻到背面看同一个三角形就是顺时针。
;; GPU 光栅化时一算绕序就知道片元在正面还是背面，背面的直接扔掉。
;;
;; ★绕序错不会报错，只会"物体消失"——新手黑屏最常见原因之一。
;; 本课用两张单面"纸片"把这件事演出来：
;;   左纸片(CCW)：正常   右纸片(CW)：故意造错
;;   ① 初始（剔除开、正面=CCW）：左可见，右消失
;;   ② C 关剔除：右出现 → 但那些注定看不见的面也在被画
;;   ③ F 翻转"正面"定义成 CW：右出现、左消失 → “正面”只是约定
;; =========================================================

(require racket/gui opengl)
(require "lib.rkt")

(define PI (acos -1.0))
(define start-ms (current-inexact-milliseconds))
(define cull-on? (box #t))
(define front-ccw? (box #t))
(define spin? (box #f))

(define vert-src
  (glsl-pretty
   (glsl
    (version 330 core)
    (layout (location 0) in vec3 aPos)
    (layout (location 1) in vec3 aColor)
    (uniform mat4 uMVP)
    (out vec3 vColor)
    (define (main) void
      (set! vColor aColor)
      (set! gl_Position (* uMVP (vec4 aPos 1.0)))))))
(define frag-src
  (glsl-pretty
   (glsl
    (version 330 core)
    (in vec3 vColor)
    (out vec4 FragColor)
    (define (main) void
      (set! FragColor (vec4 vColor 1.0))))))

(define cfg (new gl-config%))
(send cfg set-legacy? #f)
(send cfg set-double-buffered #t)
(send cfg set-depth-size 1)
(define frame
  (new (class frame%
         (augment* [on-close (lambda () (exit 0))])
         (super-new))
       (label "14 背面剔除") (width 800) (height 600)))

(define fw (box 800)) (define fh (box 600))
(define init? (box #f))
(define prog #f) (define loc-mvp 0)
(define vao-ccw 0) (define vao-cw 0)

(define canvas
  (new (class canvas%
         (inherit with-gl-context swap-gl-buffers)
         (define/override (on-size w h)
           (with-gl-context
            (lambda ()
              (define-values (gw gh) (send this get-gl-client-size))
              (set-box! fw gw) (set-box! fh gh)
              (glViewport 0 0 gw gh)
              (glEnable GL_DEPTH_TEST)
              (glClearColor 0.08 0.09 0.14 1.0))))
         (define/override (on-char e)
           (define code (send e get-key-code))
           (when (not (eq? code 'release))
             (cond
               [(eq? code 'escape) (exit 0)]
               [(or (eq? code #\c) (eq? code #\C))
                (set-box! cull-on? (not (unbox cull-on?)))
                (printf (if (unbox cull-on?) "剔除 开（右纸片消失）~%" "剔除 关（右纸片出现，背面也在画）~%"))]
               [(or (eq? code #\f) (eq? code #\F))
                (set-box! front-ccw? (not (unbox front-ccw?)))
                (printf (if (unbox front-ccw?) "正面 = 逆时针 CCW（左显右隐）~%"
                            "正面 = 顺时针 CW（对调：左隐右显）~%"))]
               [(or (eq? code #\r) (eq? code #\R))
                (set-box! spin? (not (unbox spin?)))
                (printf (if (unbox spin?) "自转 开（纸片转到背面就消失）~%" "自转 关~%"))])))
         (define/override (on-paint)
           (with-gl-context
            (lambda ()
              (unless (unbox init?)
                (set-box! init? #t)
                (set! prog (build-program vert-src frag-src))
                (set! loc-mvp (glGetUniformLocation prog "uMVP"))
                ;; 一张纸片 = 两个三角形（CCW：左下、右下、右上、左上）
                (define w 1.1) (define h 0.75)
                (define ccw (list (list (- w) (- h)) (list w (- h))
                                  (list w h) (list (- w) h)))
                (define (make-quad corners color)
                  (define verts
                    (apply f32vector
                           (apply append
                                  (for/list ([c corners])
                                    (list (car c) (cadr c) 0.0
                                          (car color) (cadr color) (caddr color))))))
                  (define idx (u16vector 0 1 2 0 2 3))
                  (define vao (u32vector-ref (glGenVertexArrays 1) 0))
                  (glBindVertexArray vao)
                  (define vb (u32vector-ref (glGenBuffers 1) 0))
                  (glBindBuffer GL_ARRAY_BUFFER vb)
                  (glBufferData GL_ARRAY_BUFFER (gl-vector-sizeof verts) verts GL_STATIC_DRAW)
                  (define s6 (* 6 4))
                  (glVertexAttribPointer 0 3 GL_FLOAT #f s6 0)
                  (glEnableVertexAttribArray 0)
                  (glVertexAttribPointer 1 3 GL_FLOAT #f s6 (* 3 4))
                  (glEnableVertexAttribArray 1)
                  (define eb (u32vector-ref (glGenBuffers 1) 0))
                  (glBindBuffer GL_ELEMENT_ARRAY_BUFFER eb)
                  (glBufferData GL_ELEMENT_ARRAY_BUFFER (gl-vector-sizeof idx) idx GL_STATIC_DRAW)
                  (glBindVertexArray 0)
                  vao)
                (set! vao-ccw (make-quad ccw '(0.20 0.55 0.95)))      ; 左：蓝
                ;; 右纸片绕序翻转：角走 0→3→2→1 = 顺时针
                (set! vao-cw (make-quad (list (list-ref ccw 0) (list-ref ccw 3)
                                               (list-ref ccw 2) (list-ref ccw 1))
                                        '(0.95 0.55 0.20))))          ; 右：橙

              (define t (/ (- (current-inexact-milliseconds) start-ms) 1000.0))
              (define gw (unbox fw)) (define gh (unbox fh))
              (define aspect (/ (exact->inexact gw) (exact->inexact gh)))
              (define P (m4-perspective 45.0 aspect 0.1 100.0))
              (define V (m4-translate 0.0 0.0 -5.0))

              (if (unbox cull-on?) (glEnable GL_CULL_FACE) (glDisable GL_CULL_FACE))
              (glFrontFace (if (unbox front-ccw?) GL_CCW GL_CW))
              (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
              (glUseProgram prog)
              (define (draw-quad-at x yaw vao)
                (glUniformMatrix4fv loc-mvp 1 #f
                                    (mat4 (m4-mult (m4-mult P V)
                                                      (m4-mult (m4-translate x 0.0 0.0)
                                                               (m4-rot-y yaw)))))
                (glBindVertexArray vao)
                (glDrawElements GL_TRIANGLES 6 GL_UNSIGNED_SHORT 0))
              (draw-quad-at -1.7 (if (unbox spin?) (* t 45.0) -15.0) vao-ccw)
              (draw-quad-at  1.7 15.0 vao-cw)

              (send this swap-gl-buffers))))
         (super-new))
       (style '(gl no-autoclear))
       (gl-config cfg)
       (parent frame)))

(define ticker
  (new timer% (interval 16)
       (notify-callback
        (lambda () (send canvas refresh)))))
(send frame show #t)
(send canvas focus)
