#lang racket/base
;; =========================================================
;; 05-animate/06-shadertoy.rkt —— 第六步：综合，会动的着色玩具
;; 运行：racket 05-animate/06-shadertoy.rkt    点 X = 退出
;; =========================================================
;; 本课前五步分别讲了：timer 让帧发生(01)、uniform 传时间(02)、分支 when(03)、
;; for 循环(04)、自写函数(05)。本步**不引入任何新语法**，把"动起来 + 分支"
;; 全部串进一个完整作品里。
;;
;; 逐行读（每一行都能在前几课找到出处）：
;;   uniform uTime              = 02 步：CPU 每帧传来的时间
;;   p = vUV*2-1                = 04 课算术：像素坐标，中心 (0,0)
;;   d = length(p)              = 04 课几何函数：到中心的距离 → 同心圆
;;   ring = fract(d*6 - uTime)  = 04 课 fract + 02 步减时间 → 环往外跑
;;   c = mix(深蓝,亮蓝,ring)     = 04 课混色：给环上色
;;   c *= (1 - 0.55*d)          = 算术：越靠边越暗（写成了 set! 形式）
;;   c += vec3(0.15)*(0.5+0.5*sin(uTime*2))  = sin 随时间呼吸：整体明暗起伏
;;   (when (p.x>0.15 且 p.y>0.15) ...)       = 03 步分支：右上角挖出"色板"
;;      色板里把坐标 p 直接当颜色            = 像素只是数据、shader 是表达式
;;   clamp(c,0,1)               = 04 课钳制：夹回合法颜色
;;
;; 你现在的 shader 已经"活"了：环在跑、亮度在呼吸、右上角有一块随坐标变色的
;; 实时调色板。这就是老教程 02-glsl-basics 的完整成品——它只用到了 GLSL 的
;; 类型、表达式、uniform 和一句分支。
;; =========================================================

(require "lib-gui.rkt")
(require "lib.rkt")

(define start-ms (current-inexact-milliseconds))

(define vert-src
  (glsl (version 330 core)
        (layout (location 0) in vec2 aPos)
        (layout (location 1) in vec2 aUV)
        (out vec2 vUV)
        (define (main) void
          (set! vUV aUV)
          (set! gl_Position (vec4 aPos 0.0 1.0)))))

;; 片元着色器：本课全部知识的综合（会动的圈圈 + 呼吸 + 右上角色板）
(define frag-src
  (glsl (version 330 core)
        (in vec2 vUV)
        (uniform float uTime)
        (out vec4 FragColor)
        (define (main) void
          (vec2 p (- (* vUV 2.0) 1.0))
          (float d (length p))
          (float ring (fract (- (* d 6.0) uTime)))
          (vec3 c (mix (vec3 0.10 0.15 0.40) (vec3 0.10 0.70 1.00) ring))
          (set! c (* c (- 1.0 (* 0.55 d))))
          (set! c (+ c (* (vec3 0.15) (+ 0.5 (* 0.5 (sin (* uTime 2.0)))))))
          (when (and (> (x p) 0.15) (> (y p) 0.15))
            (set! c (vec3 (+ (* (x p) 0.5) 0.5)
                          (+ (* (y p) 0.5) 0.5)
                          (+ 0.5 (* 0.4 (sin (+ uTime (* (x p) 3.0))))))))
          (set! FragColor (vec4 (clamp c 0.0 1.0) 1.0)))))

(printf "片元着色器展开为：\n~a\n" (glsl-pretty frag-src))

(define verts
  (vec (vec4 -1.0 -1.0  0.0 0.0)
       (vec4  1.0 -1.0  1.0 0.0)
       (vec4  1.0  1.0  1.0 1.0)
       (vec4 -1.0 -1.0  0.0 0.0)
       (vec4  1.0  1.0  1.0 1.0)
       (vec4 -1.0  1.0  0.0 1.0)))

(define (draw)
  (define t (/ (- (current-inexact-milliseconds) start-ms) 1000.0))
  (glClearColor 0.10 0.12 0.20 1.0)
  (glClear GL_COLOR_BUFFER_BIT)
  (glUseProgram prog)
  (glUniform1f loc-time t)
  (glBindVertexArray vao)
  (glDrawArrays GL_TRIANGLES 0 6))

(define-values (frame canvas)
  (make-window #:title "05-06 会动的着色玩具" #:width 400 #:height 400 #:draw draw))

(define prog (send canvas with-gl-context (lambda () (build-program (GL_VERTEX_SHADER vert-src) (GL_FRAGMENT_SHADER frag-src)))))
(define loc-time (send canvas with-gl-context (lambda () (glGetUniformLocation prog "uTime"))))
(define vao
  (send canvas with-gl-context
        (lambda ()
          (define vbo (u32vector-ref (glGenBuffers 1) 0))
          (glBindBuffer GL_ARRAY_BUFFER vbo)
          (glBufferData GL_ARRAY_BUFFER (gl-vector-sizeof (vec->f32vector verts)) (vec->f32vector verts) GL_STATIC_DRAW)
          (define v (u32vector-ref (glGenVertexArrays 1) 0))
          (glBindVertexArray v)
          (glVertexAttribPointer 0 2 GL_FLOAT #f 16 0)
          (glEnableVertexAttribArray 0)
          (glVertexAttribPointer 1 2 GL_FLOAT #f 16 8)
          (glEnableVertexAttribArray 1)
          (glBindVertexArray 0)
          v)))

(define ticker
  (new timer% (interval 16)
       (notify-callback
        (lambda () (send canvas refresh)))))

(send frame show #t)
