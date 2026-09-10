#lang racket/base
;; =========================================================
;; 04-animate/03-branch.rkt —— 第三步：分情况画（when/unless/cond）
;; 运行：racket 04-animate/03-branch.rkt    点 X = 退出
;; =========================================================
;; 上一步：圈圈动了。但到现在 shader 都是"一条公式算到底"。本步学分支——
;; 让 shader 能"看情况走不同的路"。
;;
;; 本步新增（1 组，同属"分支语句"这一件事）：
;;   when   —— 条件为真时执行体（没有 else）
;;   unless —— 条件为假时执行体（when 的反面）
;;   cond   —— 多分支（编译成 else if 链）
;;
;; ★语法（DSL 写成 S 表达式，展开成 GLSL 的 if/else if）：
;;   (when 条件 语句...)          →  if (条件) { ... }
;;   (unless 条件 语句...)        →  if (!(条件)) { ... }
;;   (cond [条件1 语句...]        →  if (条件1) { ... }
;;         [条件2 语句...]              else if (条件2) { ... }
;;         [else 语句...])              else { ... }
;;   条件里的 (and a b) (or a b) (not a) 是 03 课学过的逻辑运算符。
;;   ★注意：04 课 if 是"语句"（挑一段代码执行），03 课的三元 ?: 是"表达式"
;;   （挑一个值）。两者名字像，用途不同。
;;
;; 本步视觉：在动的圈圈右上角，用 (when ...) 挖出一块"色板"——把那块区域的
;;   坐标 p 直接当颜色涂（红随 x、绿随 y、蓝随时间波）。这直观证明：
;;   "像素只是数据，shader 是对它算公式"。整块区域 = when 的分支。
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

;; 片元着色器（本步主角）：
;;   前面的圈圈同 02 步；新增最后一段 (when ...)：
;;   (and (> (x p) 0.15) (> (y p) 0.15))  = "p.x 和 p.y 都大于 0.15"
;;     也就是右上角那块区域（其余三个区域至少一个不满足）
;;   体里把 c 改成"坐标当颜色"：红随 x、绿随 y、蓝随时间波
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
  (make-window #:title "04-03 右上角色板（分支）" #:width 400 #:height 400 #:draw draw))

(define prog (send canvas with-gl-context (lambda () (build-program vert-src frag-src))))
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
