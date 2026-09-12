#lang racket/base
;; =========================================================
;; 04-glsl-types/05-math-fns.rkt —— 第五步：内建数学函数
;; 运行：racket 04-glsl-types/05-math-fns.rkt    点 X = 退出
;; =========================================================
;; 上一步：会用字面量和运算符写表达式。本步认识 GLSL 自带的一批数学函数。
;;
;; 本步新增（2 组，同属"内建函数"这一件事）：
;;   ① 几何函数 —— length / normalize / dot
;;   ② 插值·钳制·波动 —— mix / clamp / fract / sin / cos
;;
;; ★为什么内置这么多数学函数：图形公式高频用到（算距离、混色、夹范围、造波纹），
;;   且要保证在 GPU 上实现一致——内置比手写更稳更快。它们是"纯函数"：
;;   输入值 → 输出值，正好契合"每个像素独立算"的模型。
;;
;; ★本步函数表（全部按分量/标量语义，照着读即可）：
;;   (length v)       = √(x²+y²+…) 向量长度 = 到原点的距离 ★同心圆的数学
;;   (normalize v)    = v / length(v)，同方向、长度缩到 1
;;   (dot a b)        = 逐分量相乘再相加（点积；投影/夹角，光照课 12 用）
;;   (mix a b t)      = 按 t(0..1) 在 a、b 间线性混（"lerp"）
;;   (clamp x lo hi)  = 把 x 夹回 [lo, hi]（小于 lo 变 lo，大于 hi 变 hi）
;;   (fract x)        = x - 向下取整(x)，只留小数部分 → 0..1 里循环
;;   (sin x) (cos x)  = 三角函数（造周期波动）
;;
;; 本步视觉（同心圆）：
;;   d = length(p)    每个像素到中心(0,0)的距离 → "半径"的数学
;;   fract(d*6)       半径 × 6 再取小数 → 沿半径每 1/6 循环一遍 0..1
;;   mix(深蓝, 亮蓝, ring)  按 ring 混两色 → 一圈一圈的环
;;   * (1 - 0.55*d)   越靠边乘得越小 → 边缘暗下去
;;   clamp(c,0,1)     保险：颜色夹回合法范围再输出
;; 你会发现：**同一半径上的像素颜色一样**——这就是"圆"被表达式写出来的过程。
;; =========================================================

(require "lib-gui.rkt")
(require "lib.rkt")

(define vert-src
  (glsl (version 330 core)
        (layout (location 0) in vec2 aPos)
        (layout (location 1) in vec2 aUV)
        (out vec2 vUV)
        (define (main) void
          (set! vUV aUV)
          (set! gl_Position (vec4 aPos 0.0 1.0)))))

;; 片元着色器（本步主角，逐行读）：
;;   (vec2 p (- (* vUV 2.0) 1.0))   屏幕中心变成坐标原点 (0,0)
;;   (float d (length p))           到原点的距离（本步核心：距离→圆）
;;   (float ring (fract (* d 6.0))) 距离压缩进 0..1 循环，密度 6
;;   (vec3 c (mix 深蓝 亮蓝 ring))   按 ring 混色 → 环
;;   (set! c (* c (- 1.0 (* 0.55 d))))  边缘衰减
;;   (clamp c 0.0 1.0)              夹回 [0,1]
(define frag-src
  (glsl (version 330 core)
        (in vec2 vUV)
        (out vec4 FragColor)
        (define (main) void
          (vec2 p (- (* vUV 2.0) 1.0))
          (float d (length p))
          (float ring (fract (* d 6.0)))
          (vec3 c (mix (vec3 0.10 0.15 0.40) (vec3 0.10 0.70 1.00) ring))
          (set! c (* c (- 1.0 (* 0.55 d))))
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
  (glClearColor 0.10 0.12 0.20 1.0)
  (glClear GL_COLOR_BUFFER_BIT)
  (glUseProgram prog)
  (glBindVertexArray vao)
  (glDrawArrays GL_TRIANGLES 0 6))

(define-values (frame canvas)
  (make-window #:title "04-05 内建数学函数" #:width 400 #:height 400 #:draw draw))

(define prog (send canvas with-gl-context (lambda () (build-program (GL_VERTEX_SHADER vert-src) (GL_FRAGMENT_SHADER frag-src)))))
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

(send frame show #t)
