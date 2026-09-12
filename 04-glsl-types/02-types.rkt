#lang racket/base
;; =========================================================
;; 04-glsl-types/02-types.rkt —— 第二步：GLSL 的类型 + 构造器
;; 运行：racket 04-glsl-types/02-types.rkt    点 X = 退出
;; =========================================================
;; 上一步：画布换成了铺满窗口的四边形，每个像素拿到自己的 vUV。
;; 本步停下来，把 GLSL 的**类型**和**构造器**一次讲清楚。
;;
;; 本步新增（2 个）：
;;   ① 类型总览 —— float / int / bool / vec2 / vec3 / vec4
;;   ② 构造器"升维" —— (vec3 vUV 0.5) 用 vec2 + 标量拼出 vec3
;;
;; ★类型表（本步起照着这个表读 shader）：
;;   float   浮点。★字面量必须带小数点：1.0、0.5（写 1 是 int，不是 float）
;;   int     整数。字面量不带小数点：1、2（-1 也是 int）
;;   bool    真假：true / false（比较运算会产出 bool，05 课 if 里真正用起来）
;;   vec2    2 个 float 一组；vec3 = 3 个；vec4 = 4 个
;;   向量存在的理由：图形数据天然是"小向量"（位置、颜色、法线）。向量类型让
;;   + - * / 等运算**逐分量一次写完**，不用写三遍循环——这正是 GPU 最擅长的。
;;   （mat3/mat4 矩阵留给 07 课变换；sampler2D 纹理留给 10 课，先认识名字即可。）
;;
;; ★构造器 = 类型名当函数用，把分量拼成一个向量：
;;   (vec3 0.5 0.2 0.8)        三个标量 → vec3
;;   (vec3 vUV 0.5)            vec2 + 标量 → vec3（"升维"：先填 vUV 的两个，
;;                             不够的分量再补 0.5）★这就是本步要演示的
;;   (vec4 c 1.0)              vec3 + 标量 → vec4
;;   (vec4 vUV 0.5 1.0)        vec2 + 两个标量 → vec4
;;   规则：只要"分量总数凑齐"，可以任意用 标量/小向量 拼大向量。
;;
;; 本步视觉：上一步 FragColor = (vUV, 0, 1) 蓝通道是 0。本步把颜色先拼成
;;   vec3 c = (vUV.x, vUV.y, 0.5)，蓝通道变成固定 0.5 → 整个画面罩一层蓝，
;;   和上一步的黑/红/绿/黄对比，能看出"第三通道"进来了。
;; =========================================================

(require "lib-gui.rkt")
(require "lib.rkt")

;; 顶点着色器（同 01 步，本课后面每步都复用，不再逐行解释）
(define vert-src
  (glsl (version 330 core)
        (layout (location 0) in vec2 aPos)
        (layout (location 1) in vec2 aUV)
        (out vec2 vUV)
        (define (main) void
          (set! vUV aUV)
          (set! gl_Position (vec4 aPos 0.0 1.0)))))

;; 片元着色器（本步主角）：
;;   (vec3 c (vec3 vUV 0.5))
;;      类型是 vec3、名字 c、初值用构造器升维：
;;      vUV 是 vec2，再补一个 0.5 标量 → 凑成 3 个分量。
;;      展开后就是 vec3 c = vec3(vUV, 0.5);
;;   (set! FragColor (vec4 c 1.0))
;;      vec3 + 标量 1.0 → vec4（第 4 个分量是不透明度 alpha，1.0 = 完全不透明）。
(define frag-src
  (glsl (version 330 core)
        (in vec2 vUV)
        (out vec4 FragColor)
        (define (main) void
          (vec3 c (vec3 vUV 0.5))
          (set! FragColor (vec4 c 1.0)))))

;; 打印展开出的真实 GLSL——S 表达式和 GLSL 一一对应，照这个对照读。
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
  (make-window #:title "04-02 类型与构造器" #:width 400 #:height 400 #:draw draw))

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
