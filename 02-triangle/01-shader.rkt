#lang racket/base
;; =========================================================
;; 02-triangle/01-shader.rkt —— 第一步：着色器是什么（顶点 / 片元）
;; 运行：racket 02-triangle/01-shader.rkt
;; =========================================================
;; 01 课只会"清屏"。要画出东西，先理解现代 OpenGL 最核心的新概念：着色器。
;; 本步新增（2 个概念，同属"着色器"这一件事）：
;;   顶点着色器（vertex shader）
;;   片元着色器（fragment shader）
;;
;; ★一句话：着色器 = 跑在 GPU 上的小程序。core profile 里 GPU 不再内置
;;   "怎么画"（旧式固定管线被删了），你必须自己写两段小程序，GPU 照着跑。
;;
;; ★为什么要两段（GPU 的流水线）：
;;
;;   顶点数据 ─▶ [顶点着色器] ─▶ 图元装配 ─▶ 光栅化 ─▶ [片元着色器] ─▶ 屏幕
;;               每顶点跑一次      拼成三角形    三角形变像素    每像素跑一次
;;
;;   顶点着色器：每个顶点跑一次。输入 = 这个顶点自己的数据（如位置 aPos）；
;;               输出 = 这个顶点最终落在哪（必须写 gl_Position，裁剪坐标）。
;;   片元着色器：每个像素跑一次。输入 = 顶点着色器传下来的值（GPU 自动插值）；
;;               输出 = 这个像素的颜色。
;;
;; ★为什么这样设计：GPU 有成千上万个并行单元，同一份代码、不同数据、同时执行。
;;   所以 GLSL 被设计成"无全局状态、一次只处理一个顶点/片元"的小函数。
;;   顶点着色器算完位置 → GPU 把三角形"铺"成像素（光栅化）→ 片元着色器给
;;   每个像素上色。两段分工，就是图形管线。
;;
;; ── GLSL 里的变量分两种（这个区分后面每课都用）──────────────
;;   ① 内置变量：GLSL 规定死的名字，GPU 认识它，不能自己起别的名。
;;      本课只有一个：gl_Position —— 顶点着色器必须给它赋值，
;;      告诉 GPU"这个顶点落在裁剪坐标哪里"（-1..1 的 NDC）。
;;   ② 用户自定义变量：名字自己起，靠"声明"把数据接起来。
;;      本课两个（04 步还会加一个）：
;;        aPos      —— in（输入 attribute）：每个顶点一份的数据，挂在 0 号槽
;;        FragColor —— out（输出）：片元颜色；只有一个 out 时名字随便起
;;      （04 步加 vPos：out/in 对接，顶点算的值自动插值后交给片元）
;;
;; 下面把两段着色器写出来（S 表达式，展开成 GLSL），逐行读一遍。
;; 注意：本步只"读"不"编译"——怎么把文本变成 GPU 能跑的程序，是 03 课（渲染管线）的主题。
;; =========================================================

(require "lib-gui.rkt")
(require "../racket-glsl/rewrite.rkt")   ; (glsl ...) 宏 + glsl-pretty

;; 顶点着色器（逐行读）：
;;   (version 330 core)               版本声明：330 = GLSL 3.3，core = 现代
;;   (layout (location 0) in vec2 aPos)
;;       in   = 输入 attribute（"每个顶点一份"的数据，跟着顶点走）
;;       aPos = 用户自定义名字（②类），声明成 vec2 类型
;;       vec2 = 2 个 float 组成的向量（GLSL 的类型；下一课专门讲）
;;       ★易混：Racket 侧还有个同名函数 vec2（racket-glsl 提供），是造
;;         f32vector 顶点数据的构造器——和 shader 里的 vec2 类型是两个世界的
;;         东西，名字故意对应（02 步上传数据时细说）
;;       location 0 = 这份数据挂在"0 号槽"上——Racket 侧上传数据时用同一个
;;                    数字对齐（03 步 VAO 会回来用这个数字）
;;   (define (main) void ...)         入口固定叫 main；void = 不返回任何值
;;   (set! gl_Position (vec4 aPos 0.0 1.0))
;;       gl_Position = ★内置变量（①类）：顶点着色器必须给它赋值，
;;                     值 = 裁剪坐标（-1..1 的 NDC）
;;       (vec4 aPos 0.0 1.0) = 把 2 分量位置补成 4 分量（齐次坐标，07 课讲）
(define vert-src
  (glsl (version 330 core)
        (layout (location 0) in vec2 aPos)
        (define (main) void
          (set! gl_Position (vec4 aPos 0.0 1.0)))))

;; 片元着色器（逐行读）：
;;   (out vec4 FragColor)     out = 输出。片元着色器输出"这个像素的颜色"。
;;                            FragColor = 用户自定义名字（②类）；只有一个
;;                            out 时名字随便起
;;   (set! FragColor (vec4 1.0 0.35 0.2 1.0))
;;                            四个数 = 红 绿 蓝 不透明度，合起来是橙色。
;;                            1.0 是浮点字面量（GLSL 里浮点必须带小数点，不能写 1）
(define frag-src
  (glsl (version 330 core)
        (out vec4 FragColor)
        (define (main) void
          (set! FragColor (vec4 1.0 0.35 0.2 1.0)))))

;; 打印展开出的真实 GLSL——S 表达式和 GLSL 一一对应，照这个对照读。
(printf "顶点着色器展开为：\n~a\n\n" (glsl-pretty vert-src))
(printf "片元着色器展开为：\n~a\n" (glsl-pretty frag-src))

;; 本步还没编译/上传/绘制，所以画面仍是清屏色。
(define (draw)
  (glClearColor 0.10 0.12 0.20 1.0)
  (glClear GL_COLOR_BUFFER_BIT))

(define-values (frame canvas)
  (make-window #:title "02-01 着色器是什么" #:width 400 #:height 300 #:draw draw))

(send frame show #t)
