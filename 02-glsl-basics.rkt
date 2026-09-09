#lang racket/base
;; =========================================================
;; 02-glsl-basics.rkt —— GLSL 语言本身（统一讲一次）
;; 运行：racket 02-glsl-basics.rkt    ESC/点X = 退出
;; =========================================================
;; 01 用了第一段 GLSL：`in/uniform/out + main`。那一课的重点是"数据怎么
;; 进 GPU"。这一课退一步，把 **GLSL 这门语言**讲透——只此一次，之后各课
;; 遇到 GLSL 语法只补"用法"，不再重复解释设计。
;;
;; ── 从设计原理出发：为什么着色器语言长这样 ─────────────────
;; 1) 为什么要有一门语言？
;;    GPU 要的效果千变万化（卡通、火焰、描边…），固定功能"内置套路"满足
;;    不了 → 把"每个顶点/每个像素做什么"开放成小程序，由开发者写。
;; 2) 为什么是"每个顶点一段、每个片元一段"？
;;    这些程序要跑在几千个并行单元上：同一份代码、不同输入、同时执行。
;;    所以 GLSL 被设计成 **无全局可变状态、一次处理一个单元** 的小函数：
;;    不许 malloc、没有指针、main 只往输出里写值 → 天然可大规模并行。
;; 3) 输入输出是"声明"出来的，不是函数参数：
;;     attribute(in) = 跟着顶点数据走（每顶点一份）
;;     uniform        = 这次 draw 全体共用（每帧从 CPU 传）
;;     out/in 对接    = 顶点算完 → GPU 自动插值 → 片元拿到的 in
;;     gl_Position    = 顶点着色器必须写的"裁剪坐标"（固定含义的输出）
;;     FragColor(自己起名) = 片元着色器的输出；只有它一个 out 时名字随意
;; 4) 为什么类型是 vec2/vec3/vec4？
;;    图形数据天然是"小向量"(位置、颜色、法线)。向量类型让 +-*/、点积
;;    这类操作**逐分量一次写完**，不用写三遍循环——这正是 GPU 最擅长的。
;;    配套的 swizzle（a.xy / a.zyx 重排分量）也是为此而生。
;; 5) 为什么内置这么多数学函数（mix/clamp/length/dot/fract…）？
;;    图形公式高频用到，且要保证在 GPU 上实现一致——内置比手写更稳更快。
;;
;; ── GLSL 快速表（语法就这些，按需回来查）─────────────────
;; 类型：float int bool | vec2 vec3 vec4 | mat3 mat4 | sampler2D(纹理)
;; 构造器：vec2(1.0,2.0)、vec3(v2, 1.0)、vec4(color, 1.0)（缺分量可"升维"补）
;; 字面量：浮点必须带小数点（1.0/0.5）；★★1/2 是整数除法 = 0！★★
;; 变量：类型+名字；main 不返回值(void)；函数可自写，参数默认按值
;; 分支：if/else、for(循环次数须是常数边界——GPU 不允许运行时未知循环)
;; 常用内建：mix(a,b,t)=按t混色 length/normalize/dot/clamp/fract/sin/cos…
;; 版本：文件第一行必须是 #version 330 core（配合我们的 core 上下文）
;; 布局：layout(location=N) 声明的是"槽位号"，与 Racket 侧 glVertexAttribPointer(N) 对齐
;; 纪律：源文件纯 ASCII（Mesa 对非 ASCII 报错毫无头绪）
;; =========================================================

;; ── 本课用 S 表达式写 GLSL（racket-glsl DSL）────────────
;; 本文件里的两段 shader 用 (glsl ...) 宏写成 S 表达式，运行时展开成上面
;; 表格里一模一样的 GLSL 文本（并打印出来）。S 表达式 ↔ GLSL 的逐条对照
;; 见 SYNTAX.md。好处：shader 是结构化数据，括号/分号/类型拼写由宏保证，
;; 写错会在 Racket 侧立即报错；后续课程还能在编译期做递归展开（见 §10）。
;; 注意：核心层为安全起见给每个二元运算都加括号，展开出的文本会有冗余
;; 括号（如 ((vUV * 2.0) - 1.0)），语义与手写 vUV * 2.0 - 1.0 完全一致。

(require racket/gui opengl)
;; ffi/vector 的 f32vector/u16vector/u32vector 由 lib.rkt 转发，无需再 require
(require "lib.rkt")

(define start-ms (current-inexact-milliseconds))

;; 顶点着色器：aPos 是铺满窗口的四边形角点(NDC -1..1)，aUV 是 0..1 的屏幕坐标。
;; vUV 原样交给片元，GPU 在三角形内自动插值。
(define vert
  (glsl
   (version 330 core)
   (layout (location 0) in vec2 aPos)
   (layout (location 1) in vec2 aUV)
   (out vec2 vUV)
   (define (main) void
     (set! vUV aUV)
     (set! gl_Position (vec4 aPos 0.0 1.0)))))

;; 片元着色器（本课 GLSL 语言演示，一行一意，讲解放 S 表达式外，跟着念一遍）：
(define frag
  (glsl
   (version 330 core)
   (in vec2 vUV)
   (uniform float uTime)
   (out vec4 FragColor)
   (define (main) void
     (vec2 p (- (* vUV 2.0) 1.0))
     (float d (length p))
     (float ring (fract (- (* d 6.0) uTime)))
     (vec3 c (mix (vec3 0.10 0.15 0.40) (vec3 0.10 0.70 1.00) ring))
     (*= c (- 1.0 (* 0.55 d)))
     (+= c (* (vec3 0.15) (+ 0.5 (* 0.5 (sin (* uTime 2.0))))))
     (when (and (> (x p) 0.15) (> (y p) 0.15))
       (set! c (vec3 (+ (* (x p) 0.5) 0.5)
                     (+ (* (y p) 0.5) 0.5)
                     (+ 0.5 (* 0.4 (sin (+ uTime (* (x p) 3.0))))))))
     (set! FragColor (vec4 (clamp c 0.0 1.0) 1.0)))))

;; 展开成真实 GLSL（美化 = 换行+缩进）。编译也用它，报错时行号可读。
(define vert-src (glsl-pretty vert))
(define frag-src (glsl-pretty frag))

;; ---- 上面展开出的片元着色器在画什么（一行一意，跟着念一遍）----
;;   in vec2 vUV       = 顶点着色器传来的 uv（0..1，GPU 已插值）
;;   uniform uTime     = 每次 draw 全体共用的参数，CPU 每帧传秒数
;;   vec2 p = vUV*2-1  = 每像素自己的坐标，屏幕中心是 0
;;   length(p)         = 该像素到中心的距离 → "圆"的数学
;;   fract(d*6 − time) = 距离随时间增长 → 波浪圈圈（6 = 圈的密度）
;;   mix(a,b,ring)     = 按 ring 0..1 在两个颜色间混 → 圈的亮暗
;;   c *= ...          = 逐分量乘：越靠边越暗
;;   c += ...          = 逐分量加：整体呼吸（亮度随 sin 起伏）
;;   if 分支            = 右上角色板把坐标当颜色 → 直观证明
;;                       "像素只是数据、着色器是表达式"
;;   clamp(c,0,1)      = 把颜色夹回 [0,1]，补 alpha 写进输出
(define cfg (new gl-config%))
(send cfg set-legacy? #f)
(send cfg set-double-buffered #t)
(define frame
  (new (class frame%
         (augment* [on-close (lambda () (exit 0))])
         (super-new))
       (label "02 GLSL 语言") (width 600) (height 600)))

(define init? (box #f))
(define prog #f) (define vao 0) (define loc-time 0)

(define canvas
  (new (class canvas%
         (inherit with-gl-context swap-gl-buffers)
         (define/override (on-size w h)
           (with-gl-context
            (lambda ()
              (define-values (gw gh) (send this get-gl-client-size))
              (glViewport 0 0 gw gh)
              (glClearColor 0.05 0.06 0.10 1.0))))
         (define/override (on-char e)
           (when (eq? (send e get-key-code) 'escape) (exit 0)))
         (define/override (on-paint)
           (with-gl-context
            (lambda ()
              (unless (unbox init?)
                (set-box! init? #t)
                (set! prog (build-program vert-src frag-src))
                (set! loc-time (glGetUniformLocation prog "uTime"))
                ;; 铺满窗口的四边形（EBO 4 顶点 + 6 索引，01 同套路）。
                ;; 每个顶点 = (vec2 pos, vec2 uv)，用 rename-vector 的 vec4 拼一个顶点；
                ;; stride/offset 用 glsl-byte-size 算，不再手写 16/8 魔法数字。
                (define verts
                  (apply f32vector
                         (append (f32vector->list (vec4 -1.0 -1.0  0.0 0.0))
                                 (f32vector->list (vec4  1.0 -1.0  1.0 0.0))
                                 (f32vector->list (vec4  1.0  1.0  1.0 1.0))
                                 (f32vector->list (vec4 -1.0  1.0  0.0 1.0)))))
                (define idx (u16vector 0 1 2 0 2 3))
                (define v (u32vector-ref (glGenVertexArrays 1) 0))
                (glBindVertexArray v)
                (define vb (u32vector-ref (glGenBuffers 1) 0))
                (glBindBuffer GL_ARRAY_BUFFER vb)
                (glBufferData GL_ARRAY_BUFFER (gl-vector-sizeof verts) verts GL_STATIC_DRAW)
                (define s4 (+ (glsl-byte-size 'vec2) (glsl-byte-size 'vec2)))  ; pos+uv = 16 字节
                (glVertexAttribPointer 0 2 GL_FLOAT #f s4 0)
                (glEnableVertexAttribArray 0)
                (glVertexAttribPointer 1 2 GL_FLOAT #f s4 (glsl-byte-size 'vec2))  ; offset = 8
                (glEnableVertexAttribArray 1)
                (define eb (u32vector-ref (glGenBuffers 1) 0))
                (glBindBuffer GL_ELEMENT_ARRAY_BUFFER eb)
                (glBufferData GL_ELEMENT_ARRAY_BUFFER (gl-vector-sizeof idx) idx GL_STATIC_DRAW)
                (glBindVertexArray 0)
                (set! vao v))

              (define t (/ (- (current-inexact-milliseconds) start-ms) 1000.0))
              (glClear GL_COLOR_BUFFER_BIT)
              (glUseProgram prog)
              (glUniform1f loc-time t)
              (glBindVertexArray vao)
              (glDrawElements GL_TRIANGLES 6 GL_UNSIGNED_SHORT 0)
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
