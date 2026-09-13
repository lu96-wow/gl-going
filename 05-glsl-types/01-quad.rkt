#lang racket/base
;; =========================================================
;; 05-glsl-types/01-quad.rkt —— 第一步：铺满窗口的四边形
;; 运行：racket 05-glsl-types/01-quad.rkt    点 X = 退出
;; =========================================================

;; 02 课照着抄写出了 vec2 / vec4 / in / out / gl_Position，把三角形画了出来；
;; 04 课又用上了 uniform / sin / 算术。但一直没停下来把这些类型讲透。
;; 本课（05-glsl-types）退一步，把 GLSL 语言的第一块——类型 + 表达式——讲透。
;; 只此一次，之后各课遇到 GLSL 只补"用法"，不再重复解释设计。
;;
;; 本课的画布从"一个小三角形"换成"铺满整个窗口的四边形"：
;; 片元着色器每个像素拿到一个 0..1 的屏幕坐标 vUV，于是 shader 变成
;; "像素 = uv 的纯函数"——最适合演示类型 / 构造器 / swizzle / 运算符 / 数学函数。
;;
;; 本步新增（2 个，同属"多一份顶点数据"这一件事）：
;;   ① 第二组顶点属性 aUV —— 每个顶点除了位置，还带一个 0..1 的"屏幕坐标"
;;   ② stride/offset 字节布局 —— 一个顶点里有两组数据，GPU 要按字节跳着读
;;
;; ★为什么要 0..1 的 uv 坐标：片元着色器最自然的输入是"这个像素在窗口里的
;;   相对位置"。0..1 表示"从左/下到右/上的比例"，与窗口尺寸无关。顶点着色器
;;   把 aUV 原样交给片元，GPU 在三角形内部自动插值——于是每个像素都拿到
;;   自己位置的 vUV，shader 就变成"像素 = uv 的纯函数"。
;;
;; ★一个顶点现在有 4 个 float：前两个是位置、后两个是 uv。它们**交错**存在
;;   同一块缓冲里（不是两块分开的缓冲）：每 4 个 float 一循环。
;;   stride/offset 就是描述这个布局的字节数学（float 是 4 字节）：
;;     stride 16 = 一个顶点占 4 个 float = 16 字节（读下一个顶点要跳 16）
;;     offset  0 = 位置从顶点开头 0 字节处读
;;     offset  8 = uv 从顶点开头 8 字节处读（跳过前两个 float）
;;   02 课只有一份属性，所以 stride=8、offset=0；本课多一份，就变成 16 和 8。
;;   ★本教程不手写 16 / 8 这两个魔法数字：用 glsl-stride-bytes（本课后面会讲）
;;     按类型清单直接算出来，见文件末尾的初始化。

(require "../02-triangle/04-gui-tool.rkt")   ; make-window（带视口）
(require "../racket-glsl/rewrite.rkt")        ; (glsl ...) 宏 + glsl-program-src
(require "../racket-glsl/rename-vector.rkt")  ; vec / vec4 / glsl-stride-bytes
(require "../racket-glsl/tool.rkt")           ; build-program / use-program

;; 顶点着色器：本课第一次出现"顶点 → 片元"的交接变量（02 课讲的角色②）。
;; 三种角色一次看全：
;;   (layout (location 0) in vec2 aPos)   角色①：CPU 喂位置（location 0）
;;   (layout (location 1) in vec2 aUV)    角色①：CPU 喂 uv（location 1，本课新加）
;;   (out vec2 vUV)                       角色②：顶点写出的交接变量
;;   gl_Position（赋值）                   预定义：顶点落在哪
;;
;; 交接规则（02 课讲过，这里第一次真用上）：顶点写 (out vec2 vUV)，片元写
;; (in vec2 vUV)——**同名同类型**，两边就接上了。GPU 在光栅化时自动插值：
;; 顶点只在 3 个角上写了 vUV，中间每个像素拿到的 vUV 是自动平均出来的。
(define vert-src
  (glsl (version 330 core)
        (layout (location 0) in vec2 aPos)
        (layout (location 1) in vec2 aUV)
        (out vec2 vUV)
        (define (main) void
          (set! vUV aUV)                        ; uv 原样传下去
          (set! gl_Position (vec4 aPos 0.0 1.0)))))

;; 片元着色器：把顶点交接来的 vUV 直接当颜色（红 = u，绿 = v，蓝 = 0）。
;;   (in vec2 vUV)   角色②：读顶点着色器的 (out vec2 vUV)，同名同类型接上。
;;   于是：左下(0,0)=黑 → 右下(1,0)=红 → 左上(0,1)=绿 → 右上(1,1)=黄。
;;   中间都是 GPU 插值出来的渐变。你看到的是"像素位置被涂成颜色"。
(define frag-src
  (glsl (version 330 core)
        (in vec2 vUV)
        (out vec4 FragColor)
        (define (main) void
          (set! FragColor (vec4 vUV 0.0 1.0)))))

;; 四边形 = 两个三角形 = 6 个顶点（角其实只有 4 个——这份"重复"是后面 EBO 课的引子）。
;;
;; ★为什么"四边形"要用两个三角形拼，而不是一个"四边形图元"？
;;   ① 点、线、三角形里，**只有三角形能填面积**（点无线宽、线无厚度）。
;;   ② 三点必共面 → 三角形永远是平的；且凸性让"判断像素在不在里面"极简单，
;;      GPU 的光栅化硬件就是为它优化的。
;;   ③ 任何多边形都能切成三角形——三角形是"面积"的最小原子，能画三角形就能画一切。
;;   老 OpenGL 的 GL_QUADS 只是语法糖（底层仍拆成 2 个三角形），core profile 已删掉它。
;; 所以铺满窗口 = 显式给出 2 个三角形。
;;
;; 每个顶点用 Racket 侧构造器打包成 4 个 float：(位置.x, 位置.y, uv.x, uv.y)。
;; ★两个构造器容易混，从里往外读：
;;   vec4 x y z w —— 一个"4 维向量"（f32vector，四个 float）
;;   vec          —— 把若干个**同宽度**的 vec4 打包成一块连续缓冲
;; 所以 (vec (vec4 ...) ...) = "6 个顶点，拼成一块连续的 24 个 float"。
;; ★注意：这里的 vec4 是 Racket 侧造 f32vector 的构造器，不是 shader 里的 vec4
;;   类型——只是"一个顶点 4 个 float"的打包，两个世界的名字一一对应。
;; 铺满 NDC（-1..1）的两个三角形。对角线两端（左下、右上）各出现两次：
;; 被两个三角形共用——正是这份"重复"引出后面课的 EBO（索引缓冲）。
(define verts
  (vec (vec4 -1.0 -1.0  0.0 0.0)   ; 三角形① 左下
       (vec4  1.0 -1.0  1.0 0.0)   ;        右下
       (vec4  1.0  1.0  1.0 1.0)   ;        右上
       (vec4 -1.0 -1.0  0.0 0.0)   ; 三角形② 左下（重复）
       (vec4  1.0  1.0  1.0 1.0)   ;        右上（重复）
       (vec4 -1.0  1.0  0.0 1.0))) ;        左上

;; 每帧：清屏 → 上程序 → 绑 VAO → 画 6 个顶点（两个三角形）
(define (draw)
  (gl-clear-color 0.10 0.12 0.20 1.0)
  (gl-clear gl-color-buffer-bit)
  (use-program prog)
  (gl-bind-vertex-array vao)
  (gl-draw-arrays gl-triangles 0 6))

(define-values (frame canvas)
  (make-window #:title "05-01 铺满窗口的四边形" #:width 400 #:height 400 #:draw draw))

;; 初始化：程序 + 交错数据 + VAO（两个属性，两个 gl-vertex-attrib-pointer）
(define prog
  (send canvas with-gl-context
    (lambda () (build-program (gl-vertex-shader vert-src) (gl-fragment-shader frag-src)))))
(define vao
  (send canvas with-gl-context
    (lambda ()
      (define data (vec->f32vector verts))
      (define vbo (u32vector-ref (gl-gen-buffers 1) 0))
      (gl-bind-buffer gl-array-buffer vbo)
      (gl-buffer-data gl-array-buffer (gl-vector-sizeof data) data gl-static-draw)
      (define v (u32vector-ref (gl-gen-vertex-arrays 1) 0))
      (gl-bind-vertex-array v)
      ;; ★stride/offset 不手写魔法数字，用 glsl-stride-bytes 按"这个顶点的类型清单"算：
      ;;   一个顶点 = vec2(位置) + vec2(uv)，所以
      ;;     stride = (glsl-stride-bytes 'vec2 'vec2) = 8 + 8 = 16 字节
      ;;     uv 的偏移 = (glsl-stride-bytes 'vec2) = 位置占的 8 字节
      ;;   location 0 = 位置：每属性 2 个 float，步长 16，起点 0
      (gl-vertex-attrib-pointer 0 2 gl-float #f (glsl-stride-bytes 'vec2 'vec2) 0)
      (gl-enable-vertex-attrib-array 0)
      ;;   location 1 = uv：每属性 2 个 float，步长 16，起点 8（跳过位置）
      (gl-vertex-attrib-pointer 1 2 gl-float #f
                                (glsl-stride-bytes 'vec2 'vec2)
                                (glsl-stride-bytes 'vec2))
      (gl-enable-vertex-attrib-array 1)
      (gl-bind-vertex-array 0)
      v)))

(send frame show #t)
