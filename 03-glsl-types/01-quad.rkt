#lang racket/base
;; =========================================================
;; 03-glsl-types/01-quad.rkt —— 第一步：铺满窗口的四边形
;; 运行：racket 03-glsl-types/01-quad.rkt    点 X = 退出
;; =========================================================
;; 02 课画的是屏幕中央一个小三角形。本课要把 GLSL 语言讲透，最好让"每个像素
;; 都能算自己的颜色"——所以先把画布换成一块**铺满整个窗口的四边形**。
;;
;; 本步新增（2 个，同属"多一份顶点数据"这一件事）：
;;   ① 第二组顶点属性 aUV —— 每个顶点除了位置，还带一个 0..1 的"屏幕坐标"
;;   ② stride/offset 字节布局 —— 一个顶点里有两组数据，GPU 要按字节跳着读
;;
;; ★为什么要 0..1 的 uv 坐标：片元着色器最自然的输入是"这个像素在窗口里的
;;   相对位置"。0..1 表示"从下/左到上/右的比例"，与窗口尺寸无关。顶点着色器
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
;; =========================================================

(require "lib-gui.rkt")
(require "lib.rkt")

;; 顶点着色器：现在有两条 in。location 0 是位置、location 1 是 uv。
;; 两条 out/in 对接之前只有一条（vPos）——现在多一条 vUV，同一套规则。
(define vert-src
  (glsl (version 330 core)
        (layout (location 0) in vec2 aPos)
        (layout (location 1) in vec2 aUV)
        (out vec2 vUV)
        (define (main) void
          (set! vUV aUV)                        ; uv 原样传下去
          (set! gl_Position (vec4 aPos 0.0 1.0)))))

;; 片元着色器：把 vUV 直接当颜色（红 = u，绿 = v，蓝 = 0）。
;; 于是：左下(0,0)=黑 → 右下(1,0)=红 → 左上(0,1)=绿 → 右上(1,1)=黄。
;; 中间都是 GPU 插值出来的渐变。你看到的是"像素位置被涂成颜色"。
(define frag-src
  (glsl (version 330 core)
        (in vec2 vUV)
        (out vec4 FragColor)
        (define (main) void
          (set! FragColor (vec4 vUV 0.0 1.0)))))

;; 四边形 = 两个三角形 = 6 个顶点（角其实只有 4 个——这份"重复"是 05 课讲 EBO 的引子）。
;; 每个顶点用 Racket 的 vec4 构造器打包成 4 个 float：(位置.x, 位置.y, uv.x, uv.y)。
;; ★注意：这里的 vec4 是 Racket 侧造 f32vector 的构造器（4 个 float 一组），
;;   不是 shader 里的 vec4 类型——只是"一个顶点 4 个 float"的打包，两个世界的名字对应。
;; 铺满 NDC（-1..1）的两个三角形。注意：对角线的两个端点（左下、右上）
;; 各出现两次——它们被两个三角形共用，正是这份"重复"引出 05 课的 EBO：
(define verts
  (vec (vec4 -1.0 -1.0  0.0 0.0)   ; 三角形① 左下
       (vec4  1.0 -1.0  1.0 0.0)   ;        右下
       (vec4  1.0  1.0  1.0 1.0)   ;        右上
       (vec4 -1.0 -1.0  0.0 0.0)   ; 三角形② 左下（重复）
       (vec4  1.0  1.0  1.0 1.0)   ;        右上（重复）
       (vec4 -1.0  1.0  0.0 1.0))) ;        左上

;; 每帧：清屏 → 上程序 → 绑 VAO → 画 6 个顶点（两个三角形）
(define (draw)
  (glClearColor 0.10 0.12 0.20 1.0)
  (glClear GL_COLOR_BUFFER_BIT)
  (glUseProgram prog)
  (glBindVertexArray vao)
  (glDrawArrays GL_TRIANGLES 0 6))

(define-values (frame canvas)
  (make-window #:title "03-01 铺满窗口的四边形" #:width 400 #:height 400 #:draw draw))

;; 初始化：程序 + 交错数据 + VAO（两个属性，两个 glVertexAttribPointer）
(define prog (send canvas with-gl-context (lambda () (build-program vert-src frag-src))))
(define vao
  (send canvas with-gl-context
        (lambda ()
          (define vbo (u32vector-ref (glGenBuffers 1) 0))
          (glBindBuffer GL_ARRAY_BUFFER vbo)
          (glBufferData GL_ARRAY_BUFFER (gl-vector-sizeof (vec->f32vector verts)) (vec->f32vector verts) GL_STATIC_DRAW)
          (define v (u32vector-ref (glGenVertexArrays 1) 0))
          (glBindVertexArray v)
          ;; location 0 = 位置：每属性 2 个 float，步长 16 字节，起点 0
          (glVertexAttribPointer 0 2 GL_FLOAT #f 16 0)
          (glEnableVertexAttribArray 0)
          ;; location 1 = uv：每属性 2 个 float，步长 16 字节，起点 8（跳过位置）
          (glVertexAttribPointer 1 2 GL_FLOAT #f 16 8)
          (glEnableVertexAttribArray 1)
          (glBindVertexArray 0)
          v)))

(send frame show #t)
