#lang racket/base
;; =========================================================
;; 03-pipeline/01-pipeline.rkt —— 第一步：渲染管线全景
;; 运行：racket 03-pipeline/01-pipeline.rkt
;; =========================================================
;; 02 课我们"照着抄"画出了三角形。本步停下来，把 GPU 画它的完整路径看清楚。
;;
;; ★core profile（OpenGL 3.2 起删掉旧式固定管线的配置）的渲染管线分 7 段（本教程 GL 3.3）：
;;
;;   ① 输入装配    读 VBO 里的顶点数据，按图元类型（三角形/线/点）分组
;;   ② 顶点着色器  ★可编程★ 每个顶点跑一次，算出它落在哪（写 gl_Position）
;;   ③ 曲面细分    （可选，可编程）把图元再细分出更多顶点；本教程不用，跳过
;;   ④ 几何着色器  （可选，可编程）按图元增删顶点；本教程不用，跳过
;;   ⑤ 光栅化      ★固定★ 把图元铺成像素；裁剪、透视除法、视口变换、背面剔除
;;   ⑥ 片元着色器  ★可编程★ 每个像素跑一次，算出它的颜色
;;   ⑦ 逐采样操作  ★固定★ 深度测试、混合等最后一道过滤，决定像素写不写进缓冲
;;
;; ★关键区分（后面每课都用）：
;;   - 可编程段（②⑥，以及可选的③④）= 你要自己写 GLSL 小程序；GPU 照跑。
;;   - 固定段（①⑤⑦）= GPU 硬件写死，但很多行为能用 gl* 配置
;;     （①用 glVertexAttribPointer 描述布局，⑤用 glViewport/glEnable 设视口和
;;      剔除，⑦用 glEnable(GL_DEPTH_TEST/BLEND) 开测试和混合）。
;;
;; ★本步代码 = 02 的三角形。唯一区别：下面在每处 gl* 调用旁标出它属于哪一段。
;;   你不需要记函数名，只需要建立"我的代码在喂/配置哪一段"这个地图。
;; =========================================================

(require "lib-gui.rkt")
(require "lib.rkt")

(define vert-src
  (glsl (version 330 core)
        (layout (location 0) in vec2 aPos)
        (define (main) void
          (set! gl_Position (vec4 aPos 0.0 1.0)))))   ; 段②：顶点着色器
(define frag-src
  (glsl (version 330 core)
        (out vec4 FragColor)
        (define (main) void
          (set! FragColor (vec4 1.0 0.35 0.2 1.0))))) ; 段⑥：片元着色器

(define verts (vec (vec2 -0.5 -0.5) (vec2 0.5 -0.5) (vec2 0.0 0.5)))

(define (draw)
  (glClearColor 0.10 0.12 0.20 1.0)
  (glClear GL_COLOR_BUFFER_BIT)                        ; 清屏（清默认帧缓冲）
  (glUseProgram prog)        ; 选择程序：让段②⑥用我们编译的 GLSL
  (glBindVertexArray vao)    ; 绑 VAO：告诉段①"字节怎么读"
  (glDrawArrays GL_TRIANGLES 0 3))  ; 发出绘制：段①开始 → 依次流过 ②⑤⑥⑦

(define-values (frame canvas)
  (make-window #:title "03-01 渲染管线全景" #:width 400 #:height 300 #:draw draw))

;; 段②⑥：把两段 GLSL 编译链接成一个"程序"（03/04 步会拆开细讲）
(define prog (send canvas with-gl-context
                    (lambda ()
                      (build-program (GL_VERTEX_SHADER vert-src)
                                     (GL_FRAGMENT_SHADER frag-src)))))
;; 段①：数据上传（VBO）+ 布局说明书（VAO）
(define vao
  (send canvas with-gl-context
        (lambda ()
          (define vbo (u32vector-ref (glGenBuffers 1) 0))
          (glBindBuffer GL_ARRAY_BUFFER vbo)
          (glBufferData GL_ARRAY_BUFFER (gl-vector-sizeof (vec->f32vector verts))
                        (vec->f32vector verts) GL_STATIC_DRAW)
          (define v (u32vector-ref (glGenVertexArrays 1) 0))
          (glBindVertexArray v)
          (glVertexAttribPointer 0 2 GL_FLOAT #f 8 0)
          (glEnableVertexAttribArray 0)
          (glBindVertexArray 0)
          v)))

(send frame show #t)
;; 段⑤⑦本课没显式配置（走默认：视口=窗口、不剔除、不测深度/不混合）——
;; 这些在后面的课按需打开。现在只要知道它们"在但默认关/默认值"即可。
