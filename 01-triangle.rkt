#lang racket/base
;; =========================================================
;; 01-triangle.rkt —— 现代管线的第一课：VAO + VBO + 着色器
;; 运行：racket 01-triangle.rkt    关闭：点 X / ESC
;; =========================================================
;; 上一课 00 只清屏。本课画出一个彩色渐变三角形——现代 OpenGL 的
;; "hello world"。全程只有 4 个新 API，却串起整条数据流：
;;
;;   ① glGenVertexArrays / glBindVertexArray  → VAO（记录"怎么取数"）
;;   ② glGenBuffers / glBindBuffer / glBufferData
;;                            → VBO（把顶点数据搬上 GPU 显存）
;;   ③ glVertexAttribPointer / glEnableVertexAttribArray
;;                            → 告诉 GPU：缓冲里每段字节是什么属性
;;   ④ glUseProgram / glDrawArrays
;;                            → 上色器上工，画三角形
;;
;; 图形原理（为什么现代管线长这样）：
;;   老 API 画一个三角形 = CPU 每个顶点调一次函数，CPU 与 GPU 之间一
;;   趟趟通信。现代做法 = 顶点数据**一次批量上传**到显存（VBO），然后用
;;   描述"缓冲布局"的 VAO + 两个小着色器程序，让 GPU 自己流水线处理：
;;
;;     顶点数组 → [顶点着色器: 算位置] → 图元装配 → 光栅化 → [片元着色器: 算颜色] → 屏幕
;;
;;   两个着色器是必须自己写的程序（GLSL 330 core，见文件末尾字符串）。
;;
;; 本课数据布局：每个顶点 5 个 float = 位置 vec2(2) + 颜色 vec3(3)。
;; 位置直接给 NDC 坐标（-1..1），所以这课还没有任何矩阵。
;; 颜色是"逐顶点"属性 → 片元着色器收到的 vColor 是三角形内插值
;; （所以三个顶点红/绿/蓝，中间自动渐变）。
;;
;; ★用法：layout(location=N) 与 glVertexAttribPointer(N,…) 的数字
;;   是同一套"槽位号"——GLSL 声明 0 号槽放 aPos，Racket 这边就把
;;   VBO 前 2 个 float 喂进 0 号槽；两边编号对不上画面就错位/黑屏。
;;   片元输出变量名可以随便起（只有它一个 out 时 GPU 知道送颜色缓冲）。
;; =========================================================

(require racket/gui 
         opengl)
(require "lib.rkt")

;; ---- 着色器源码（用 (glsl ...) S 表达式写，运行时展开成等价 GLSL）----
;; 顶点着色器，逐行读：
;;   layout(location=N) in ...  声明"从 N 号槽取数据"，与 glVertexAttribPointer(N) 对齐
;;   out vec3 vColor            算完的颜色交给片元着色器（GPU 自动插值）
;;   gl_Position                顶点着色器必须写的裁剪坐标（本课位置已是 NDC，直接透传）
(define vert-src
  (glsl-pretty
   (glsl
    (version 330 core)
    (layout (location 0) in vec2 aPos)
    (layout (location 1) in vec3 aColor)
    (out vec3 vColor)
    (define (main) void
      (set! vColor aColor)
      (set! gl_Position (vec4 aPos 0.0 1.0))))))

;; 片元着色器：收下顶点着色器传来的（已插值）颜色，写进输出颜色
(define frag-src
  (glsl-pretty
   (glsl
    (version 330 core)
    (in vec3 vColor)
    (out vec4 FragColor)
    (define (main) void
      (set! FragColor (vec4 vColor 1.0))))))

;; ---- 上下文配置：core profile（现代）----
(define cfg (new gl-config%))
(send cfg set-legacy? #f)
(send cfg set-double-buffered #t)
(define frame
  (new (class frame%
         (augment* [on-close (lambda () (exit 0))])
         (super-new))
       (label "01 第一个三角形") (width 600) (height 600)))

(define init? (box #f))
(define prog #f)
(define vao 0)

(define canvas
  (new (class canvas%
         (inherit with-gl-context swap-gl-buffers)
         (define/override (on-size w h)
           (with-gl-context
            (lambda ()
              (define-values (fw fh) (send this get-gl-client-size))
              (glViewport 0 0 fw fh)
              (glClearColor 0.07 0.08 0.14 1.0))))
         (define/override (on-char e)
           (when (eq? (send e get-key-code) 'escape) (exit 0)))
         (define/override (on-paint)
           (with-gl-context
            (lambda ()
              (unless (unbox init?)
                (set-box! init? #t)
                (set! prog (build-program vert-src frag-src))

                ;; ---- ① 三个顶点的数据：[x,y, r,g,b] ×3 ----
                (define verts
                  (f32vector -0.55 -0.45   1.0 0.2 0.2     ; 左下 红
                              0.55 -0.45   0.2 0.9 0.3     ; 右下 绿
                              0.0   0.55   0.2 0.5 1.0))   ; 顶上 蓝

                ;; ---- ② VBO：数据搬进 GPU 显存 ----
                (define vbo (u32vector-ref (glGenBuffers 1) 0))
                (glBindBuffer GL_ARRAY_BUFFER vbo)
                (glBufferData GL_ARRAY_BUFFER (gl-vector-sizeof verts) verts GL_STATIC_DRAW)

                ;; ---- ③ VAO：描述这份数据的"字节含义" ----
                (define v (u32vector-ref (glGenVertexArrays 1) 0))
                (glBindVertexArray v)
                (define stride (* 5 4))               ; 一个顶点占 5×4=20 字节
                (glVertexAttribPointer 0 2 GL_FLOAT #f stride 0)      ; 前 2 float = 位置
                (glEnableVertexAttribArray 0)
                (glVertexAttribPointer 1 3 GL_FLOAT #f stride (* 2 4)) ; 后 3 float = 颜色
                (glEnableVertexAttribArray 1)
                (glBindVertexArray 0)                 ; 解绑，收好 VAO
                (set! vao v))

              (glClear (bitwise-ior GL_COLOR_BUFFER_BIT))
              (glUseProgram prog)
              (glBindVertexArray vao)
              (glDrawArrays GL_TRIANGLES 0 3)         ; 3 个顶点 = 1 个三角形
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
