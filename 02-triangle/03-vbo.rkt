#lang racket/base
;; =========================================================
;; 02-triangle/03-vbo.rkt —— 把顶点数据放进 GPU
;; 运行：racket 02-triangle/03-vbo.rkt
;; =========================================================

;; 程序有了（02 步），但还没"画什么"。三角形 = 3 个顶点，每个顶点 = 一个
;; 位置 (x, y)。本步把 3 个顶点传进 GPU，并告诉 GPU 怎么读。
;;
;; 为什么要传进 GPU？CPU 和 GPU 之间通信很慢。老式 OpenGL 每画一个顶点就
;; CPU→GPU 传一次；现代做法 = 把整块顶点数据"一次批量"拷进 GPU 显存（VBO），
;; 之后 GPU 自己读，不再打扰 CPU。

(require "../01-window/04-gui-tool.rkt")         ; 01 课的窗口工具（上传数据只需上下文）
(require "../racket-glsl/rename-vector.rkt")  ; vec / vec2 / vec->f32vector / u32vector-ref

;; 顶点数据：3 个顶点，每个 = 一个 vec2 位置。
;;   vec2 x y —— 两个 float 组成的向量（对应 shader 里的 vec2 类型）
;;   vec      —— 把若干同宽度的 vec2 打包成一块连续缓冲
(define verts
  (vec (vec2 -0.5 -0.5)   ; 左下
       (vec2  0.5 -0.5)   ; 右下
       (vec2  0.0  0.5))) ; 上方

(define-values (frame canvas)
  (make-window #:title "02-03 顶点数据"))

;; 上传 + 配置都在 GL 上下文里做。
(define vao
  (send canvas with-gl-context
    (lambda ()
      ;; ── ① VBO：把数据拷进 GPU 显存 ──
      ;; vec->f32vector：拿到底层连续的一块 f32vector（上传要连续的字节块）
      (define data (vec->f32vector verts))

      ;; gl-gen-buffers：生成缓冲对象。GL 的 C 函数一次能生成 n 个编号，
      ;; 所以 Racket 绑定返回一个 u32vector（编号数组）。
      ;;   u32vector-ref = 从 u32vector 取第 i 个元素（u32 = 32 位无符号整数，
      ;;   这个类型来自 ffi/vector，racket-glsl 的 rename-vector 重新导出了它）。
      ;;   这里取第 0 个 = 我们只生成了 1 个，拿它当 vbo 编号。
      (define vbo (u32vector-ref (gl-gen-buffers 1) 0))

      ;; gl-bind-buffer：把 vbo 设为"当前要操作的缓冲"（GL 状态机）。
      ;; gl-array-buffer 是 GL 常量，表示"这块缓冲的用途 = 存顶点属性数据"。
      (gl-bind-buffer gl-array-buffer vbo)

      ;; gl-buffer-data：把数据真正拷进 GPU。
      ;;   (gl-vector-sizeof data) = 这块 f32vector 占多少字节（统一命名层转出的工具）
      ;;   gl-static-draw = 使用提示"数据基本不变"（GL 据此做优化；
      ;;                    经常改换 gl-dynamic-draw，每帧改换 gl-stream-draw）
      (gl-buffer-data gl-array-buffer
                    (gl-vector-sizeof data)
                    data
                    gl-static-draw)

      ;; ── ② VAO：连续数据的"切片手册" ──
      ;; VBO 里是一条**连续**的字节流（顶点一个接一个平铺，没有结构）。
      ;; GPU 要读，就得知道"怎么在这条流上切出每个顶点的每个属性"。
      ;; VAO 就是这本切片手册：记下 切哪个槽、每片几个分量、什么类型、
      ;; 每片隔多少字节（步长）、从第几字节开始切（偏移）、这个槽用不用。
      ;; 它**不存数据**（数据在 VBO），只存"怎么切"。
      ;;
      ;; 为什么把切片手册单独存成一个对象，而不是每次画之前现说一遍？
      ;;   ① 画的时候只要"绑 VAO"一步，就带上了整套切法，省事；
      ;;   ② core profile 里画东西**必须**绑一个 VAO，没 VAO 连画都不让画。
      ;;
      ;; gl-gen-vertex-arrays：生成 VAO 对象（同 gl-gen-buffers，返回 u32vector）。
      (define v (u32vector-ref (gl-gen-vertex-arrays 1) 0))

      ;; gl-bind-vertex-array：绑定 VAO——之后所有切片规则都记在它上面。
      (gl-bind-vertex-array v)

      ;; 回想 01-shader.rkt：shader 里 (layout (location 0) in vec2 aPos)
      ;; 声明"0 号槽 = 一个 vec2 位置"。这里用同一个号码 0 对齐。
      ;; gl-vertex-attrib-pointer(0, ...) = "0 号槽的数据长这样"，参数逐一看：
      ;;   0        —— 槽号（对应 shader 的 location 0）
      ;;   2        —— 每个属性 2 个分量（x、y）
      ;;   gl-float —— 每个分量的类型是 float（GL 常量）
      ;;   #f       —— 不归一化（float 数据不需要；整数数据要归一化到 0~1 才用 #t）
      ;;   8        —— 步长：切完这一片，要跳过 8 字节才到下一个顶点
      ;;               （一个顶点 = 2 个 float × 4 字节）
      ;;   0        —— 偏移：从流的第 0 字节开始切（只有这一个属性）
      (gl-vertex-attrib-pointer 0 2 gl-float #f 8 0)

      ;; gl-enable-vertex-attrib-array：启用 0 号槽（不启用的槽 GPU 不会读）
      (gl-enable-vertex-attrib-array 0)

      (gl-bind-vertex-array 0)   ; 解绑收好
      v)))

(printf "3 个顶点已上传，VAO 已记录：0 号槽 = 每顶点 2 个 float\n")
;; 本步还不画，画面是清屏色；下一步 gl-draw-arrays 才真正画出来。
