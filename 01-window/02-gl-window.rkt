#lang racket/base
;; =========================================================
;; 01-window/02-gl-window.rkt —— 带 OpenGL 上下文的窗口
;; 运行：racket 01-window/02-gl-window.rkt
;; =========================================================

;; 本步仍只引入 racket/gui：画布、上下文配置都是它提供的。
;; 还没到真正"画"的时候，所以暂不引入 OpenGL 函数（gl-* 函数下一步才用）。
(require racket/gui)

;; ① 沿用上一步"点 X 退出"的窗口类。
(define closeable-frame%
  (class frame%
    (augment* [on-close (lambda () (exit 0))])
    (super-new)))

;; ② GL 上下文：OpenGL 是一台状态机，所有状态（颜色、缓冲、程序……）都住在
;;    一个叫"上下文"的环境里，一块画布 = 一个上下文。
;;    上下文长什么样，用配置对象 gl-config% 告诉系统。
;;    gl-config% 提供 set-* 方法来设置各项配置（Racket 命名习惯：set-xxx 设置、
;;    get-xxx 读取，都用 (send 对象 方法 参数...) 调用）：
(define cfg (new gl-config%))
(send cfg set-legacy? #f)          ; 请求 core profile：只用可编程管线，不要旧式固定管线
(send cfg set-double-buffered #t)  ; 双缓冲：画在后台，画完一次翻到前台，避免闪烁

;; ③ 画布 canvas%：窗口内容区里一块能画图的矩形，加上 'gl 就变成 OpenGL 画布，
;;    上下文就在这里创建。
;;      'gl           = 用 OpenGL 画
;;      'no-autoclear = 别让系统自动清屏（以后清屏时机我们自己控制）
;;      (gl-config cfg) = 使用 ② 配好的上下文
;;      (parent frame)  = 放进窗口
(define frame
  (new closeable-frame% (label "01-02 带 GL 上下文的窗口") (width 400) (height 300)))
(define canvas
  (new canvas%
       (style '(gl no-autoclear))
       (gl-config cfg)
       (parent frame)))

;; 到这里，窗口里已经有了一块"能画 OpenGL"的区域（上下文配好了），
;; 但还没画任何东西，所以是黑的。下一步再引入 gl-* 函数，往里画第一帧。
(send frame show #t)
