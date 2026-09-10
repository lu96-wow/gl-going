#lang racket/base
;; =========================================================
;; 01-window/03-canvas.rkt —— 第三步：放进一块 OpenGL 画布
;; 运行：racket 01-window/03-canvas.rkt
;; =========================================================
;; 前两步得到一个空窗口。本步在窗口里放一块"能画 OpenGL 的矩形区域"。
;; 本步新增（3 个，同属"画布"这一件事）：
;;   canvas%   —— 画布类；给它两个初始化参数就变成 OpenGL 画布
;;   (style '(gl no-autoclear)) —— 'gl 打开 GL 支持；'no-autoclear 别让系统清屏
;;   gl-config% —— GL 上下文配置（racket/gui 从 racket/draw 转发出来）
;;
;; 概念：
;;   "GL 上下文" = GL 的全部状态（颜色、缓冲、程序…）所在的环境，一个画布
;;   对应一个上下文。set-legacy? #f 请求 core profile = 现代 OpenGL——
;;   没有固定管线，一切靠我们自己写着色器（02 课开始写）。
;; =========================================================

(require racket/gui
         opengl)      ; gl* 函数与常量

;; 上下文配置：
(define cfg (new gl-config%))
(send cfg set-legacy? #f)        ; #f = 要 core（现代）上下文，不要旧式固定管线
(send cfg set-double-buffered #t) ; 双缓冲：后台画好再翻到屏幕（下一步讲为什么）

(define frame
  (new (class frame%
         (augment* [on-close (lambda () (exit 0))])
         (super-new))
       (label "01-03 OpenGL 画布") (width 400) (height 300)))

;; 画布：parent 把它放进 frame；(gl-config cfg) 让它使用上面的上下文配置。
;; 现在画布还是黑的——还没在上面画任何东西（下一步画第一帧）。
(define canvas
  (new canvas%
       (style '(gl no-autoclear))
       (gl-config cfg)
       (parent frame)))

(send frame show #t)
