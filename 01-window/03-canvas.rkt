#lang racket/base
;; =========================================================
;; 01-window/03-canvas.rkt —— 第三步：放进一块 OpenGL 画布
;; 运行：racket 01-window/03-canvas.rkt
;; =========================================================
;; 前两步得到一个空窗口。本步在窗口里放一块"能画 OpenGL 的矩形区域"。
;; 本步新增（3 个，同属"画布"这一件事）：
;;   canvas%   —— 画布类；给它两个初始化参数就变成 OpenGL 画布
;;   (style '(gl no-autoclear)) —— 'gl 打开 GL 支持；'no-autoclear 别让系统清屏
;;   gl-config% —— GL 上下文配置
;;
;; 概念：
;;   "GL 上下文" = GL 的全部状态（颜色、缓冲、程序…）所在的环境，一个画布
;;   对应一个上下文。
;;   set-legacy? #f 请求"core profile"上下文（= 不是旧式 legacy 兼容配置）。
;;   OpenGL 3.2 起把 API 分成两种 profile：
;;     · core profile：删掉旧式"固定管线"（glBegin/glEnd 立即模式、内置矩阵/
;;       光照那套），只保留可编程管线，必须用 shader 画（02 课开始写）。
;;     · legacy（兼容）：保留那些旧接口，新旧都能用。
;;   本教程只用可编程管线，所以请求 core profile：用不到的旧接口干脆不要求，
;;   也和 shader 里 (version 330 core) 保持一致。（必要：shader 全按 core 写）
;; =========================================================

(require racket/gui
         opengl)      ; gl* 函数与常量

;; 上下文配置：
(define cfg (new gl-config%))
(send cfg set-legacy? #f)        ; 请求 core profile（非 legacy）：删掉旧式固定管线
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
