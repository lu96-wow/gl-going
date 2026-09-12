#lang racket/base
;; =========================================================
;; 02-triangle/04-gui-tool.rkt —— 窗口工具（本课版）
;; =========================================================
;; 从 01-window/04-gui-tool.rkt 复制，本课新增一件事：
;;   视口处理（on-size + gl-viewport）。
;;
;; 为什么本课才加：视口决定"-1..1 的坐标"映射到哪个像素矩形。
;;   01 课只清屏，gl-clear 清的是整块缓冲、与视口无关，所以不需要视口；
;;   本课开始画几何（三角形），几何要按视口定位——不设视口，三角形会被裁掉。
;;
;; 其余（上下文 / 点 X 退出 / 进入上下文 → draw → 翻页）与 01 相同。
;; =========================================================

(require racket/gui "../racket-glsl/opengl-rename.rkt")
(provide make-window
         (all-from-out racket/gui)   ; frame% canvas% 等
         (all-from-out "../racket-glsl/opengl-rename.rkt"))   ; gl-* 函数与常量

(define (make-window #:title title
                     #:draw [draw void]
                     #:width [w 400]
                     #:height [h 300])
  ;; 点 X 退出的窗口类（同 01）
  (define closeable-frame%
    (class frame%
      (augment* [on-close (lambda () (exit 0))])
      (super-new)))
  ;; 画布类：本课多了 on-size（视口跟住画布真实像素大小）
  (define gl-canvas%
    (class canvas%
      (inherit with-gl-context swap-gl-buffers)
      ;; on-size：尺寸变化（含第一次显示）时调用。
      ;; 视口 = 把 -1..1 映射到哪个像素矩形；铺满整个绘图区 = gl-viewport(0,0,fw,fh)。
      ;; 注意 w h 不用：它们是"整个窗口"的逻辑像素（含边框），不是 gl-viewport
      ;; 要的；要的是绘图区的真实 GL 像素尺寸 → get-gl-client-size。
      (define/override (on-size w h)
        (with-gl-context
         (lambda ()
           (define-values (fw fh) (send this get-gl-client-size))
           (gl-viewport 0 0 fw fh))))
      ;; on-paint：每次重绘 进入上下文 → draw → 翻页（清屏在 draw 里）
      (define/override (on-paint)
        (with-gl-context
         (lambda ()
           (draw)                              ; 你的每帧内容（含清屏）
           (send this swap-gl-buffers))))
      (super-new)))
  ;; 上下文配置
  (define cfg (new gl-config%))
  (send cfg set-legacy? #f)          ; core profile
  (send cfg set-double-buffered #t)  ; 双缓冲
  ;; 实例化窗口 + 画布，返回两个对象
  (define frame (new closeable-frame% (label title) (width w) (height h)))
  (define canvas (new gl-canvas% (style '(gl no-autoclear)) (gl-config cfg) (parent frame)))
  (values frame canvas))

;; 同 01：不顺便 show。本课每步都要先在 canvas 上下文里编译着色器、上传数据，
;; 所以把 canvas 交给你，先 init 再自己 show。
