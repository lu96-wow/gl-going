#lang racket/base
;; =========================================================
;; 01-window/05-viewport.rkt —— 第五步：窗口缩放时视口跟住
;; 运行：racket 01-window/05-viewport.rkt
;; =========================================================
;; 上一步：能画一帧，但拖大窗口时清屏色可能只铺一部分 / 铺错位置。
;; 本步新增（3 个，同属"视口"这一件事）：
;;   on-size           —— 窗口尺寸变化时被回调
;;   get-gl-client-size —— 拿到画布的真实像素尺寸（高 DPI 下 ≠ 逻辑尺寸）
;;   glViewport        —— 设置视口：NDC 映射到画布的哪个像素矩形
;;
;; ★视口的数学：顶点着色器输出的坐标落在 NDC（-1..1 的正方形），与像素无关。
;;   视口就是"把这个正方形放大/平移到像素区"：glViewport(0,0,w,h) 表示
;;   NDC 的 (-1,-1) 贴到像素 (0,0)，(1,1) 贴到 (w,h)。窗口一缩放，就要重算。
;; =========================================================

(require racket/gui opengl)
(require "../lib.rkt")

(define cfg (new gl-config%))
(send cfg set-legacy? #f)
(send cfg set-double-buffered #t)

(define frame
  (new (class frame%
         (augment* [on-close (lambda () (exit 0))])
         (super-new))
       (label "01-05 视口") (width 400) (height 300)))

(define printed? #f)
(define canvas
  (new (class canvas%
         (inherit with-gl-context swap-gl-buffers)

         ;; 尺寸一变（首次显示、拖窗口）→ 视口跟住真实像素尺寸
         (define/override (on-size w h)
           (with-gl-context
            (lambda ()
              (define-values (fw fh) (send this get-gl-client-size))
              (glViewport 0 0 fw fh)
              (glClearColor 0.10 0.12 0.20 1.0))))

         (define/override (on-paint)
           (with-gl-context
            (lambda ()
              (unless printed?
                (set! printed? #t)
                (print-gl-info))
              (glClear GL_COLOR_BUFFER_BIT)
              (send this swap-gl-buffers))))
         (super-new))
       (style '(gl no-autoclear))
       (gl-config cfg)
       (parent frame)))

(send frame show #t)
