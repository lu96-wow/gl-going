#lang racket/base
;; =========================================================
;; 01-window/03-first-frame.rkt —— 画出第一帧（清屏）
;; 运行：racket 01-window/03-first-frame.rkt
;; =========================================================

;; 终于要"画"了。这次引入 opengl 库。
;; 它的命名规则：gl 开头的都是函数（glClearColor、glClear），
;;               GL_ 开头的都是常量（GL_COLOR_BUFFER_BIT 等）。
(require racket/gui opengl)

;; ① 窗口类 + ② 上下文配置，同 02-gl-window.rkt（不再解释）。
(define closeable-frame%
  (class frame%
    (augment* [on-close (lambda () (exit 0))])
    (super-new)))
(define cfg (new gl-config%))
(send cfg set-legacy? #f)          ; core profile
(send cfg set-double-buffered #t)  ; 双缓冲

;; ③ 画布要子类化，因为"画一帧"要写在 on-paint 里。
;;
;; ★先搞清楚"什么时候会画"：racket/gui 是事件驱动的，没有 while 主循环。
;;   系统只在"需要重画"时调一次 on-paint，典型时机：
;;     窗口第一次显示、被别的窗口遮挡后露出、拖大拖小。
;;   画完这一帧就停，直到下一次"需要重画"。
;;   ——所以现在不是"每秒 60 帧在跑"，而是"有事才画一帧"。
(define gl-canvas%
  (class canvas%
    ;; inherit：with-gl-context / swap-gl-buffers 是父类 canvas% 的两个方法。
    ;; Racket 的类里，子类不能直接使用父类的方法名——要先用 inherit 把它们
    ;; "拿进来"，本类代码里才能调用 (with-gl-context ...) 和 swap-gl-buffers。
    (inherit with-gl-context swap-gl-buffers)

    ;; define/override = 整体替换父类方法（父类的 on-paint 默认什么都不画）。
    (define/override (on-paint)
      ;; with-gl-context：本画布的方法。它接收一个"要执行的函数"，先把本画布的
      ;; GL 上下文设为"当前"，再执行那个函数。所有 gl* 调用都必须写在这个函数里：
      ;; GL 是状态机，状态存在"当前上下文"里，不进去就不知道该改谁的状态。
      (with-gl-context
       (lambda ()
         ;; 画一帧 = 清屏。GL 状态机的两步：
         (glClearColor 0.10 0.12 0.20 1.0)  ; ① 先"记住"清屏色（深蓝灰 r g b a，各 0~1）
         ;; GL_COLOR_BUFFER_BIT 是 opengl 的常量："颜色缓冲" = 存每个像素颜色的
         ;; 那块内存（GL 还有深度/模板等别的缓冲，这个常量指定擦哪一种）。
         (glClear GL_COLOR_BUFFER_BIT)       ; ② 再执行：把颜色缓冲擦成那个色
         ;; swap-gl-buffers：双缓冲的"翻页"。双缓冲 = 两块缓冲：前台（正显示在
         ;; 屏幕）和后台（你在上面画）。画完交换两块，后台变前台。如果直接画
         ;; 前台，画到一半的中间态会被看见 → 闪烁。
         ;; this = 当前对象自己。方法里 (send this 方法...) = 调用"我自己"的方法。
         (send this swap-gl-buffers))))
    (super-new)))

;; ④ 实例化 + 显示。
(define frame
  (new closeable-frame% (label "01-03 第一帧") (width 400) (height 300)))
(define canvas
  (new gl-canvas%
       (style '(gl no-autoclear))
       (gl-config cfg)
       (parent frame)))

;; 运行后窗口是一整片深蓝灰色——这就是"画出来的一帧"。
;; 拖大拖小都还是整片蓝：glClear 清的是"整块颜色缓冲"，和视口无关。
;; （视口 glViewport 是后面画几何图形时才需要的东西，这里先不引入。）
;;
;; ★当前代码下，图像是怎么"一直显示"的？
;;   画完 + swap 之后，程序就闲下来了（事件循环在等下一个事件），
;;   是显示硬件在持续把"前台缓冲"输出到屏幕——这不需要程序跑循环。
;;   双缓冲也不是"一直切换"：每次 on-paint 画完，swap 才换一次；
;;   不重绘时，屏幕就一直显示上一次换上去的那帧。
;;
;; ★想要"每帧都在变"（动画）？需要定时器主动触发重绘——那是下一课
;;   （动起来）的主题：timer% 每隔一段时间叫系统重画；"暂停"= 停掉 timer。
;;   这一课：画一次 = 固定一帧。
(send frame show #t)
