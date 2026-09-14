#lang racket/base
;; =========================================================
;; 04-animate/05-gui-tool.rkt —— 窗口工具（本课版，收进 timer + 键盘）
;; =========================================================
;; 本课 01 步裸写了 timer（每 16ms refresh 让帧持续发生），04 步裸写了
;; on-char（键盘钩子 + canvas focus）。这两块和 02 课的窗口骨架一样是
;; "每课都要抄"的样板，所以本步收进工具：
;;
;;   ① make-window 加 #:on-char / #:on-event —— 键盘/鼠标回调，不再手写整个窗口
;;   ② start-animation —— 把 01 步的 timer 收成一句 (start-animation canvas 16)
;;
;; 之后每课要动画，只需 make-window + start-animation 两行，窗口细节全部藏好。
;; =========================================================

(require racket/gui "../racket-glsl/opengl-rename.rkt")
(provide make-window start-animation
         (all-from-out racket/gui)   ; frame% canvas% timer% 等
         (all-from-out "../racket-glsl/opengl-rename.rkt"))   ; gl-* 函数与常量

(define (make-window #:title title
                     #:draw [draw void]
                     #:width [w 400]
                     #:height [h 300]
                     #:on-char [char-cb #f]
                     #:on-event [event-cb #f])
  ;; 点 X 退出的窗口类（同 01 课）
  (define closeable-frame%
    (class frame%
      (augment* [on-close (lambda () (exit 0))])
      (super-new)))
  ;; 画布类：视口（on-size）+ 重绘（on-paint）+ 输入（on-char / on-event）。
  (define gl-canvas%
    (class canvas%
      (inherit with-gl-context swap-gl-buffers)
      ;; on-size：尺寸变化（含第一次显示）时把视口铺满绘图区。
      (define/override (on-size w h)
        (with-gl-context
         (lambda ()
           (define-values (fw fh) (send this get-gl-client-size))
           (gl-viewport 0 0 fw fh))))
      ;; on-paint：每次重绘 进上下文 → draw → 翻页（清屏在 draw 里）。
      (define/override (on-paint)
        (with-gl-context
         (lambda ()
           (draw)
           (send this swap-gl-buffers))))
      ;; 输入回调：给了就转发，没给（#f）就什么都不做（= 默认行为）。
      (define/override (on-char e) (when char-cb (char-cb e)))
      (define/override (on-event e) (when event-cb (event-cb e)))
      (super-new)))
  ;; 上下文配置
  (define cfg (new gl-config%))
  (send cfg set-legacy? #f)          ; core profile
  (send cfg set-double-buffered #t)  ; 双缓冲
  ;; 实例化窗口 + 画布，返回两个对象
  (define frame (new closeable-frame% (label title) (width w) (height h)))
  (define canvas (new gl-canvas% (style '(gl no-autoclear)) (gl-config cfg) (parent frame)))
  (values frame canvas))

;; start-animation：每 ms 毫秒 (send canvas refresh) 一次 → 系统调 on-paint → draw。
;; 返回 ticker；暂停/继续用 (send ticker stop) / (send ticker start ms)。
;; 这就是 01 步裸写的 timer 收成的一句（1000/60 ≈ 16.7，默认 16ms ≈ 60 帧/秒）。
(define (start-animation canvas [ms 16])
  (new timer%
       (interval ms)
       (notify-callback (lambda () (send canvas refresh)))))

;; 同 02：不顺便 show。先 init 再自己 show。
