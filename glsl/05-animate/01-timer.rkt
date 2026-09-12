#lang racket/base
;; =========================================================
;; 05-animate/01-timer.rkt —— 第一步：让"帧"持续发生（timer%）
;; 运行：racket glsl/05-animate/01-timer.rkt    点 X = 退出
;; =========================================================
;; 04 课的画面是静态的：GL 本身**不会自己动**，你不叫它重画，它就停在最后一帧。
;; 要让画面动，第一步不是改 shader，而是先解决"怎么让画面**每帧重画**"。
;;
;; 本步新增（1 个）：
;;   timer% —— Racket 的定时器：每隔 interval 毫秒，调用一次 notify-callback
;;
;; ★帧循环的机制（动画 = 帧 × 每帧不同）：
;;   GL 和 racket/gui 一样是事件驱动，没有你手写的 while 循环。要让画面持续
;;   更新，就用一个 timer 定时"敲门"，每敲一次 → (send canvas refresh)
;;   → 系统触发 on-paint → 我们的 draw 跑一遍 → 换页到屏幕。这就是游戏循环
;;   在 racket/gui 里的最小形态：
;;
;;     timer% ──每 16ms──▶ refresh ──▶ on-paint ──▶ draw ──▶ 换页
;;
;;   interval 16 毫秒 ≈ 每秒 60 帧（1000 / 60 ≈ 16.7）。
;;
;; 本步视觉：不画 shader，只用"清屏色"做最小演示——每帧让红色通道随 sin 呼吸。
;;   你看到背景从暗红到亮红来回起伏，就证明"帧真的在一帧一帧发生"了。
;;   （时间用 current-inexact-milliseconds 量：返回开机到现在经过的毫秒数，
;;     减去 start-ms 得到"从本程序启动到现在"的秒数 t。）
;; =========================================================

(require "lib-gui.rkt")
(require "lib.rkt")

(define start-ms (current-inexact-milliseconds))  ; 程序启动时刻，之后每帧减它算 t

;; 每帧做什么：清屏色随 sin 呼吸（红通道在 0..1 之间起伏）。
;; t 每秒 +1；sin(t) 在 -1..1 之间来回摆，*0.5+0.5 映射到 0..1。
(define (draw)
  (define t (/ (- (current-inexact-milliseconds) start-ms) 1000.0))
  (define breathe (+ 0.5 (* 0.5 (sin t))))
  (glClearColor breathe 0.12 0.20 1.0)
  (glClear GL_COLOR_BUFFER_BIT))

(define-values (frame canvas)
  (make-window #:title "05-01 让帧发生" #:width 400 #:height 400 #:draw draw))

;; 定时器：每 16ms 敲一次 → 触发重画。★timer% 是 racket/gui 的类。
;; notify-callback 是"到点回调"：里面调 (send canvas refresh) 让系统重画。
(define ticker
  (new timer% (interval 16)
       (notify-callback
        (lambda () (send canvas refresh)))))

(send frame show #t)
