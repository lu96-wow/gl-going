#lang racket/base
;; =========================================================
;; 04-animate/01-timer.rkt —— 让"帧"持续发生（timer%）
;; 运行：racket 04-animate/01-timer.rkt    点 X = 退出
;; =========================================================

;; 02 课的画面是静态的：GL 不会自己动，你不叫它重画，它就停在最后一帧。
;; 动画 = 帧 × 每帧不同。第一步先解决"怎么让画面每帧重画"。
;;
;; 本步新增一个类：
;;   timer% —— Racket 的定时器：每隔 interval 毫秒，调用一次 notify-callback
;;
;; ★帧循环的机制（动画 = 帧 × 每帧不同）：
;;   GL 和 racket/gui 一样是事件驱动，没有 while 主循环。要让画面持续更新，
;;   就用一个 timer 定时"敲门"：
;;
;;     timer ──每 16ms──▶ (send canvas refresh) ──▶ on-paint ──▶ draw ──▶ 翻页
;;
;;   refresh = "给系统排一个重画事件"，系统随后调 on-paint 跑我们的 draw。
;;   interval 16 毫秒 ≈ 每秒 60 帧（1000 / 60 ≈ 16.7）。

(require "../02-triangle/04-gui-tool.rkt")  ; make-window（带视口）

(define start-ms (current-inexact-milliseconds))  ; 程序启动时刻，之后每帧减它算 t

;; 每帧：清屏色随 sin 呼吸（红通道在 0..1 之间起伏）——证明帧真的在一帧帧发生。
;; t = 程序启动以来的秒数；sin(t) ∈ -1..1，*0.5+0.5 映射到 0..1。
(define (draw)
  (define t (/ (- (current-inexact-milliseconds) start-ms) 1000.0))
  (define breathe (+ 0.5 (* 0.5 (sin t))))
  (glClearColor breathe 0.12 0.20 1.0)
  (glClear GL_COLOR_BUFFER_BIT))

(define-values (frame canvas)
  (make-window #:title "04-01 让帧发生" #:width 400 #:height 400 #:draw draw))

;; 定时器：每 16ms 敲一次 → 触发重画。
;;   interval        = 间隔毫秒数（16 ≈ 60 帧/秒）
;;   notify-callback = 到点回调：里面 (send canvas refresh) 让系统重画一帧
(define ticker
  (new timer%
       (interval 16)
       (notify-callback (lambda () (send canvas refresh)))))

(send frame show #t)
