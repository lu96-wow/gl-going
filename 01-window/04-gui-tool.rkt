#lang racket/base
;; =========================================================
;; 01-window/04-gui-tool.rkt —— 窗口骨架工具
;; =========================================================
;; 把前 3 步（01 窗口 / 02 上下文 / 03 清屏）收成一个函数 make-window。
;; 之后每课只要三行：
;;   (require "04-gui-tool.rkt")
;;   (define-values (frame canvas) (make-window #:title "..." #:draw draw))
;;   (send frame show #t)
;;
;; make-window 帮你做四件事：
;;   ① 配好 GL 上下文（core profile + 双缓冲）
;;   ② 建好"点 X 退出"的窗口
;;   ③ 建好画布，每帧自动：清屏 → 调你的 #:draw → 翻页
;;   ④ 把 frame、canvas 两个对象交还给你（不自动 show，见文件末尾）
;; =========================================================

(require racket/gui opengl)
(provide make-window
         (all-from-out racket/gui)   ; frame% canvas% 等
         (all-from-out opengl))      ; gl* 函数、GL_* 常量

;; #:title 窗口标题；#:width/#:height 窗口大小；#:draw 每帧画什么（可选，不给就只清屏）。
;; 注意：你的 #:draw 会在 GL 上下文里被调用，里面可以直接写 gl* 调用。
(define (make-window #:title title
                     #:draw [draw void]
                     #:width [w 400]
                     #:height [h 300])
  ;; 点 X 退出的窗口类（同 01 步）
  (define closeable-frame%
    (class frame%
      (augment* [on-close (lambda () (exit 0))])
      (super-new)))
  ;; 画布类：每帧 清屏 → draw → 翻页（同 03 步）
  (define gl-canvas%
    (class canvas%
      (inherit with-gl-context swap-gl-buffers)
      (define/override (on-paint)
        (with-gl-context
         (lambda ()
           (glClearColor 0.10 0.12 0.20 1.0)  ; 清屏色（深蓝灰）
           (glClear GL_COLOR_BUFFER_BIT)
           (draw)                              ; 你的每帧内容
           (send this swap-gl-buffers))))
      (super-new)))
  ;; 上下文配置（同 02 步）
  (define cfg (new gl-config%))
  (send cfg set-legacy? #f)          ; core profile
  (send cfg set-double-buffered #t)  ; 双缓冲
  ;; 实例化窗口 + 画布，返回两个对象
  (define frame (new closeable-frame% (label title) (width w) (height h)))
  (define canvas (new gl-canvas% (style '(gl no-autoclear)) (gl-config cfg) (parent frame)))
  (values frame canvas))

;; ★为什么不顺便 show？下一课起，每课都要先"在画布的 GL 上下文里初始化"
;;   （编译着色器、上传数据），上下文挂在 canvas 上；所以把 canvas 交给你，
;;   先 init 再自己 show。本课还没这需求，但接口先按这个设计。
