#lang racket/base
;; =========================================================
;; 01-window/06-lib-gui.rkt —— 第六步：把骨架收进 lib-gui.rkt
;; 运行：racket glsl/01-window/06-lib-gui.rkt
;; =========================================================
;; 前 5 步把窗口骨架（frame/canvas/上下文/清屏/翻页/视口）逐块讲透了。
;; 那 40 行每课都一模一样，只有"画什么"在变。所以收成 lib-gui.rkt 的 make-window。
;;
;; make-window 第一版做一件事：建窗口 + 接管骨架，你只提供 #:draw。
;; 完整源码在同文件夹 lib-gui.rkt；它是前 5 步的逐块对应：
;;   03 步  gl-config% + set-legacy?/set-double-buffered
;;   02 步  frame% + on-close → exit
;;   04 步  canvas%：on-paint → (with-gl-context → draw → swap)
;;   05 步  on-size → glViewport
;;
;; ★注意：make-window 只"建"不"显示"——它把 frame 和 canvas 交给你。
;;   这样 02 课起，你可以先在 canvas 的上下文里做 GL 初始化，再自己 show。
;;   （本步还没有需要初始化的东西，所以建完直接 show。）
;;
;; ★从本步起每课只写：draw + make-window + show，聚焦本课核心。
;;   这套"裸写 → 收进 lib → 复用"的节奏，整门课通用。
;; =========================================================

(require "lib-gui.rkt")   ; make-window + gl*

;; 每帧画什么（先定义，供 make-window 引用）
(define (draw)
  (glClearColor 0.10 0.12 0.20 1.0)
  (glClear GL_COLOR_BUFFER_BIT))

(define-values (frame canvas)
  (make-window #:title "01-06 骨架收进 make-window"
               #:width 400 #:height 300
               #:draw draw))

(send frame show #t)
