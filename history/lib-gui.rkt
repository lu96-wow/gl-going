#lang racket/base
;; =========================================================
;; lib-gui.rkt —— 窗口骨架的简化（01 课第 6 步引入）
;;
;; 01 课前 5 步逐块讲了：frame%、canvas%、gl-config%、with-gl-context、
;; glClear、swap-gl-buffers、glViewport。那套代码每课都一模一样，只有
;; "首次初始化什么"和"每帧画什么"在变。run-gl 把这套骨架收成一个调用，
;; 之后每课只写 #:init / #:draw 两个 thunk，聚焦本课核心。
;; =========================================================

(require racket/gui opengl)
(provide run-gl
         (all-from-out opengl))   ; 转发 gl*，让本文件的调用者不用再 require opengl

;; run-gl：开一个 core-profile 窗口 + OpenGL 画布，并接管整套骨架
;;   （cfg / frame / canvas / on-close / on-size / on-paint / 翻页）。
;;   调用者只需提供：
;;     #:init  —— 首次进入 GL 上下文时执行一次（建程序/缓冲等）
;;     #:draw  —— 每帧执行（在 with-gl-context 里，画完自动翻页）
;;   可选：
;;     #:char      —— 按键回调（后面的输入课用）
;;     #:timer-ms  —— 动画刷新间隔，给 n 毫秒就自动每 n 毫秒重画一帧
;;
;; 为什么 #:init 要放进回调、不能直接写在顶层：gl* 调用必须在 GL 上下文里，
;; 而上下文要等画布第一次显示才存在——所以 run-gl 在第一次 on-paint 时
;; 替你调一次 #:init。
(define (run-gl #:title title
                #:width [w 400]
                #:height [h 300]
                #:init [init-thunk (lambda () (void))]
                #:draw draw-thunk
                #:char [char-handler (lambda (e) (void))]
                #:timer-ms [timer-ms #f])
  (define cfg (new gl-config%))
  (send cfg set-legacy? #f)          ; core profile（01-03 讲）
  (send cfg set-double-buffered #t)  ; 双缓冲（01-04 讲）

  (define frame
    (new (class frame%
           (augment* [on-close (lambda () (exit 0))])   ; 点 X 退出（01-02 讲）
           (super-new))
         (label title) (width w) (height h)))

  (define init? (box #f))
  (define canvas
    (new (class canvas%
           (inherit with-gl-context swap-gl-buffers)
           ;; 视口跟住窗口（01-05 讲）
           (define/override (on-size sw sh)
             (with-gl-context
              (lambda ()
                (define-values (fw fh) (send this get-gl-client-size))
                (glViewport 0 0 fw fh))))
           (define/override (on-char e) (char-handler e))
           ;; 一帧 = 首次先 init → 每帧 draw → 翻页（01-04 讲）
           (define/override (on-paint)
             (with-gl-context
              (lambda ()
                (unless (unbox init?)
                  (set-box! init? #t)
                  (init-thunk))
                (draw-thunk)
                (send this swap-gl-buffers))))
           (super-new))
         (style '(gl no-autoclear))
         (gl-config cfg)
         (parent frame)))

  (when timer-ms
    (new timer% (interval timer-ms)
         (notify-callback (lambda () (send canvas refresh)))))
  (send frame show #t)
  (void))   ; 不返回画布：让脚本顶层不被打印成 (object ...)
