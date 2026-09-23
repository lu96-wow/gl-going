#lang racket/base
;; =========================================================
;; window.rkt —— 极简窗口骨架（独立自包含，不依赖 glsl/ 备份）
;; 复制自课程 lib-gui.rkt 的模式：core-profile 窗口 + GL 画布。
;; 返回 (values frame canvas)，**不显示**：调用者先初始化再 (send frame show #t)。
;; #:draw 每帧在 with-gl-context 里执行，画完自动翻页。
;; =========================================================

(require racket/gui opengl)

(provide make-window
         (all-from-out opengl)      ; gl* 常量与函数
         (all-from-out racket/gui)) ; timer% / canvas% / send …

(define (make-window #:title title
                     #:draw draw
                     #:width [w 640]
                     #:height [h 480])
  (define cfg (new gl-config%))
  (send cfg set-legacy? #f)          ; core profile
  (send cfg set-double-buffered #t)
  (send cfg set-depth-size 24)

  (define frame
    (new (class frame%
           (augment* [on-close (lambda () (exit 0))])
           (super-new))
         (label title) (width w) (height h)))

  (define canvas
    (new (class canvas%
           (inherit with-gl-context swap-gl-buffers)
           (define/override (on-size sw sh)
             (with-gl-context
              (lambda ()
                (define-values (fw fh) (send this get-gl-client-size))
                (glViewport 0 0 fw fh))))
           (define/override (on-paint)
             (with-gl-context
              (lambda ()
                (draw)
                (send this swap-gl-buffers))))
           (super-new))
         (style '(gl no-autoclear))
         (gl-config cfg)
         (parent frame)))

  (values frame canvas))
