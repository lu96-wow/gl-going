#lang racket/base
;; =========================================================
;; lib-gui.rkt —— 窗口骨架库（第一版）
;;
;; 本文件在 01-window/06-lib-gui.rkt 一步创建。
;; 把本课前 5 步裸写的骨架（frame/canvas/上下文/清屏/翻页/视口）收成 make-window。
;; 后面每一课会复制本文件，并在需要时往里加功能。
;; =========================================================

(require racket/gui opengl)
(provide make-window
         (all-from-out opengl)     ; gl*
         (all-from-out racket/gui)) ; send 等（调用者要用 (send frame show #t)）

;; make-window：建一个 core-profile 窗口 + GL 画布，接管整套骨架
;;   （cfg / frame / canvas / on-close / on-size / on-paint / 翻页）。
;;   返回 (values frame canvas)，**不显示**——调用者自己 init 后再 show。
;;   #:draw —— 每帧执行（在 with-gl-context 里，画完自动翻页）。
;;             draw 在显示之后才会被调用，所以它可以引用后面才定义的变量。
;;
;; 为什么不顺便 show：02 课起，每课都要在画布的 GL 上下文里做初始化
;;   （编译着色器、上传数据），而上下文挂在 canvas 上——所以把 canvas 交出去，
;;   让调用者先 init，再自己 (send frame show #t)。
(define (make-window #:title title
                     #:draw draw
                     #:width [w 400]
                     #:height [h 300])
  (define cfg (new gl-config%))
  (send cfg set-legacy? #f)
  (send cfg set-double-buffered #t)

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
