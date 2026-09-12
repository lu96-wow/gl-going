#lang racket/base
;; =========================================================
;; lib-gui.rkt —— 窗口骨架库
;;
;; 从 08-3d-depth 复制；本课第 3 步（03-input.rkt）扩展：
;;   make-window 增加 #:on-char / #:on-event（键盘 / 鼠标输入回调）。
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
;;   #:on-char [cb]   —— 键盘事件回调 (e)（缺省不装）
;;   #:on-event [cb]  —— 鼠标事件回调 (e)（缺省不装）
;;
;; 为什么不顺便 show：本课起，每课都要在画布的 GL 上下文里做初始化
;;   （编译着色器、上传数据），而上下文挂在 canvas 上——所以把 canvas 交出去，
;;   让调用者先 init，再自己 (send frame show #t)。
(define (make-window #:title title
                     #:draw draw
                     #:width [w 400]
                     #:height [h 300]
                     #:on-char [char-cb #f]
                     #:on-event [event-cb #f])
  (define cfg (new gl-config%))
  (send cfg set-legacy? #f)
  (send cfg set-double-buffered #t)
  (send cfg set-depth-size 24)    ; 申请 24 位深度缓冲（08 课起深度测试要用）

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
           ;; 注意：define/override 必须写在类体顶层，不能用 when 包——
           ;; 所以这里总是定义，回调为 #f 时转发给父类（默认什么都不做）。
           (define/override (on-char e)
             (if char-cb (char-cb e) (super on-char e)))
           (define/override (on-event e)
             (if event-cb (event-cb e) (super on-event e)))
           (super-new))
         (style '(gl no-autoclear))
         (gl-config cfg)
         (parent frame)))

  (values frame canvas))
