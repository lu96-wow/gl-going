#lang racket/base
;; =========================================================
;; 09-3d-depth/gui-tool.rkt —— 窗口工具（本课版，加深度缓冲）
;; =========================================================
;; 从 04-animate/05-gui-tool.rkt 复制，本课新增一件事：
;;   上下文配置加 (send cfg set-depth-size 24) —— 申请 24 位深度缓冲。
;;
;; ★深度缓冲要"两半"，这是窗口侧那一半：申请。另一半"使用"
;;   （gl-enable gl-depth-test + 每帧清 gl-depth-buffer-bit）在 02 步学。
;;   没有前一半，深度测试会静默失效；没有后一半，深度缓冲只是占着不用。
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
      (define/override (on-size w h)
        (with-gl-context
         (lambda ()
           (define-values (fw fh) (send this get-gl-client-size))
           (gl-viewport 0 0 fw fh))))
      (define/override (on-paint)
        (with-gl-context
         (lambda ()
           (draw)
           (send this swap-gl-buffers))))
      (define/override (on-char e) (when char-cb (char-cb e)))
      (define/override (on-event e) (when event-cb (event-cb e)))
      (super-new)))
  ;; 上下文配置
  (define cfg (new gl-config%))
  (send cfg set-legacy? #f)          ; core profile
  (send cfg set-double-buffered #t)  ; 双缓冲
  (send cfg set-depth-size 24)       ; ★深度缓冲（本课新增，08 课起深度测试要用）
  ;; 实例化窗口 + 画布，返回两个对象
  (define frame (new closeable-frame% (label title) (width w) (height h)))
  (define canvas (new gl-canvas% (style '(gl no-autoclear)) (gl-config cfg) (parent frame)))
  (values frame canvas))

(define (start-animation canvas [ms 16])
  (new timer%
       (interval ms)
       (notify-callback (lambda () (send canvas refresh)))))

;; 同 02：不顺便 show。先 init 再自己 show。
