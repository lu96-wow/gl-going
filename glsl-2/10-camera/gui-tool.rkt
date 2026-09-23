#lang racket/base
;; =========================================================
;; 10-camera/gui-tool.rkt —— 窗口工具（本课版）
;; =========================================================
;; 从 09-3d-depth/gui-tool.rkt 复制，本课**不加新东西**：
;;   深度缓冲（09 课 set-depth-size 24）和 输入回调（04 课 on-char/on-event）
;;   已经在 make-window 里了；本课只是开始用输入回调，窗口骨架零改动。
;;
;; 本课要学的"相机"纯在 CPU 侧算矩阵（mat4-look-at + 球坐标），不碰窗口，
;; 所以窗口工具保持原样。之后每课从这里复制。
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
  (send cfg set-depth-size 24)       ; 深度缓冲（09 课起深度测试要用）
  ;; 实例化窗口 + 画布，返回两个对象
  (define frame (new closeable-frame% (label title) (width w) (height h)))
  (define canvas (new gl-canvas% (style '(gl no-autoclear)) (gl-config cfg) (parent frame)))
  (values frame canvas))

(define (start-animation canvas [ms 16])
  (new timer%
       (interval ms)
       (notify-callback (lambda () (send canvas refresh)))))

;; 同 02：不顺便 show。先 init 再自己 show。
