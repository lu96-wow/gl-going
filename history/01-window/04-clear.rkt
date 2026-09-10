#lang racket/base
;; =========================================================
;; 01-window/04-clear.rkt —— 第四步：画出第一帧（清屏）
;; 运行：racket 01-window/04-clear.rkt
;; =========================================================
;; 上一步：画布是黑的。本步在画布上"画一帧"——先把整个画面清成一种颜色。
;; 本步新增（5 个，同属"画一帧"这一件事）：
;;   on-paint        —— 系统要重画时回调的方法（我们在这里画）
;;   with-gl-context —— 进入本画布的 GL 上下文（所有 gl* 调用必须在里面）
;;   glClearColor    —— 设置"清屏色"（状态机：先设值）
;;   glClear         —— 用清屏色擦掉缓冲（真正执行）
;;   swap-gl-buffers —— 双缓冲翻页：把后台画好的缓冲换到屏幕
;;
;; ★为什么双缓冲：如果直接往屏幕上写，擦到一半的中间态会被看见 → 闪烁。
;;   双缓冲 = 一块"后台"缓冲随便画，画完一次整体翻到前台。glClear 画在后台，
;;   swap-gl-buffers 才把后台整块换到屏幕。
;; =========================================================

(require racket/gui opengl)
(require "../lib.rkt")   ; print-gl-info：打印 GL 版本，确认 core 上下文生效

(define cfg (new gl-config%))
(send cfg set-legacy? #f)
(send cfg set-double-buffered #t)

(define frame
  (new (class frame%
         (augment* [on-close (lambda () (exit 0))])
         (super-new))
       (label "01-04 第一帧") (width 400) (height 300)))

(define printed? #f)
(define canvas
  (new (class canvas%
         ;; 把这两个方法从父类"拿进来"，类体内才能直接调用
         (inherit with-gl-context swap-gl-buffers)

         (define/override (on-paint)
           (with-gl-context            ; 进入 GL 上下文
            (lambda ()
              (unless printed?
                (set! printed? #t)
                (print-gl-info))       ; 打一次 GL/GLSL 版本，确认 core 生效
              ;; 状态机：先设清屏色（只是记住，不执行），再执行清屏
              (glClearColor 0.10 0.12 0.20 1.0)   ; 深蓝灰 (r g b a)
              (glClear GL_COLOR_BUFFER_BIT)        ; 擦颜色缓冲
              (send this swap-gl-buffers))))       ; 翻到屏幕
         (super-new))
       (style '(gl no-autoclear))
       (gl-config cfg)
       (parent frame)))

(send frame show #t)
