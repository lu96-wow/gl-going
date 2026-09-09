#lang racket/base
;; =========================================================
;; 00-window.rkt —— 窗口与骨架：一块能画 OpenGL 的画布
;; 运行：racket 00-window.rkt    关闭：点窗口右上角 X（自动退出）
;; =========================================================
;; 本课是之后每一课都会用的骨架，只有四个零件：
;;   frame%      = 窗口（标题栏 + 内容区）
;;   canvas%     = 窗口里的画布；给它两个参数就变成 OpenGL 画布：
;;                   (style '(gl no-autoclear))  ← 'gl = 画 OpenGL 内容
;;                                                  'no-autoclear = 别让系统擦背景
;;                   (gl-config cfg)             ← cfg = (new gl-config%)，
;;                                                 上下文配置（racket/gui 自带）
;;   with-gl-context = 进入 GL 上下文。所有 gl* 调用都必须在它里面发生，
;;                     它内部负责把这块画布的上下文设为当前
;;   swap-gl-buffers = 把画好的后台缓冲翻到屏幕（双缓冲）
;;
;; ★本课程用现代 OpenGL：窗口上下文请求 core profile（不含任何固定管线），
;;   着色器一律写 GLSL 330 core。就一行：
;;     (send cfg set-legacy? #f)   ← racket/gui 默认是兼容(旧式)上下文；
;;                                   我们不需要固定管线，直接要 core
;;
;; ★racket/gui 是事件驱动，没有手写"主循环"：
;;   系统想重画时调用画布的 on-paint —— 我们在里面画一帧。
;;   需要动画（下一课起）就另加 timer% 定时让画布 refresh，触发下一次 on-paint。
;; =========================================================

(require racket/gui      ; frame% canvas% timer% gl-config%
         opengl)         ; gl* 函数与常量
(require "lib.rkt")      ; 本课程共享工具（这里只用 print-gl-info）

;; ---- ① GL 上下文配置（core profile）----
(define cfg (new gl-config%))
(send cfg set-legacy? #f)      ; ★请求现代 core 上下文（不含固定管线）
(send cfg set-double-buffered #t)

;; ---- ② 窗口 + OpenGL 画布 ----
(define frame
  (new (class frame%
         (augment* [on-close (lambda () (exit 0))])
         (super-new))
       (label "00 窗口骨架") (width 800) (height 600)))

(define printed-info? #f)
(define canvas
  (new (class canvas%
         (inherit with-gl-context swap-gl-buffers)

         ;; 尺寸一变（首次显示、拖动窗口）→ 视口跟住"真实像素尺寸"
         (define/override (on-size w h)
           (with-gl-context
            (lambda ()
              (define-values (fw fh) (send this get-gl-client-size))
              (glViewport 0 0 fw fh)
              (glClearColor 0.10 0.12 0.20 1.0))))

         ;; 一帧 = on-paint：进上下文 → 清屏 →（画）→ 翻页
         (define/override (on-paint)
           (with-gl-context
            (lambda ()
              (unless printed-info?        ; 打一次版本，确认 core 生效
                (set! printed-info? #t)
                (print-gl-info))
              (glClear GL_COLOR_BUFFER_BIT)
              (send this swap-gl-buffers))))
         (super-new))
       (style '(gl no-autoclear))
       (gl-config cfg)
       (parent frame)))

;; ---- ③ 显示窗口；点 X 关窗即退出 ----
;; frame% 的 on-close 方法在用户点关闭按钮时被调用，在里面 exit 即可。
;; （后面有动画 timer 的课也一样：timer 会让进程在窗口关掉后仍不退出，
;;   所以每课都靠 on-close 显式退出。）
;;
;; 直接 exit 就够了，不用手动释放资源：进程一退出，操作系统会回收它
;; 占用的全部内存、窗口，以及 GPU 里的 VAO/VBO/纹理（它们挂在 GL 上下文
;; 上，上下文随进程一起销毁）。glDelete* 只在"程序运行中反复创建/销毁
;; 资源"时才需要——本课程只有 13 课的窗口缩放重建 FBO 用到了它。
(send frame show #t)
