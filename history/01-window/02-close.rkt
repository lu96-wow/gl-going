#lang racket/base
;; =========================================================
;; 01-window/02-close.rkt —— 第二步：点 X 真正退出进程
;; 运行：racket 01-window/02-close.rkt
;; =========================================================
;; 上一步：窗口能开，但点 X 只是把它藏起来——进程还活着（事件循环在空转）。
;; 本步新增（2 个）：
;;   on-close —— 窗口被点 X 关闭时，系统回调的方法（"钩子"）
;;   exit     —— 结束整个进程
;;
;; 怎么做：racket/gui 的类都可以被"子类化并只改一个方法"。不再直接
;;   (new frame%)，而是 (new (class frame% ...)) 造一个 frame% 的子类，
;;   只给 on-close 加一个动作，其余行为照旧。
;; =========================================================

(require racket/gui)

(define frame
  (new (class frame%
         ;; augment*：给父类已有的方法"追加一个动作"（不是整体替换）。
         ;; on-close 在用户点 X 时被系统调用，我们在里面 exit 结束进程。
         (augment* [on-close (lambda () (exit 0))])
         ;; super-new：调用父类构造器，照常完成窗口初始化。
         (super-new))
       (label "01-02 点 X 退出") (width 400) (height 300)))

(send frame show #t)
