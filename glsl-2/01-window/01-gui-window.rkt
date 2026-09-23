#lang racket/base
;; =========================================================
;; 01-window/01-gui-window.rkt —— 开一个窗口
;; 运行：racket 01-window/01-gui-window.rkt
;; =========================================================

;; 先引入 racket/gui：Racket 自带的图形界面库，窗口、画布、按钮都在里面。
(require racket/gui)

;; 我们要开一个窗口。窗口在 Racket 里是一个"类"。
;;   类 = 造对象的模板；对象 = 模板造出来的具体东西。
;; racket/gui 已经现成做好了一个窗口类，叫 frame%
;;   （Racket 的命名习惯：类名以 % 结尾）。
;;
;; 但先不能直接拿 frame% 来用——点窗口右上角 X 时，frame% 默认只是
;; "把窗口藏起来"，程序还在后台空转。我们想要"点 X 就整个退出"，
;; 所以要造一个 frame% 的"子类"：继承窗口的一切，只改 X 这一处行为。
;;
;; 子类用 class 写，括号里逐行解释：
;;   (class frame% ...)  = 以 frame% 为父类，定义一个新类
;;   augment* = 给父类已有的方法"追加"动作（先做父类的，再做我们的）
;;   on-close = 点右上角 X 时，系统会自动调用的那个方法
;;   exit     = 结束整个程序（0 表示"正常退出"）
;;   super-new = 调用父类构造器，把"初始化一个窗口"这件事交给 frame% 完成
(define closeable-frame%
  (class frame%
    (augment* [on-close (lambda () (exit 0))])
    (super-new)))

;; 上面只定义了"类"（模板），还没造出窗口。
;; new = 拿模板造一个实例（一个真正出现在屏幕上的窗口），是 Racket 标准写法：
;;   第一个参数 = 用哪个类
;;   之后 (名字 值) 一对一对地写初始化参数。
;; ★这些"名字"不是随便起的：label / width / height 是 frame% 这个类预先定义
;;   好的初始化参数（查文档就知道一个类接受哪些参数），我们只是给它们填值。
;; 本窗口用到的三个：
;;   label        = 标题栏上的文字
;;   width/height = 内容区大小，单位像素（不含标题栏）
(define frame
  (new closeable-frame%
       (label "我的第一个窗口")
       (width 400)
       (height 300)))

;; 窗口刚造出来时是"隐藏"的。show 让它显示：#t = 显示。
;; send = 调用对象的方法，语法 (send 对象 方法名 参数...)。
;;   这里就是：调用 frame 这个对象的 show 方法，参数 #t。
;; 现在窗口出现；点右上角 X 就会执行上面的 exit，程序退出。
(send frame show #t)
