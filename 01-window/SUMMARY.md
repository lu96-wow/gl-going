# 01-window 小结

做了什么: 开一个 OpenGL 窗口, 把重复骨架收成 make-window.

API:
- frame%                   窗口类; new 造实例, show #t 显示
- on-close                 点 X 回调, 里面 exit 才真正退出
- augment* / super-new     子类化: 只给 on-close 追加退出动作
- gl-config%               GL 上下文配置; set-legacy? #f = core profile, set-double-buffered #t = 双缓冲
- canvas%                  GL 画布; (style '(gl no-autoclear)) (gl-config cfg) (parent frame)
- on-paint                 重绘回调(画一帧入口); define/override 重写
- inherit                  把父类方法拿进子类才能用
- with-gl-context          进入当前上下文; 所有 gl-* 必须写在里面
- gl-clear-color           设状态: 记住清屏色
- gl-clear                 执行: 擦颜色缓冲(配 gl-color-buffer-bit)
- swap-gl-buffers          双缓冲翻页
- make-window              封装上面全部; #:title/#:width/#:height/#:draw; 只建不显示
