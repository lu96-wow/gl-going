# 04-animate 小结

做了什么: 让画面动起来(timer + uniform), 能算 FPS, 能暂停/继续; 05 步把 timer + 键盘收进 gui-tool.

API:
- timer%                         定时器; 01 裸写, 05 收成 start-animation
- (send canvas refresh)          排重画事件 -> on-paint -> draw
- start-animation(canvas ms)     05 步收的工具: 封装 timer+refresh, 返回 ticker, 默认 16ms
- (send timer stop/start)        暂停/继续
- make-window #:on-char/#:on-event  04 裸写 on-char, 05 收成参数
- current-inexact-milliseconds   取毫秒时间, 减 start-ms 算 t
- uniform                        GLSL 声明: 本次 draw 全体共用参数
- uniform-location               查 uniform 位置号(初始化查一次)
- gl-uniform-1f                  每帧上传 1 个 float; 须在 use-program 之后
- on-char + get-key-code         键盘回调; 空格 = #\space
- (send frame set-label)         改窗口标题
- (send canvas focus)            把键盘焦点交给画布, 否则 on-char 收不到
