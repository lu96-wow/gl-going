# 01 窗口与画布 —— racket/gui + OpenGL 上下文

按顺序运行，每步只在前一步基础上加 1–2 个新东西。

前 5 步裸写讲原理；第 6 步把这些重复骨架收进**本文件夹的 `lib-gui.rkt`**（第一版）。

1. `01-frame.rkt`    `frame%` + `show` —— 开一个窗口
2. `02-close.rkt`    `on-close` + `exit` —— 点 X 真正退出
3. `03-canvas.rkt`   `canvas%` + `(style '(gl no-autoclear))` + `gl-config%` —— OpenGL 画布
4. `04-clear.rkt`    `with-gl-context` + `glClear` + `swap-gl-buffers` —— 画出第一帧
5. `05-viewport.rkt` `on-size` + `glViewport` —— 视口跟住窗口缩放
6. `06-lib-gui.rkt`  **创建本文件夹的 `lib-gui.rkt`**（`make-window` 第一版）
