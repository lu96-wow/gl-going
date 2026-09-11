# 07 进入 3D —— 透视投影 + 深度缓冲

按顺序运行，每步只在前一步基础上加 1–2 个新东西。

06 课在 2D 像素世界里摆弄矩阵。本课升到 3D，只多两个概念，却让"**近大远小**"
和"**前后遮挡**"同时成立：透视投影 + 深度缓冲。顶点位置从 vec2 变成 vec3，
顺便认识真正的 3D 物体——立方体。

本文件夹的 lib 采用"每课一份复制"；**本课的 lib.rkt 把矩阵升级到 3D**：

- `lib-gui.rkt` —— 从 06 复制（本课未改动；make-window 已申请 24 位深度缓冲，见下）

★深度缓冲需要两半：**窗口侧申请**（`gl-config%` 的 `set-depth-size`，已由 make-window
  办好）+ **每帧使用**（`glEnable(GL_DEPTH_TEST)` + 清 `GL_DEPTH_BUFFER_BIT`，本课 02 步学）。
  没有前一半，深度测试静默失效；没有后一半，深度缓冲只是占着不用。
- `lib.rkt` —— 从 06 复制，升级：mat4-translate / mat4-scale 加 z 参数，新增 mat4-rot-x / mat4-rot-y；
  第 3 步裸写 mat4-perspective，第 4 步收进 lib

1. `01-cube.rkt`      **3D 顶点 + 立方体（线框）**：vec3 aPos、8 角 + 边索引，绕 y 旋转
2. `02-solid.rkt`     **实心面 + 深度缓冲**：24 顶点 36 索引，glEnable(GL_DEPTH_TEST)
3. `03-perspective.rkt` **透视投影**：mat4-perspective + 透视除法 w → 近大远小
4. `04-lib.rkt`       **收进 lib**：mat4-perspective（节奏步）
5. `05-demo.rkt`      **综合**（无新语法）：三颗立方体、不同 z 深度、各自旋转
