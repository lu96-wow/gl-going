# 02 第一个三角形 —— 着色器 + 数据上 GPU

按顺序运行，每步只在前一步基础上加 1–2 个新东西。
本文件夹的 lib 采用"每课一份复制"：

- `lib-gui.rkt` —— 从 01-window 复制（本课未改动）
- `lib.rkt` —— 本课新建（03 创建 `build-program`；04 转发 rename-vector 的 `vec2`/`vec`）

从本课起，每课的固定结构是：**先 `make-window` 拿画布 → 在上下文里 `define` 初始化 → 自己 `show`**（不再有 `#:init`）。

1. `01-shader.rkt`        **着色器是什么**：顶点着色器 / 片元着色器 / 流水线（只读不编译）
2. `02-shader-load.rkt`   裸写 `glCreateShader`→`glLinkProgram`（首次用 `send canvas with-gl-context` 初始化）
3. `03-build-program.rkt` **创建 `lib.rkt` 的 `build-program`**；之后直接调用
4. `04-vbo.rkt`           `glGenBuffers`/`glBindBuffer`/`glBufferData`；顶点数据用 `vec`（若干 `vec2`）写
5. `05-vao.rkt`           `glVertexAttribPointer` 等 —— VAO 描述"字节的含义"
6. `06-draw.rkt`          `glDrawArrays` + `out/in` 插值 —— 三角形出现
