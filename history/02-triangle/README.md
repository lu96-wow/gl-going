# 02 第一个三角形 —— 数据上 GPU + 着色器流水线

按顺序运行，每步只在前一步基础上加 1–2 个新东西。
本文件夹用 01 课的 `run-gl`（骨架已收好），所以每步只剩"本课核心"。

1. `01-shader-load.rkt`   裸写 `glCreateShader`→`glLinkProgram` —— GLSL 文本怎么变成程序
2. `02-build-program.rkt`  **把它收进 `build-program`** —— 之后直接调用
3. `03-vbo.rkt`           `glGenBuffers`/`glBindBuffer`/`glBufferData` —— 顶点数据上显存
4. `04-vao.rkt`           `glVertexAttribPointer` 等 —— VAO 描述"字节的含义"
5. `05-draw.rkt`          `glDrawArrays` + `out/in` 插值 —— 三角形出现
