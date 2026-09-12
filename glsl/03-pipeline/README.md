# 03 渲染管线与 GLSL —— 把 OpenGL 地基补全

按顺序运行，每步只在前一步基础上加 1–2 个新东西。

02 课我们「照着抄」把三角形画了出来，但没停下来说明：
GPU 的渲染管线分几段、GLSL 是什么、一段源码怎么变成能跑的程序、又是怎么「启用」的。
本课把这些地基一次讲透——之后每课默认你已经懂了这些，不再重复。

本文件夹的 lib 采用「每课一份复制」：

- `lib-gui.rkt` —— 从 02 复制（未改动）
- `lib.rkt` —— 从 02 复制；03/04/06 步把 `build-program` 拆开重写（编译→链接→通用版）

1. `01-pipeline.rkt`      **渲染管线 7 段**：哪些可编程、哪些固定，每段干什么
2. `02-glsl-what.rkt`     **GLSL 是什么**：C 风格着色语言，运行时编译、GPU 执行
3. `03-shader-object.rkt` **着色器对象**：裸写 `glCreateShader`→`glShaderSource`→`glCompileShader`
4. `04-program-object.rkt`**程序对象**：裸写 `glCreateProgram`→`glAttachShader`→`glLinkProgram`
5. `05-use-program.rkt`   **启用机制**：`glUseProgram` 才是开关；两个程序来回切
6. `06-build-program.rkt` **收进 lib**：`build-program` 一次编译链接任意阶段
