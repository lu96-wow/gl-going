# Racket + OpenGL 图形教程（v2 · 全程现代管线）

用 **Racket 自带 GUI（racket/gui）** 开窗口，**opengl 绑定库 + GLSL 330 core** 画图。
整条课程只有**一套现代 API**：core profile 上下文、VAO/VBO、着色器、手写 mat4——
没有固定管线，没有两代 API 混讲。每课**只引入一个新 API/一个图形原理**，
代码自带中文讲解，全部文件在项目根目录独立可运行。

> 课程各代的演进与旧版归档见 [`history/`](history/README.md)。

## 先装好

```sh
raco pkg install opengl        # gl* 函数绑定（若未安装）
# racket/gui、racket/draw、opengl/util 随 Racket 自带
```

素材：`assets/cube.png`、`assets/floor.png`（`assets/gen-textures.rkt` 可重新生成）；
模型 `assets/models/cube.obj`、`suzanne.obj`（来自 GitHub，见文件头部注释）。
所有示例在项目根目录运行：`racket 00-window.rkt`。

## 为什么每课只讲一个概念

本课程的目标是**循序渐进**：每一步在上一课的基础上只加一层新东西，
让"这个新 API 解决什么问题、背后的图形原理是什么"始终清晰。
GLSL 语言本身在 02 课**统一讲一次**（从设计原理出发），之后的课程遇到新语法只补"用法"，不再重复解释设计。

| 课 | 文件 | 本课新 API | 图形原理 |
|---|---|---|---|
| 00 | `00-window.rkt` | frame/canvas/上下文/双缓冲 | core profile、事件驱动绘制、翻页 |
| 01 | `01-triangle.rkt` | VAO、VBO、glDrawArrays、双段着色器 | 顶点数据上 GPU；着色器流水线两段 |
| 02 | `02-glsl-basics.rkt` | **GLSL 语言本身**（类型/构造器/内建/控制流） | 为什么着色器语言长这样；一次讲清，此后各课只补用法 |
| 03 | `03-primitives-ebo.rkt` | 图元类型、EBO、glDrawElements | 拓扑怎么连形状；索引=复用顶点 |
| 04 | `04-uniform-time.rkt` | glUniform*、glGetUniformLocation | uniform=一次 draw 的参数；CPU 驱动动画 |
| 05 | `05-transform.rkt` | glUniformMatrix4fv、手写 mat4 | 模型矩阵 T·R·S；像素世界+正交投影 |
| 06 | `06-3d-depth.rkt` | m4-perspective、深度测试 | 透视除法→近大远小；Z 缓冲→遮挡 |
| 07 | `07-camera.rkt` | m4-look-at、轨道相机（拖拽/滚轮） | 视图矩阵=把世界搬到相机面前 |
| 08 | `08-texture.rkt` | 纹理对象、uv、sampler2D、滤波/环绕 | 纹理映射=给表面"贴图" |
| 09 | `09-light-phong.rkt` | 法线 attribute、逐片元计算 | Phong：环境/漫反射/高光 |
| 10 | `10-blending.rkt` | glEnable(GL_BLEND)、glBlendFunc、glDepthMask | alpha 混合；透明物绘制顺序 |
| 11 | `11-instancing.rkt` | glVertexAttribDivisor、glDrawElementsInstanced | 一次 draw 画 N 个实例 |
| 12 | `12-fbo.rkt` | FBO、纹理/深度挂载、滤镜 | 画到纹理再贴回 = 离屏渲染/后处理 |
| 13 | `13-msaa.rkt` | 多重采样缓冲、glBlitFramebuffer 解析 | 抗锯齿=采样点投票覆盖率 |
| 14 | `14-culling.rkt` | glEnable(GL_CULL_FACE)、glFrontFace | 绕序决定正/背面；剔除省一半片元 |
| 15 | `15-text.rkt` | glyph atlas（R8 纹理、逐字四边形） | 文本=CPU 排版+贴字形格子；UTF-8/中文 |
| 16 | `16-text3d.rkt` | 文字吃 MVP + 深度测试；billboard 朝向矩阵 | 3D 文字=世界里的字形四边形（raylib DrawText3D 同原理）；贴平面 vs 面向相机 |
| 17 | `17-model.rkt` | OBJ 解析（lib.rkt 的 obj-load-file）+ 索引化去重 | 模型加载=文件→顶点数组；GL 不认文件只认数组；flat/平滑法线 |

## 约定与结构（先读 00 再看其余）

- **一个文件 = 一课**：着色器源码内联在文件里。从 02 起用 racket-glsl 的
  `(glsl ...)` S 表达式写 shader（逐条对照见 [`SYNTAX.md`](SYNTAX.md)），
  运行时会展开成等价的 GLSL 文本；字符串形态同样可用，GLSL 源码保持纯
  ASCII（Mesa 不吃非 ASCII），复制单文件即可带走演示。
- **共享库 `lib.rkt`** 只放与窗口无关的工具：`build-program`（编译 GLSL 字符串）、
  手写 mat4（`m4-*`，列主序），并转发 racket-glsl 的 `(glsl ...)`/`vec2…`/`mat4…`
  （每课只 `(require "lib.rkt")` 一处即可）。窗口/事件/每帧绘制全部逐课直写
  racket/gui，不搞"模拟框架"的抽象。
- **每课骨架相同**（00 有逐行注释）：`frame%` 放 `(style '(gl no-autoclear))`
  + `gl-config%` 的 `canvas%`；画=覆写 `on-paint` 在 `with-gl-context` 里调 gl*，
  帧尾 `swap-gl-buffers`；动画=`timer%` 定时 `refresh`；输入=`on-char/on-event`；
  关窗=在 frame 的 `on-close` 里 `exit`（动画 timer 会让进程在关窗后仍不退出）。
- **现代上下文**：所有课都 `(send cfg set-legacy? #f)` 请求 core profile，
  版本可用性在 00 启动时打印（GL 4.5 Core / GLSL 4.50 之类）。
  注意：core profile 里固定管线（glBegin、矩阵栈、固定管线光照…）**不存在**，
  这也是本课程从 01 起就只教现代写法的原因。

## 每课怎么"看"

代码注释就是讲解；建议顺序跑 00→17，每课先运行再看注释。可动手的按键都写在
各文件头部（如 08 按 T/F 切纹理与过滤、10 按 B 关混合对比、12 按 1/2/3/4 切滤镜、
13 按 M 对比锯齿、14 按 C/F/R 折腾绕序、15 按 T 关文字层、16 按 B/D 对比文字朝向与遮挡、
17 按 1/2 切换模型、Space 暂停）。

## 教程之外（下一步可自学）

深度/模板缓冲细节、阴影贴图、几何着色器、PBR 材质、字形缓存与换页
（几千常用汉字超单张纹理时按需烤字）、以及把 mat4 换成完整相机系统。
模型方向：更多格式（PLY/STL/glTF 二进制）与材质组（MTL/mtl）解析、骨骼动画。
