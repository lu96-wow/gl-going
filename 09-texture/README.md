# 09 纹理 —— uv 坐标 + sampler2D + 采样参数

按顺序运行，每步只在前一步基础上加 1–2 个新东西。

前几课每个顶点都亲手填颜色。本课换成"贴图"——照片级的细节不可能逐顶点手填，
而是把整张图片作为**纹理**贴到表面上。新概念：把图片上传成纹理、用 uv 坐标
采样、设置过滤/环绕/mipmap 让贴图不糊不花不闪。

素材在 `assets/`（cube.png / floor.png，可用 gen-textures.rkt 重新生成）。

本文件夹的 lib 采用"每课一份复制"：

- `lib-gui.rkt` —— 从 08 复制（本课未改动）
- `lib.rkt` —— 从 08 复制，第 4 步把 load-tex（纹理加载器）收进 lib

1. `01-texture.rkt`  **上传纹理 + sampler2D**：read-bitmap → glTexImage2D → texture() 采样，四边形贴图
2. `02-filter.rkt`   **过滤 + 环绕**：NEAREST/LINEAR、REPEAT/CLAMP_TO_EDGE
3. `03-mipmap.rkt`   **mipmap**：多级预缩小图，远处不闪烁
4. `04-cube.rkt`     **立方体贴图**：uv 映射到 3D 面 + 收 load-tex 进 lib
5. `05-demo.rkt`     **综合**（无新语法）：地板棋盘平铺 + 旋转立方体贴图
