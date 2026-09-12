# 18 模型加载 —— 把 OBJ 文件变成能画的顶点数组

按顺序运行，每步只在前一步基础上加 1–2 个新东西。

前面的课里立方体/四边形都是"手写顶点数组"。真实场景里模型在文件里
（OBJ / glTF…），而 GL 根本不认识"文件"——它只认 VAO/VBO/EBO 里的数组。
本课把模型文件变成数组，核心是一条三步流水线（raylib 的 LoadModel/DrawModel
就是它的现成实现）：

```
raylib                        本课（Racket + lib.rkt）
-------------------------     -----------------------------------
LoadModel 解析文件 → Mesh     obj-load-file 读文本 → obj-mesh（纯 CPU）
rlLoadMesh 上传 GPU           mesh->vao：verts→VBO、idx→EBO、配 attribute
rlDrawMesh → glDrawElements   glDrawElements（索引绘制）
```

所以"导入模型"**没有新 GL 函数**（VBO/EBO/VAO 是旧知识），新东西是两步
CPU 侧的活：把文本解析成数组 + 认识数组排布灌进 VBO/EBO。

本文件夹的 lib 采用"每课一份复制"：

- `lib-gui.rkt` —— 从 17-text 复制（本课未改动）
- `lib.rkt` —— 从 17-text 复制，第 4 步（04-lib.rkt）把 `obj-load-file` /
  `obj-mesh` 收进本文件（纯 CPU 解析，见文件末尾）

前 3 步是纯 CPU（无窗口，直接打印统计）；第 4 步收 lib；第 5 步才开窗口画。

1. `01-parse.rkt` **OBJ 是什么 + 逐行读**：v/vt/vn/f 四种行，其余跳过
2. `02-index.rkt` **索引化去重**：角点 = (顶点,uv,法线) 三编号去重，suzanne 省 80%
3. `03-normal.rkt` **法线 + 归一化**：vn 直接 / 缺 vn 叉积补；包围盒居中缩放
4. `04-lib.rkt`   **收进 lib.rkt**：obj-load-file（节奏步，打印验证）
5. `05-draw.rkt`  **画出来**：mesh → VAO + 光照，1/2 切换 cube/suzanne
6. `06-demo.rkt`  **综合**（无新语法）：相机环绕 + 自转 + Space 暂停 + 切换

★关键设计两处（03/05 步正文各讲一次）：

- **索引化去重**：OBJ 的面共享角点；按"角点 = (v,vt,vn) 三编号"去重，
  suzanne 的 968 个三角形只存 590 个顶点（省 80%）。
- **flat / 平滑法线**：文件带 vn（建模软件烘焙好的）就直接用；缺 vn 的面
  现场叉积算面法线（flat，不与邻面共享 → 硬边）。05/06 步按 1/2 切换即可
  看到两种：cube 的 vn 是逐面法线 → 硬边棱角；suzanne 的 vn 是平滑法线 →
  圆润表面。
