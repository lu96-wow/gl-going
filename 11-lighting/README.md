# 11 光照（进阶）—— Lambert 余弦定律 + Phong/Blinn-Phong + 模型谱系

按顺序运行，每步只在前一步基础上加 1–2 个新东西。

前几课颜色都"写死"，物体没有立体感。本课加光照，并**把原理讲透**——
不只教"怎么算"，还讲清"为什么这样算"，以及"**光照不止这一种实现**"。

先建立心智模型：我们看到物体的明暗，本质是**光线打到表面的角度**决定的
能量密度（Lambert 余弦定律，02 步）。Phong/Blinn-Phong 只是在这个物理直觉上
叠加的经验模型（03/04 步），而光照还有一大片谱系：flat/Gouraud/Phong 着色、
PBR 物理渲染……（04 步集中梳理）。

本文件夹的 lib 采用"每课一份复制"：

- `lib-gui.rkt` —— 从 10 复制（本课未改动）
- `lib.rkt` —— 从 10 复制，新增 `cube-normal-verts`/`cube-normal-idx`（带法线的立方体）

1. `01-normal.rkt`  **法线**：表面朝向 attribute + 世界空间变换 + 把法线当颜色可视化
2. `02-lambert.rkt` **Lambert 余弦定律**：N·L = cosθ，为什么"正对光源的面最亮"（漫反射）
3. `03-phong.rkt`   **Blinn-Phong 三件套**：环境光 + 漫反射 + 镜面高光（半向量）
4. `04-models.rkt`  **光照模型谱系**：Phong↔Blinn-Phong 切换 + flat/Gouraud/Phong/PBR 梳理
5. `05-demo.rkt`    **综合**（无新语法）：中央立方体 + 绕行彩色立方体 + 绕圈点光源
