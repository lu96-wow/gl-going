# 06 图元与索引 —— glDrawArrays 的第一个参数 + EBO

按顺序运行，每步只在前一步基础上加 1–2 个新东西。

02 课到现在，我们只会 `(glDrawArrays GL_TRIANGLES 0 3)` 画三角形。本课把
"**顶点流怎么连成形状**"讲透：`glDrawArrays` 的第一个参数（图元类型）决定
连线方式；再用 **EBO（索引缓冲）**解决"重复存顶点"的浪费。04 课埋下的
"四边形 6 个顶点却只有 4 个角"的伏笔，本课回收。

本文件夹的 lib 采用"每课一份复制"：

- `lib-gui.rkt` —— 从 05 复制（本课未改动）
- `lib.rkt` —— 从 05 复制（本课未改动；图元/EBO 都是标准 gl* 调用，内联在步骤里）

1. `01-color.rkt`     **顶点颜色 aColor**：vec3 属性 + `concat-vecs` 拼交错数据 → 彩色三角形
2. `02-points.rkt`    **GL_POINTS + gl_PointSize**：每个顶点一个点，点的大小
3. `03-lines.rkt`     **GL_LINES / GL_LINE_STRIP / GL_LINE_LOOP**：线段 / 折线 / 闭合折线
4. `04-triangles.rkt` **GL_TRIANGLES / GL_TRIANGLE_STRIP / GL_TRIANGLE_FAN**：三角形与省顶点
5. `05-ebo.rkt`       **EBO 索引缓冲 + glDrawElements**：4 个角 + 6 个编号，不再重复存
6. `06-demo.rkt`      **综合**（无新语法）：一窗口 点/折线/条带四边形 ↔ EBO 四边形
