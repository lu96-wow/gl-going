# 07-primitives 小结

做了什么: 讲 gl-draw-arrays 的第一个参数(图元类型)决定顶点怎么连; 再用 EBO 索引去重, 收掉 05 课"四边形 6 顶点 4 角"的伏笔.

API:
- aColor               每个顶点一份的 vec3 颜色(第二个 attribute)
- concat-vecs          把不同宽度的 f32vector 连成一块交错缓冲(vec2 位置 + vec3 颜色)
- glsl-size 'vec3      一个类型的元素数(3), 传给 gl-vertex-attrib-pointer 的 size
- glsl-stride-bytes 'vec2 'vec3 交错步长(20 字节); 单类型 = offset
- gl-points + gl_PointSize  每个顶点一个点; 点大小(像素, 只对点生效)
- gl-lines / gl-line-strip / gl-line-loop  两两成段 / 顺次折线 / 折线闭合
- gl-triangles / gl-triangle-strip / gl-triangle-fan  独立三角形 / 条带 / 扇形(省顶点)
- gl-draw-arrays(图元, 起点, 数量)  按图元类型画顶点流
- gl-bind-buffer gl-element-array-buffer + gl-buffer-data  上传索引缓冲(EBO)
- gl-draw-elements(图元, 数量, 索引类型, 起点)  按索引取顶点画
- gl-unsigned-short     u16vector 的索引类型

注意: EBO 要在 VAO 还绑着时绑(VAO 会记住它); strip/fan 靠共享边省顶点, EBO 靠编号去重.
