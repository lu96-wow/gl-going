# 08-transform 小结

做了什么: 用 4x4 矩阵做平移/旋转/缩放, 讲清 T·R·S 顺序, 正交投影进入像素世界.

API:
- uniform mat4 + gl-uniform-matrix-4fv  上传 4x4 矩阵(列主序 f32vector)
- gl-uniform-3f                         上传 vec3 颜色
- mat4 构造器                           16 标量 = 列主序; 单标量 = 对角矩阵
- mat4-translate / rot-z / scale        平移(第4列放量) / 旋转(sin/cos) / 缩放(对角线)
- mat4-mult A B                         矩阵乘法 = 变换复合, 从右往左读
- mat4-ortho l r b t n f                正交投影: 像素矩形 -> NDC(含 y 翻转)
- T·R·S                                 先缩放再旋转最后平移(矩阵不交换律)

注意: 矩阵不交换律, 顺序错 = 绕原点公转 vs 原地自转; 列主序元素(r,c)存下标 c*4+r.
