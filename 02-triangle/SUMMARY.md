# 02-triangle 小结

做了什么: 画第一个三角形, 走通 数据(VBO) + 说明书(VAO) + 程序 + use + draw.

API:
- (glsl ...)                    宏: S 表达式写 GLSL, 展开成 GLSL 文本
- glsl-program-src              取展开后的 GLSL 文本对照读
- build-program                 编译+链接成程序, 自动查状态/日志
- use-program                   设为当前程序(gl-use-program 包装)
- gl-vertex-shader              阶段常量: 顶点着色器
- gl-fragment-shader            阶段常量: 片元着色器
- vec2 / vec                    造 2 维向量 / 打包成连续缓冲
- vec->f32vector                拿连续 f32vector 字节块
- gl-vector-sizeof              算字节数
- u32vector-ref                 从编号数组取第 i 个
- gl-gen-buffers                生成缓冲对象拿编号
- gl-bind-buffer                设为当前操作缓冲(先绑再操作)
- gl-array-buffer               用途常量: 顶点属性数组
- gl-buffer-data                把数据拷进显存
- gl-static-draw                使用提示: 数据基本不变
- gl-gen-vertex-arrays          生成 VAO 拿编号
- gl-bind-vertex-array          绑定 VAO(切片规则记它上面)
- gl-vertex-attrib-pointer      声明槽 N 怎么切(槽号/分量数/类型/归一化/步长/偏移), 记住当前 VBO
- gl-enable-vertex-attrib-array 启用槽 N
- gl-viewport                   视口: NDC 映射到像素矩形; 配 get-gl-client-size
- gl-draw-arrays + gl-triangles 画; 每 3 顶点一个三角形
