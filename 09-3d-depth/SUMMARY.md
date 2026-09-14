# 09-3d-depth 小结

做了什么: 顶点升到 vec3 画立方体; 透视投影(近大远小) + 深度缓冲(前后遮挡) 让 3D 成立.

API:
- vec3 aPos                   顶点位置多一个 z(深度), 3D 用 vec4(aPos, 1.0)
- gl-lines                    线框图元(12 条边 = 24 索引)
- gl-enable gl-depth-test     开深度测试(一次即可)
- gl-clear(bitwise-ior gl-color-buffer-bit gl-depth-buffer-bit)  每帧清颜色+深度
- gl-config% set-depth-size 24  窗口侧申请深度缓冲(gui-tool 本课新增)
- mat4-perspective fovy aspect near far  透视投影; 让 w=-z, GPU 透视除法 -> 近大远小
- mat4-rot-x / mat4-rot-y    绕 x/y 轴旋转(3D 新增)
- mat4-translate/scale 加 z  3D 版平移/缩放
- cube-verts / cube-idx      6面x4顶点(24顶点36索引), 拆面是因为每面颜色不同

注意: 深度缓冲两半(窗口申请 + 使用时开/清); 实心立方体 24 顶点(角不共享); near/far 别太极端.
