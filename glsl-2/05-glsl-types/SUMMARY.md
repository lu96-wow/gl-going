# 05-glsl-types 小结

做了什么: 把 shader 当"像素 = uv 的纯函数", 讲类型/构造器/swizzle/运算符/内建函数.

API/GLSL:
- glsl-stride-bytes            按类型清单算字节步长/偏移(不手写魔法数字)
- vec4                         Racket 侧 4 维向量构造器
- float/int/bool/vec2/vec3/vec4 类型; 浮点字面量必须带小数点
- 构造器 (vec3 vUV 0.5)        分量凑齐即可拼大向量(升维)
- swizzle (yx c)->c.yx         重排/取分量
- + - * / < > = != and or not  逐分量运算/比较/逻辑
- 三元 (if c a b)              表达式版 if(挑值)
- length/normalize/dot         几何函数(距离/单位化/点积)
- mix/clamp/fract/sin/cos      插值/夹范围/取小数/三角
- gl-vertex-attrib-pointer     两个槽(位置0, uv1), 只偏移不同
