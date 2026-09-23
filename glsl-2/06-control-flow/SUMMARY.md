# 06-control-flow 小结

做了什么: 给 shader 加控制流 - 分支(when/unless/cond), 循环(for), 自写函数(define), 拼成一个会动的着色玩具.

API/GLSL:
- when/unless/cond      语句版分支(挑一段代码执行); 展开成 if / else if
- for (int i 1) (<= i 6) (++ i) 循环; 边界必须是编译期常数, 不能跑到 uniform 为止
- (define (wave (float x) (float phase)) float ...) 自写函数; 参数 (类型 名), 返回类型在参数表后, 体尾表达式自动 return
- 三元 if              表达式版分支(挑一个值), 05 课已学, 和 when/unless/cond 互补
- uniform uTime + gl-uniform-1f + uniform-location  复习 04 课: 每帧传时间
- start-animation      复习 04 课 05 步: 封装 timer+refresh, 每帧重画

注意: for 边界要写死; if 是表达式, 语句位置的分支用 when/unless/cond.
