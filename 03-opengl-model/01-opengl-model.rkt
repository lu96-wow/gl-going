#lang racket/base
;; =========================================================
;; 03-opengl-model/01-opengl-model.rkt —— OpenGL 的驱动模型（原理 + 设计）
;; =========================================================
;; 本课是纯讲解课：把 01、02 课写过的每个函数背后的"为什么"串起来，
;; 讲清楚 OpenGL 这台机器到底是怎么被"驱动"的。
;;
;; 运行：racket 03-opengl-model/01-opengl-model.rkt
;;   —— 只会打印一行；正文全在本文件的注释里，请从头读到尾。
;; =========================================================

(printf "本课是纯讲解：请直接读本文件的注释（01-opengl-model.rkt）。\n")

;; ══════════════════════════════════════════════════════════
;; 一、总体架构：CPU 是"客户端"，GPU 是"服务器"
;; ══════════════════════════════════════════════════════════
;;
;; OpenGL 不是"一个函数库"那么简单，它是一套"客户端-服务器"协议：
;;
;;   你的程序（CPU） ──发命令──▶ OpenGL 驱动 ──转成硬件命令──▶ GPU
;;     客户端                          （排队/缓冲）              服务器
;;
;; 关键点：命令是**异步**的。CPU 发出一条 gl* 命令，它只是"放进命令流"，
;; 并不等 GPU 执行完就返回了。命令可能在驱动里排队，真正在 GPU 上跑是
;; 之后的事。
;;
;; 这解释了两个我们 02 课遇到的"为什么"：
;;   · 为什么 glCompileShader / glLinkProgram 之后要查状态？
;;     —— 因为调用返回 ≠ 编译/链接成功。它们只是"提交了编译/链接请求"，
;;       必须再查 GL_COMPILE_STATUS / GL_LINK_STATUS 才知道结果。
;;       不查，写错一行 GLSL 只会黑屏、毫无提示。
;;   · 为什么有个"上下文"？
;;     —— 因为命令要发给"某一个" GPU 上下文。上下文 = 一块完整的 GL 状态
;;       环境（颜色、缓冲、程序……），一块画布对应一个上下文。发出命令前
;;       要先"进入"它：with-gl-context 就是干这个的。

;; ══════════════════════════════════════════════════════════
;; 二、状态机：先"记住"，再"执行"
;; ══════════════════════════════════════════════════════════
;;
;; GL 是一台**状态机**。所谓状态，就是"当前正在用哪一套东西"：
;;   当前程序（glUseProgram 设的）、当前缓冲（glBindBuffer 设的）、
;;   当前 VAO（glBindVertexArray 设的）、清屏色（glClearColor 设的）……
;;
;; 于是 API 分成两类：
;;   · 设状态的：glBindBuffer / glBindVertexArray / glUseProgram / glClearColor
;;   · 用状态执行的：glBufferData / glDrawArrays / glClear
;;
;; 典型例子（01 课画第一帧）：
;;   (glClearColor 0.10 0.12 0.20 1.0)   ; 只"记住"清屏色，还没画
;;   (glClear GL_COLOR_BUFFER_BIT)        ; 现在才"用"它把整块颜色缓冲擦掉
;;
;; 这也是为什么 02 课上传 VBO 要先 glBindBuffer：glBufferData 不知道你要
;; 操作哪个缓冲，它只会说"把数据拷进**当前绑定**的那个缓冲"。先绑、再操作，
;; 是 GL 状态机的统一套路。

;; ══════════════════════════════════════════════════════════
;; 三、对象模型：GPU 上的东西，CPU 只拿"编号"
;; ══════════════════════════════════════════════════════════
;;
;; GL 里的着色器、程序、VBO、VAO 都是**对象**。对象活在 GPU 一侧，
;; 你的程序手里只攥着一个整数编号（GLuint）来引用它。这就是"opaque handle"：
;;
;;   创建 → glCreateShader / glCreateProgram / glGenBuffers / glGenVertexArrays
;;   引用 → 拿编号当参数传给别的 gl* 调用
;;   删除 → glDeleteShader / glDeleteProgram（mark 待删，真正释放等不用了）
;;
;; 所以你在 02 课看到的 (u32vector-ref (glGenBuffers 1) 0)，本质就是：
;; "请在 GPU 上开 1 个缓冲对象，把它的编号给我"。
;; （返回 u32vector 是因为 C 的 glGenBuffers 一次能开 n 个，Racket 绑定
;;   沿用了这个签名，这里我们只取第 0 个。）

;; ══════════════════════════════════════════════════════════
;; 四、着色器管线：为什么是"两段"、为什么分"编译/链接"
;; ══════════════════════════════════════════════════════════
;;
;; 现代 OpenGL 的绘制是一段固定管线，其中两个阶段可编程：
;;
;;   顶点数据 ─▶ [顶点着色器] ─▶ 光栅化 ─▶ [片元着色器] ─▶ 像素
;;                每顶点跑一次        三角形铺成像素   每像素跑一次
;;
;;   · 顶点着色器：输入"当前顶点"的数据（in），算"它落在哪"（gl_Position）。
;;   · 光栅化：GPU 自动把三角形铺成一堆像素（这是固定功能，不用写）。
;;   · 片元着色器：输入"当前像素"（含自动插值的值），算"它什么颜色"（out）。
;;
;; 为什么分两步"编译 → 链接"（02 课 build-program）：
;;   · 编译 = 每段 GLSL 单独变"着色器对象"，哪段错报哪段；
;;   · 链接 = 把顶点+片元拼成"程序对象"，顺便检查两段接口（顶点 out ↔ 片元 in）
;;     是否对得上。类比 C：.c → .o → 链接成可执行文件。
;;
;; in 的数据从哪来？靠"槽号"对齐（01 课 layout(location 0)）：
;;   shader 里 (layout (location 0) in vec2 aPos)  说"0 号槽 = vec2 位置"；
;;   CPU 侧 VAO 里 glVertexAttribPointer(0, ...)    说"0 号槽从缓冲这样读"。
;;   两边号码一致，数据就接上了。

;; ══════════════════════════════════════════════════════════
;; 五、画一个三角形的完整数据流（复习 02 课）
;; ══════════════════════════════════════════════════════════
;;
;; 把 02 课串成一条线，每个函数在"搬数据/发命令"里各管一段：
;;
;; ① 写 shader（CPU 侧，纯文本）
;;     (glsl ...) 宏 → 两段 GLSL 字符串
;;
;; ② 编译 + 链接（工具 racket-glsl/tool.rkt 帮我们藏了细节）
;;     build-program (GL_VERTEX_SHADER vs) (GL_FRAGMENT_SHADER fs)
;;       内部 = compile-shader ×2 → link-program，失败自动报错带日志
;;
;; ③ 造顶点数据（CPU 侧）
;;     (vec (vec2 -0.5 -0.5) (vec2 0.5 -0.5) (vec2 0.0 0.5))
;;       3 个顶点，每个 = 2 个 float
;;
;; ④ 上传到 GPU（VBO）
;;     glGenBuffers    —— 开一个缓冲对象，拿编号
;;     glBindBuffer    —— 设为"当前缓冲"（状态机）
;;     glBufferData    —— 把数据拷进当前缓冲
;;
;; ⑤ 描述"怎么切"（VAO —— 连续数据的切片手册）
;;     VBO 是一条连续字节流，VAO 记"怎么在这条流上切出每个顶点的属性"：
;;     槽号/分量数/类型/步长/偏移、槽是否启用。画的时候绑上 VAO 就带上了
;;     整套切法；core profile 里必须有 VAO 才能画。
;;     glGenVertexArrays / glBindVertexArray —— 开 VAO、设为当前
;;     glVertexAttribPointer(0 2 GL_FLOAT #f 8 0) —— 0 号槽：每顶点 2 个
;;                                                   float、步长 8 字节、从 0 读
;;     glEnableVertexAttribArray 0             —— 启用 0 号槽
;;
;; ⑥ 画（每帧 on-paint 里）
;;     use-program（= glUseProgram）    —— 用哪个程序
;;     glBindVertexArray                —— 用哪份"数据说明书"
;;     glDrawArrays GL_TRIANGLES 0 3    —— 从 0 号顶点起，画 3 个（一个三角形）
;;
;; ⑦ 显示
;;     glClearColor + glClear           —— 清屏（与视口无关，清整块缓冲）
;;     glViewport                       —— 视口：把 -1..1 映射到像素矩形
;;                                        （画几何必须有，否则被裁掉）
;;     swap-gl-buffers                  —— 双缓冲翻页：后台换到前台

;; ══════════════════════════════════════════════════════════
;; 六、显示模型：谁在"一直输出图像"
;; ══════════════════════════════════════════════════════════
;;
;; 01 课讲过，这里再精确定位一次：
;;
;;   · racket/gui 是事件驱动，没有 while 主循环。系统在"需要重画"时
;;     （首次显示、被遮挡后露出、拖大拖小）调一次 on-paint。
;;   · on-paint 里我们"画一帧"：清屏 → 画 → swap。画完程序就闲下来。
;;   · **是显示硬件在持续把"前台缓冲"输出到屏幕**——这不需要程序跑循环。
;;   · 双缓冲不是"一直切换"：每次 on-paint 画完 swap 换一次；不重绘时，
;;     屏幕一直显示上一次换上去的那帧。
;;
;; 所以现在的三角形是"画一次、固定显示一帧"。想让它动起来，需要定时器
;; 主动触发重绘——那是下一课「动起来」的主题：timer% 每隔一段时间叫系统
;; 重画，"暂停"就是停掉 timer。

;; ══════════════════════════════════════════════════════════
;; 七、函数速查表（画三角形用到的全部）
;; ══════════════════════════════════════════════════════════
;;
;; 上下文
;;   with-gl-context        进入本画布的 GL 上下文；所有 gl* 必须包在里面
;;
;; 程序（racket-glsl/tool.rkt 已包装）
;;   compile-shader         一段 GLSL → 着色器对象（失败抛错带日志）
;;   link-program           若干着色器对象 → 程序对象（失败抛错带日志）
;;   build-program          宏：(阶段类型 源码)... → 编译+链接一次成程序
;;   use-program            启用程序（glUseProgram）
;;
;; 数据
;;   glGenBuffers           开缓冲对象，拿编号（VBO）
;;   glBindBuffer           设为"当前缓冲"（GL_ARRAY_BUFFER = 顶点属性缓冲）
;;   glBufferData           把数据拷进当前缓冲（GL_STATIC_DRAW = 基本不变）
;;   glGenVertexArrays      开 VAO 对象，拿编号
;;   glBindVertexArray      设为"当前 VAO"
;;   glVertexAttribPointer  描述某槽号的数据布局（类型/分量数/步长/偏移）
;;   glEnableVertexAttribArray  启用某槽号
;;
;; 画
;;   glClearColor           记住清屏色
;;   glClear                擦缓冲（GL_COLOR_BUFFER_BIT = 颜色缓冲）
;;   glViewport             把 -1..1 映射到像素矩形
;;   glDrawArrays           画顶点（GL_TRIANGLES = 每 3 个一组三角形）
;;   swap-gl-buffers        双缓冲翻页
;;
;; 数据构造（racket-glsl/rename-vector.rkt）
;;   vec2 / vec             造顶点位置（f32vector / 连续缓冲）
;;   vec->f32vector         拿底层连续 f32vector（上传用）

;; ══════════════════════════════════════════════════════════
;; 八、总纲：按"显卡 / CPU / 内存"把 OpenGL 分成五块逻辑
;; ══════════════════════════════════════════════════════════
;;
;; 硬件现实 = 两台机器 + 一条慢通道：
;;
;;   CPU（你的 Racket 程序）                      GPU（显卡）
;;   ├─ 内存 RAM：                               ├─ 显存 VRAM：
;;   │    f32vector（顶点数据）                  │    VBO、着色器/程序对象
;;   │    GLSL 字符串（源码）                    ├─ 计算单元：成千上万，并行跑着色器
;;   │                                          └─ 输出：前台缓冲 → 屏幕
;;   └────── 发 GL 命令 ──（PCIe，慢）──▶
;;
;; 两条由硬件决定的设计：
;;   ① 中间通道慢 → 数据要"一次批量上传"（VBO）；命令是异步排队（所以
;;      编译/链接要查状态，不是调用返回就成功）。
;;   ② GPU 强在并行 → 着色器被设计成"一个顶点/一个像素的纯函数"，
;;      同一份代码、不同数据、同时执行，看不到别人。
;;
;; 状态机 = 连接两台机器的"当前指针"：
;;   状态就是"当前用哪套东西"（当前程序 / 当前缓冲 / 当前 VAO）。
;;   设状态：glBind*、glUseProgram、with-gl-context（进入上下文）
;;   用状态执行：glBufferData、glDrawArrays、glClear
;;   上下文 = 一整份状态的容器；一块画布一个上下文。
;;
;; 把画三角形的所有函数，按"五块逻辑"归类（对照着读第 5 节）：
;;
;;   ① 准备数据（CPU + 内存，纯 Racket）
;;        (glsl ...) 宏   →  GLSL 源码字符串
;;        vec / vec2      →  顶点数据 f32vector
;;
;;   ② 建 GPU 对象（在显存里开对象，CPU 只拿编号）
;;        compile-shader / link-program / build-program → 着色器、程序
;;        glGenBuffers / glGenVertexArrays              → VBO、VAO
;;
;;   ③ 传数据 + 描述（过 PCIe，进显存）
;;        glBufferData                                    → 顶点批量进 VBO
;;        glVertexAttribPointer / glEnableVertexAttribArray → VAO 切片手册
;;
;;   ④ 设状态（状态机，指定"当前用哪套"）
;;        glBindBuffer / glBindVertexArray / use-program / with-gl-context
;;
;;   ⑤ 执行 + 显示（GPU 干活 + 翻页）
;;        glDrawArrays                                    → 并行跑着色器
;;        glClearColor / glClear / glViewport / swap-gl-buffers → 清屏/映射/翻页
;;
;; 一句话记住 OpenGL：CPU 在内存里造数据、一次批量搬进显存、用状态机指定
;; "当前用哪套"，然后一条 glDrawArrays 让 GPU 并行跑着色器，最后翻页显示。
;; =========================================================
