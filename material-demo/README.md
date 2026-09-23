# material：把手动命名 + 手动取值 变成 自动

## 1. 原理：uniform 在 GPU 侧，CPU 只有一个「名字 → 整数」的查询口

shader 里写：

```glsl
uniform vec4 uColor;
```

- **名字只存在于 shader 源码里（GPU 侧）**。CPU 侧没有「uniform 变量」这个东西。
- CPU 能做的只有一个查询：`glGetUniformLocation(program, "uColor")` → 返回一个整数 **location**（找不到返回 `-1`）。
- 设值：`glUniform4f(loc, …)`（或 `glUniform1f` / `glUniformMatrix4fv` …），**而且当前 `glUseProgram` 的必须是这个 loc 所属的 program**。

于是每加一个 uniform，CPU 侧要**手动**做三件事：

1. 手写名字字符串 `"uColor"`（拼写必须和 shader 里**一字不差**）；
2. 手动调 `glGetUniformLocation` 拿到整数 loc；
3. 手动记住这个 loc 属于哪个 program，并手动挑对 `glUniform*` 里那一个。

**任何一步错了，GL 不报错** —— 只是那个 uniform 没设上。见 `silent-bug.rkt`。

## 2. material 把这三步变成自动

关键点：`(glsl ...)` 是宏，**编译期就解析了 shader 的声明**。所以「这个 shader 有哪些 uniform、各是什么类型」程序自己就知道，不必人肉再抄一遍。

| 手动做法 | material 自动 | 靠什么 |
|---|---|---|
| 手写名字 `"uColor"` | 从 shader 的 `(uniform vec4 uColor)` 读出 | `glsl-program-forms` / `glsl-form-text`（shader 的表面语法） |
| 手动 `glGetUniformLocation` | 建材质时**一次性**查好，藏进对象 | `hash：名字 → loc` |
| 手动挑 `glUniform4f` | 按**声明里的类型**自动分派 | `hash：名字 → 类型` |

## 3. 代码形状对比

**裸 GL**（现在 `glsl/` 里那种）：

```racket
(define prog       (build-program (GL_VERTEX_SHADER vs) (GL_FRAGMENT_SHADER fs)))
(define loc-offset (glGetUniformLocation prog "uOffset"))   ; ← 手写名字 + 手动查
(define loc-color  (glGetUniformLocation prog "uColor"))
...
(glUseProgram prog)
(glUniform2f loc-offset 0.5 0.0)          ; ← 手动配对 + 手动挑 2f
(glUniform4f loc-color 1.0 0.0 0.0 1.0)   ; ← 手动挑 4f
```

**material**：

```racket
(define mat (make-material 'my (list (list GL_VERTEX_SHADER vs)
                                     (list GL_FRAGMENT_SHADER fs))))
...
(with-material mat
  (material-set! mat 'uOffset '(0.5 0.0))          ; ← 只写名字；loc 自动；2f 自动
  (material-set! mat 'uColor  '(1.0 0.0 0.0 1.0))) ; ← 4f 自动
```

- 名字写错（比如 scene 里没有 `uScreen`）→ **立刻报错**；裸 GL 是静默忽略。
- 类型自动分派：`float→glUniform1f`、`vec2→glUniform2f`、`vec3→glUniform3f`、
  `vec4→glUniform4f`、`mat4→glUniformMatrix4fv`、`sampler2D→glUniform1i`。
- `(with-material m …)` = 切到该 program + 在作用域里设值；`(material-replay! m)`
  可把上次设过的值重放一遍。

## 4. 跑起来

```bash
racket material-demo/demo.rkt        # 真窗口：左半 = 裸 GL，右半 = material
racket material-demo/silent-bug.rkt  # 不用 GPU：看裸 GL 的静默忽略
```

`demo.rkt` 运行时会在终端打印 material 自动读到的 uniform：

```
material `pulse' 自动读到的 uniform：(uColor uOffset uPulse)
```

这些名字**没有在任何地方手写**，是从 `vs` / `fs-pulse` 的 `(glsl ...)` 声明里读出来的。

## 5. 它不解决什么

- 不减少 `glUseProgram` / `glUniform*` 的调用次数；
- 不管 UBO（`layout(std140) uniform Camera {…}`，那是跨 program 共享 uniform 的另一套机制）；
- 不做 program 缓存 / shader 变体；
- 不是场景图、不是渲染状态机。

它只解决一件事：**program 和 uniform 的手工配对，以及配错时的静默失败。**
