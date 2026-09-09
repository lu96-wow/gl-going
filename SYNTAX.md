# GLSL → S 表达式 语法规范 v1

目标：显式类型、与 GLSL 一一对应（薄层）、循环保留、递归/展开由 Racket 宏在编译期完成、`(raw "...")` 逃逸口兜底。

## 0. 设计原则

1. **显式类型**：所有声明都写类型，和 GLSL 一样；不做类型推断。
2. **一一对应**：每个 S 表达式对应一段确定的 GLSL 文本，不生成接口块/隐式临时名（临时变量只在"组合层"需要时生成）。
3. **循环保留**：`for`/`while`/`do-while` 照写，不强迫递归。
4. **递归/展开是 Racket 层**：`named-let`/`unroll` 等宏在**编译期**展开成核心 DSL（for/while/直线代码），不生成递归 GLSL。
5. **逃逸口**：任何冷门 GLSL 用 `(raw "...")` 直接内联。

## 1. 字面量

| 写法 | 生成 |
|---|---|
| `1.0` `0.5` `3.14159` | 原样（浮点必须带小数点，同 GLSL） |
| `0` `1` `16` | 原样（整数；`1/2` 会按 Racket 有理数解析，禁止出现，用 `1.0`/`2.0`） |
| `#t` / `#f` | `true` / `false` |
| `uTime` `vUV` `gl_Position` | 原样标识符 |

## 2. 类型（GLSL 原名，显式写出）

标量 `float int uint bool double void`；向量 `vec2 vec3 vec4 ivec2..4 uvec2..4 bvec2..4 dvec2..4`；
矩阵 `mat2 mat3 mat4 mat2x3 mat3x4 ...`；采样/图像 `sampler2D samplerCube image2D ...`（注意 GLSL 大小写，如 `sampler2D`）。

**构造器** = 类型名当函数调用：`(vec3 1.0 2.0 3.0)`、`(vec4 v2 0.0 1.0)`、`(vec3 0.15)`（标量广播）。

**数组类型** = `(array 内层类型 [大小])`，内层类型可以是任何类型（含数组）：

```racket
((array float 4) weights)             ; float[4] weights;
((array vec3) v)                      ; vec3[] v;（大小缺省 = 未定长，用于几何/细分 in/out、参数）
((array (array float 4) 3) m)         ; float[4][3] m;（数组的数组）
(in (array vec3 4) aPos)              ; in vec3[4] aPos;
```

数组构造器 = 类型位当函数调用：`((array float 4) 1.0 2.0 3.0 4.0)` → `float[4](1.0, 2.0, 3.0, 4.0)`。

## 3. 声明（全局/局部，含限定符）

声明形如 `(QUAL* TYPE name [init])`，其中 `QUAL*` 是可选的任意限定符（`in/out/uniform/const/flat/smooth/noperspective/centroid/sample/patch/invariant/precise/shared/readonly/writeonly/restrict/coherent/volatile`）或 `layout(...)`：

```racket
(in vec2 aPos)                          ; in vec2 aPos;
(out vec4 FragColor)                    ; out vec4 FragColor;
(uniform float uTime)                   ; uniform float uTime;
(uniform sampler2D uTex)                ; uniform sampler2D uTex;
(const float k 0.5)                     ; const float k = 0.5;
(layout (location 0) in vec2 aPos)      ; layout(location = 0) in vec2 aPos;
(flat in vec3 v)                        ; flat in vec3 v;（插值/存储限定符都放最前）
(patch out vec3 p)                      ; patch out vec3 p;（细分着色器）
(shared vec4 acc)                       ; shared vec4 acc;（compute 共享内存）
(float d (length p))                    ; 局部声明+初始化：float d = length(p);
(vec2 p (- (* vUV 2.0) 1.0))            ; vec2 p = vUV * 2.0 - 1.0;
```

**独立 layout**（无类型/变量名，用于 compute/几何/early_fragment_tests）：

```racket
(layout (local_size_x 8) in)                      ; layout(local_size_x = 8) in;
(layout (triangle_strip) (max_vertices 3) out)    ; layout(triangle_strip, max_vertices = 3) out;
```

**接口块（UBO/SSBO/in-out block）** = 类型位写 `(block 名 (字段类型 字段名) ...)`：

```racket
(layout (std140) (binding 0) uniform (block Camera (mat4 view) (mat4 proj)) cam)
; → layout(std140, binding = 0) uniform Camera { mat4 view; mat4 proj; } cam;

(layout (std430) (binding 1) buffer (block Particles ((array vec4) position)) particles)
; → layout(std430, binding = 1) buffer Particles { vec4[] position; } particles;

(out (block VertexData (vec3 normal) (vec2 uv)) vs_out)
; → out VertexData { vec3 normal; vec2 uv; } vs_out;
```

成员访问用 `.field`/`aref`：`(.view cam)` → `cam.view`；`(aref (.position particles) i)` → `particles.position[i]`。

字段也可以带限定符或 `layout`（字段级 layout，与顶层声明同构）：

```racket
(layout (std140) uniform (block Camera
                          (layout (offset 0) mat4 view)
                          (layout (offset 64) mat4 proj)) cam)
; → layout(std140) uniform Camera { layout(offset = 0) mat4 view; layout(offset = 64) mat4 proj; } cam;

(out (block V (flat vec3 normal) (vec2 uv)) vs_out)
; → out V { flat vec3 normal; vec2 uv; } vs_out;
```

## 4. 表达式

### 运算符（前缀，n 元）

| S 表达式 | GLSL |
|---|---|
| `(+ a b c)` | `a + b + c` |
| `(- a b)` / `(- a)` | `a - b` / `-a` |
| `(* a b)` `(/ a b)` `(% a b)` | `* / %` |
| `(= a b)` `(!= a b)` `< <= > >=` | `== != < <= > >=` |
| `(and a b)` `(or a b)` `(not a)` | `&&` `\|\|` `!` |
| `(bit-and a b)` `(bit-or a b)` `(bit-xor a b)` `(bit-not a)` | `& \| ^ ~` |
| `(<< a b)` `(>> a b)` | `<< >>` |

### 赋值

| S 表达式 | GLSL |
|---|---|
| `(set! x v)` | `x = v;`（语句） |
| `(+= x v)` `(-= x v)` `(*= x v)` `(/= x v)` `(%= x v)` | `x += v;` 等 |
| `(<<= x v)` `(>>= x v)` `(&= x v)` `(\|= x v)` `(^= x v)` | `x <<= v;` 等 |
| `(++ x)` `(-- x)` | `++x;` `--x;`（前缀；后缀极罕见，需要就用逃逸口） |

### 三元（表达式 if）

`(if c t e)` → `c ? t : e`

### swizzle（图形核心，简写形式）

`(COMPONENTS vector)`，其中 COMPONENTS 是由 `x y z w / r g b a / s t p q` 组成的符号：

```racket
(x p)      ; p.x
(xy v)     ; v.xy
(rgb c)    ; c.rgb
(zyx v)    ; v.zyx
```

### 字段访问 / 下标

```racket
(.field s)   ; s.field
(aref a i)   ; a[i]
```

### 函数调用 / GLSL 内建

普通函数调用形式，内建全部自然可用：

```racket
(mix a b t)  (clamp x lo hi)  (length v)  (normalize v)
(dot a b)  (cross a b)  (fract x)  (texture tex uv)  (sin x)
```

## 5. 语句

| S 表达式 | GLSL |
|---|---|
| 任意表达式 | `expr;` |
| `(begin s ...)` | `{ ... }`（块/作用域） |
| `(TYPE name init)` | 局部声明（见 §3） |
| `(set! x v)` 等 | 赋值语句 |
| `(when c s ...)` | `if (c) { ... }` |
| `(unless c s ...)` | `if (!(c)) { ... }` |
| `(cond [c s ...] ... [else s ...])` | `if/else if/else` 链 |
| `(for (TYPE var init) cond update s ...)` | `for (TYPE var = init; cond; update) { ... }` |
| `(while cond s ...)` | `while (cond) { ... }` |
| `(do-while s ... cond)` | `do { ... } while (cond);` |
| `(break)` `(continue)` | `break;` `continue;` |
| `(return v)` / `(return)` | `return v;` / `return;` |
| `(discard)` | `discard;` |

> 注意：`if` 是**表达式**（三元），语句级分支用 `when`/`unless`/`cond`——避免二义性，和 Scheme 惯例一致。

## 6. 函数定义

```racket
(define (name (TYPE arg) ...) RET body...)

(define (main) void ...)                         ; void main() { ... }
(define (sq (float x)) float (* x x))            ; float sq(float x) { return x*x; }  ← 见下
(define (f (in vec3 p) (out float r)) void ...)  ; void f(in vec3 p, out float r) {...}
```

- 参数写成 `(TYPE name)`，`in/out/inout` 放在类型前：`(inout vec3 p)`。
- 返回类型直接写在参数表之后（不加 `:`）；`void` 显式写。
- **返回值的约定**：body 最后一个表达式若有值即作为返回值（`(define (sq (float x)) float (* x x))` → `return x*x;`）；需要提前返回用 `(return v)`；`void` 函数不生成 return。

## 7. struct

```racket
(struct Light (vec3 pos) (float intensity))   ; struct Light { vec3 pos; float intensity; };
```

## 8. 顶层 & shader 包装

```racket
(glsl
 (version 330 core)           ; #version 330 core
 (in vec2 aPos)
 (uniform float uTime)
 (define (main) void ...))
```

`(glsl ...)` 把各顶层形式翻译后拼成一个 GLSL 字符串，接进 `build-program`。
顶层允许：`version`、声明、`struct`、`define`、`raw`。uniform/in/out 直接写声明即可，不需要 `#:uniforms` 关键字糖。

## 9. 逃逸口

```racket
(raw "textureLod(tex, uv, lod)")            ; 内联任意 GLSL，原样输出
(raw "vec3(0.0, 1.0, 0.0)")                 ; 无参也可以
```

`raw` 可用于**顶层、语句、表达式**三个位置，内容原样输出、不经过重写。
任何上面没覆盖的 GLSL 特性（后置 ++、某些内建、扩展语法）一律走这里。

## 10. Racket 宏层：递归 / 展开（二期，编译期）

核心 DSL 之上，Racket 宏在**展开期**转成 §5 的循环或直线代码：

```racket
;; 尾递归 → for/while（编译成 §5 的循环，不生成递归 GLSL）
(named-let loop ([i 0] [acc 0.0])
  (if (< i 8)
      (loop (+ i 1) (+ acc (noise i)))
      acc))

;; 编译期有界展开 → 直线代码（n 是编译期常量，WebGL1 下唯一可行）
(unroll 5 (λ (i oct) (* oct (noise i))))
```

> 这些是**宿主 Racket 的能力**，不是 GLSL 语法；第一期先把 §1–§9 的核心 DSL 做出来，宏层之后叠加。

## 11. 完整对照（02 课）

顶点着色器：

```racket
(glsl
 (version 330 core)
 (layout (location 0) in vec2 aPos)
 (layout (location 1) in vec2 aUV)
 (out vec2 vUV)
 (define (main) void
   (set! vUV aUV)
   (set! gl_Position (vec4 aPos 0.0 1.0))))
```

片元着色器：

```racket
(glsl
 (version 330 core)
 (in vec2 vUV)
 (uniform float uTime)
 (out vec4 FragColor)
 (define (main) void
   (vec2 p (- (* vUV 2.0) 1.0))
   (float d (length p))
   (float ring (fract (- (* d 6.0) uTime)))
   (vec3 c (mix (vec3 0.10 0.15 0.40) (vec3 0.10 0.70 1.00) ring))
   (set! c (* c (- 1.0 (* 0.55 d))))
   (set! c (+ c (* (vec3 0.15) (+ 0.5 (* 0.5 (sin (* uTime 2.0)))))))
   (when (and (> (x p) 0.15) (> (y p) 0.15))
     (set! c (vec3 (+ (* (x p) 0.5) 0.5)
                   (+ (* (y p) 0.5) 0.5)
                   (+ 0.5 (* 0.4 (sin (+ uTime (* (x p) 3.0))))))))
   (set! FragColor (vec4 (clamp c 0.0 1.0) 1.0))))
```
