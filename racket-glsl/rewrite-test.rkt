#lang racket/base
;; ============================================================
;; 重写层 (glsl ...) 测试
;; 运行：racket racket-glsl/rewrite-test.rkt
;; ============================================================
(require rackunit
         "rewrite.rkt")

;; (glsl ...) 返回 glsl-program；这里比较它的美化 src 与「原始串的美化」。
;; 原始串是重写层的真值，glsl-pretty 是确定性函数 → 等价于测重写层输出。
(define-syntax-rule (check-glsl raw (form ...))
  (check-equal? (glsl-program-src (glsl form ...))
                (glsl-pretty raw)))

;; ---------- 顶点着色器（02 课，表面语法） ----------
(define vert
  (glsl
   (version 330 core)
   (layout (location 0) in vec2 aPos)
   (layout (location 1) in vec2 aUV)
   (out vec2 vUV)
   (define (main) void
     (set! vUV aUV)
     (set! gl_Position (vec4 aPos 0.0 1.0)))))

(check-equal?
 (glsl-program-src vert)
 "#version 330 core\nlayout(location = 0) in vec2 aPos;\nlayout(location = 1) in vec2 aUV;\nout vec2 vUV;\nvoid main() {\n  vUV = aUV;\n  gl_Position = vec4(aPos, 0.0, 1.0);\n}")

;; ---------- 片元着色器（02 课，表面语法） ----------
(define frag
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
     (*= c (- 1.0 (* 0.55 d)))
     (+= c (* (vec3 0.15) (+ 0.5 (* 0.5 (sin (* uTime 2.0))))))
     (when (and (> (x p) 0.15) (> (y p) 0.15))
       (set! c (vec3 (+ (* (x p) 0.5) 0.5)
                     (+ (* (y p) 0.5) 0.5)
                     (+ 0.5 (* 0.4 (sin (+ uTime (* (x p) 3.0))))))))
     (set! FragColor (vec4 (clamp c 0.0 1.0) 1.0)))))

(displayln "===== 02 片元（表面语法 → 美化）=====")
(displayln (glsl-program-src frag))
(newline)

;; ---------- 语法覆盖断言（逐个特性） ----------
(check-glsl "#version 330 core\n uniform vec4 uMVP;"
            ((version 330 core) (uniform vec4 uMVP)))
(check-glsl "struct Light { vec3 pos; float i; };"
            ((struct Light (vec3 pos) (float i))))
(check-glsl "float sq(float x) { return (x * x); }"
            ((define (sq (float x)) float (* x x))))
;; 函数体最后一个表达式有值 → 作为 return（重写层此能力待加，先手写 return）
(check-glsl "void f(inout vec3 p) { return (p.x + 1.0); }"
            ((define (f (inout vec3 p)) void
                      (return (+ (x p) 1.0)))))
(check-glsl "void main() { for (int i = 0; (i < 8); ++i) { x = (x + i); } }"
            ((define (main) void
                      (for (int i 0) (< i 8) (++ i)
                        (set! x (+ x i))))))
(check-glsl "void main() { if ((a > b)) { c = a; } else { c = b; } }"
            ((define (main) void
                      (cond [(> a b) (set! c a)]
                            [else (set! c b)]))))
(check-glsl "void main() { while ((i < n)) { ++i; } }"
            ((define (main) void
                      (while (< i n) (++ i)))))
(check-glsl "void main() { if ((!done)) { break; } }"
            ((define (main) void
                      (unless done (break)))))
;; switch
(check-glsl "void main() { switch (mode) { case 1: { c = a; } case 2: { c = b; } default: { c = z; } } }"
            ((define (main) void
                      (switch mode
                        [case 1 (set! c a)]
                        [case 2 (set! c b)]
                        [default (set! c z)]))))
;; layout 多 item
(check-glsl "layout(location = 0, binding = 1) uniform sampler2D uTex;"
            ((layout (location 0) (binding 1) uniform sampler2D uTex)))
;; raw 逃逸（顶层 + 语句）
(check-glsl "#define FOO 1"
            ((raw "#define FOO 1")))
(check-glsl "void main() { foo(); }"
            ((define (main) void (raw "foo();"))))

;; ---------- 数组类型（(array 内层 [大小])）----------
(check-glsl "#version 330 core\n float[4] weights;"
            ((version 330 core) ((array float 4) weights)))
(check-glsl "vec3[] v;"
            (((array vec3) v)))
(check-glsl "float[4][3] m;"
            (((array (array float 4) 3) m)))
(check-glsl "in vec3[4] aPos;"
            ((in (array vec3 4) aPos)))
(check-glsl "layout(location = 0) in vec2[4] data;"
            ((layout (location 0) in (array vec2 4) data)))
(check-glsl "struct S { float[4] weights; };"
            ((struct S ((array float 4) weights))))
;; 数组构造器（类型位当构造器头）
(check-glsl "float[4] f() { return float[4](1.0, 2.0, 3.0, 4.0); }"
            ((define (f) (array float 4) ((array float 4) 1.0 2.0 3.0 4.0))))

;; ---------- raw 表达式位（★必须在 swizzle 判定之前）----------
(check-glsl "void main() { float x = 1.0 + 2.0; }"
            ((define (main) void (float x (raw "1.0 + 2.0")))))

;; ---------- 独立 layout（compute/几何/early_fragment_tests，无类型）----------
(check-glsl "#version 430 core\n layout(local_size_x = 8, local_size_y = 8) in;"
            ((version 430 core) (layout (local_size_x 8) (local_size_y 8) in)))
(check-glsl "layout(points) in;"
            ((layout (points) in)))
(check-glsl "layout(triangle_strip, max_vertices = 3) out;"
            ((layout (triangle_strip) (max_vertices 3) out)))

;; ---------- cond → else if 链（不套多余的 {}）----------
(check-glsl "void main() { if ((a == 1)) { c = a; } else if ((a == 2)) { c = b; } else { c = z; } }"
            ((define (main) void
                      (cond [(= a 1) (set! c a)]
                            [(= a 2) (set! c b)]
                            [else (set! c z)]))))

;; ---------- 通用声明限定符（flat/patch/shared/readonly...）----------
(check-glsl "flat in vec3 v;"
            ((flat in vec3 v)))
(check-glsl "patch in vec3 p; patch out vec3 q;"
            ((patch in vec3 p) (patch out vec3 q)))
(check-glsl "shared vec4 acc;"
            ((shared vec4 acc)))
(check-glsl "layout(rgba32f) readonly uniform image2D img;"
            ((layout (rgba32f) readonly uniform image2D img)))

;; ---------- 接口块（UBO/SSBO/in-out block）----------
(check-glsl "layout(std140, binding = 0) uniform Camera { mat4 view; mat4 proj; } cam;"
            ((layout (std140) (binding 0) uniform (block Camera (mat4 view) (mat4 proj)) cam)))
(check-glsl "layout(std430, binding = 1) buffer Particles { vec4[] position; } particles;"
            ((layout (std430) (binding 1) buffer (block Particles ((array vec4) position)) particles)))
(check-glsl "out VertexData { vec3 normal; vec2 uv; } vs_out;"
            ((out (block VertexData (vec3 normal) (vec2 uv)) vs_out)))
;; 成员访问（.field + aref 组合）
(check-glsl "void main() { cam.view = mat4(1.0); }"
            ((define (main) void (set! (.view cam) (mat4 1.0)))))
(check-glsl "void main() { particles.position[i] = vec4(1.0); }"
            ((define (main) void (set! (aref (.position particles) i) (vec4 1.0)))))

;; ---------- 字段级 layout / 限定符 ----------
(check-glsl "layout(std140) uniform Camera { layout(offset = 0) mat4 view; layout(offset = 64) mat4 proj; } cam;"
            ((layout (std140) uniform (block Camera
                                            (layout (offset 0) mat4 view)
                                            (layout (offset 64) mat4 proj)) cam)))
(check-glsl "out V { flat vec3 normal; vec2 uv; } vs_out;"
            ((out (block V (flat vec3 normal) (vec2 uv)) vs_out)))

;; ---------- double 精度类型（dvec/dmat/double）----------
(check-glsl "#version 400 core\n in dvec3 aPos;"
            ((version 400 core) (in dvec3 aPos)))
(check-glsl "uniform double uTime;"
            ((uniform double uTime)))
(check-glsl "void main() { dvec3 p = dvec3(vUV, 0.5); }"
            ((define (main) void (dvec3 p (dvec3 vUV 0.5)))))
(check-glsl "void main() { dmat4 m = dmat4(1.0); }"
            ((define (main) void (dmat4 m (dmat4 1.0)))))
;; double 字面量：raw 逃生舱（GLSL 里 1.5 是 float，1.5lf 才是 double）
(check-glsl "void main() { double x = 1.5lf; }"
            ((define (main) void (double x (raw "1.5lf")))))
;; double ↔ float 显式转换（GLSL 不隐式转）
(check-glsl "void main() { float f = float(d); }"
            ((define (main) void (float f (float d)))))
(check-glsl "void main() { double d = double(f); }"
            ((define (main) void (double d (double f)))))
