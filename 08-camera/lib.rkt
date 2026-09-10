#lang racket/base
;; =========================================================
;; lib.rkt —— GL/GLSL 工具库（第一版）
;;
;; 本文件在 02-triangle/03-build-program.rkt 一步创建：
;;   把 02-shader-load.rkt 裸写的 7 个 gl* 调用收成 build-program。
;; 02-triangle/04-vbo.rkt 一步添加：
;;   转发 rename-vector —— 拿到 vec2/vec3/vec4 构造器和 concat-vecs
;;   （顶点数据统一用 vec2 写，不再裸写 f32vector）。
;; 后面每一课会复制本文件，并在需要时往里加功能。
;; =========================================================
;;
;; 本课（08-camera）从 07-3d-depth 复制，并新增相机工具：
;;   第 1 步（01-lookat.rkt）裸写 m4-look-at 和网格地面 grid-verts，
;;   第 4 步（04-lib.rkt）收进本文件。矩阵约定不变。
;; =========================================================

(require opengl ffi/vector)                  ; gl* 常量；s32vector（glShaderSource 用）
(require "../racket-glsl/rewrite.rkt")       ; (glsl ...) 宏 + glsl-pretty
(require "../racket-glsl/rename-vector.rkt") ; vec2/vec3/vec4、concat-vecs、mat4

(provide build-program
         m4-identity m4-translate m4-rot-x m4-rot-y m4-rot-z
         m4-scale m4-mult m4-ortho m4-perspective m4-look-at
         cube-verts cube-idx grid-verts
         (all-from-out "../racket-glsl/rewrite.rkt")        ; (glsl ...) 宏、glsl-pretty
         (all-from-out "../racket-glsl/rename-vector.rkt")) ; vec2…、concat-vecs、mat4…

;; 把一段 GLSL 文本编译成一个"着色器对象"（02-shader-load.rkt 讲的原始过程）
;; 取着色器信息日志（编译失败时打印，帮读者定位错误行）
(define (shader-info-log shader)
  (define len (glGetShaderiv shader GL_INFO_LOG_LENGTH))
  (define-values (actual log) (glGetShaderInfoLog shader len))
  (bytes->string/utf-8 log #\? 0 actual))

;; 取程序信息日志（链接失败时打印）
(define (program-info-log prog)
  (define len (glGetProgramiv prog GL_INFO_LOG_LENGTH))
  (define-values (actual log) (glGetProgramInfoLog prog len))
  (bytes->string/utf-8 log #\? 0 actual))

(define (compile-shader type src)
  (define shader (glCreateShader type))
  (glShaderSource shader 1 (vector src) (s32vector (string-length src)))
  (glCompileShader shader)
  ;; ★编译失败：打印 GLSL 报错日志并停在这里（否则只会黑屏、毫无提示）
  (when (zero? (glGetShaderiv shader GL_COMPILE_STATUS))
    (error 'build-program "着色器编译失败：\n~a" (shader-info-log shader)))
  shader)

;; 编译 + 链接两段着色器，返回程序编号
;; （= 02-shader-load.rkt 的 compile-shader + link-program 收成的函数）
(define (build-program vs-src fs-src)
  (define vs (compile-shader GL_VERTEX_SHADER vs-src))
  (define fs (compile-shader GL_FRAGMENT_SHADER fs-src))
  (define prog (glCreateProgram))
  (glAttachShader prog vs)
  (glAttachShader prog fs)
  (glLinkProgram prog)
  ;; ★链接失败：打印日志并停在这里
  (when (zero? (glGetProgramiv prog GL_LINK_STATUS))
    (error 'build-program "程序链接失败：\n~a" (program-info-log prog)))
  prog)

;; =========================================================
;; m4-*：4×4 矩阵工具（列主序 f64vector[16]，元素 (r行,c列) 存下标 c*4+r）
;; 前 3 步裸写、第 4 步收进这里；第 5 步加 m4-ortho。
;; 数学用 f64 保证精度，上传前用 rename-vector 的 (mat4 ...) 转 f32。
;; =========================================================

(define (m4-identity)
  (f64vector 1.0 0.0 0.0 0.0
             0.0 1.0 0.0 0.0
             0.0 0.0 1.0 0.0
             0.0 0.0 0.0 1.0))

(define (m4-translate tx ty tz)
  (f64vector 1.0 0.0 0.0 0.0
             0.0 1.0 0.0 0.0
             0.0 0.0 1.0 0.0
             tx  ty  tz  1.0))

;; 旋转（绕各轴，角度制）。绕 z 就是 2D 旋转；绕 x/y 是 3D 新增的。
(define (m4-rot-z deg)
  (define r (* (/ (acos -1.0) 180.0) deg))
  (define c (cos r))
  (define s (sin r))
  (f64vector c     s     0.0 0.0
             (- s) c     0.0 0.0
             0.0   0.0   1.0 0.0
             0.0   0.0   0.0 1.0))

(define (m4-rot-x deg)
  (define r (* (/ (acos -1.0) 180.0) deg))
  (define c (cos r))
  (define s (sin r))
  (f64vector 1.0 0.0    0.0   0.0
             0.0 c     s     0.0
             0.0 (- s) c     0.0
             0.0 0.0    0.0   1.0))

(define (m4-rot-y deg)
  (define r (* (/ (acos -1.0) 180.0) deg))
  (define c (cos r))
  (define s (sin r))
  (f64vector c    0.0 (- s) 0.0
             0.0  1.0 0.0    0.0
             s    0.0 c      0.0
             0.0  0.0 0.0    1.0))

(define (m4-scale sx sy sz)
  (f64vector sx  0.0 0.0 0.0
             0.0 sy  0.0 0.0
             0.0 0.0 sz  0.0
             0.0 0.0 0.0 1.0))

;; A·B（先作用 B，再作用 A）
(define (m4-mult A B)
  (define R (make-f64vector 16 0.0))
  (for* ([c (in-range 4)] [r (in-range 4)] [k (in-range 4)])
    (f64vector-set! R (+ (* 4 c) r)
                    (+ (f64vector-ref R (+ (* 4 c) r))
                       (* (f64vector-ref A (+ (* 4 k) r))
                          (f64vector-ref B (+ (* 4 c) k))))))
  R)

;; 正交投影：把 [l,r]×[b,t]（深度 [n,f]）映射到 NDC。
;; 像素世界（左上原点、y 向下）用 (m4-ortho 0 w h 0 -1 1)。
(define (m4-ortho l r b t n f)
  (define rl (- r l)) (define tb (- t b)) (define fn (- f n))
  (f64vector (/ 2.0 rl) 0.0 0.0 0.0
             0.0 (/ 2.0 tb) 0.0 0.0
             0.0 0.0 (/ -2.0 fn) 0.0
             (- (/ (+ r l) rl)) (- (/ (+ t b) tb)) (- (/ (+ f n) fn)) 1.0))

;; 透视投影：fovy=垂直视角(度)、aspect=宽/高、near/far=近远平面(正数)。
;; 让 w=-z，GPU 透视除法后产生"近大远小"。
(define (m4-perspective fovy aspect near far)
  (define f (/ 1.0 (tan (* 0.5 (/ (acos -1.0) 180.0) fovy))))
  (define nf (/ (+ near far) (- near far)))
  (define n2f (/ (* 2.0 near far) (- near far)))
  (f64vector (/ f aspect) 0.0 0.0 0.0
             0.0 f 0.0 0.0
             0.0 0.0 nf -1.0
             0.0 0.0 n2f 0.0))

;; 视图矩阵 lookAt：eye=(ex,ey,ez) 看向 center=(cx,cy,cz)，up=(ux,uy,uz)。
;; 用 f（前）、s（右）、u（上）三个正交基向量 + 平移拼成"把世界搬到相机面前"的矩阵。
(define (m4-look-at ex ey ez cx cy cz ux uy uz)
  (define fx (- cx ex)) (define fy (- cy ey)) (define fz (- cz ez))
  (define fl (sqrt (+ (* fx fx) (* fy fy) (* fz fz))))
  (define fxx (/ fx fl)) (define fyy (/ fy fl)) (define fzz (/ fz fl))
  (define sx (- (* fyy uz) (* fzz uy)))
  (define sy (- (* fzz ux) (* fxx uz)))
  (define sz (- (* fxx uy) (* fyy ux)))
  (define sl (sqrt (+ (* sx sx) (* sy sy) (* sz sz))))
  (define sxx (/ sx sl)) (define syy (/ sy sl)) (define szz (/ sz sl))
  (define uxx (- (* syy fzz) (* szz fyy)))
  (define uyy (- (* szz fxx) (* sxx fzz)))
  (define uzz (- (* sxx fyy) (* syy fxx)))
  (f64vector sxx uxx (- fxx) 0.0
             syy uyy (- fyy) 0.0
             szz uzz (- fzz) 0.0
             (- (+ (* sxx ex) (* syy ey) (* szz ez)))
             (- (+ (* uxx ex) (* uyy ey) (* uzz ez)))
             (+ (* fxx ex) (* fyy ey) (* fzz ez))
             1.0))

;; 网格地面：XZ 平面（y=0）上 GL_LINES 顶点（位置 vec3 + 颜色 vec3），
;; 从 -span 到 span 每 step 一条。顶点数 = (线数) × 2。
(define (grid-verts span step)
  (define color (vec3 0.30 0.32 0.50))
  (apply concat-vecs
         (apply append
                (for/list ([s (in-range (- span) (+ span step) step)])
                  (list (vec3 (- span) 0.0 s) color
                        (vec3 span 0.0 s) color
                        (vec3 s 0.0 (- span)) color
                        (vec3 s 0.0 span) color)))))

;; =========================================================
;; 立方体网格：6 面 × 4 顶点（每个 = 位置 vec3 + 颜色 vec3），36 索引。
;; 02 步裸写过构造过程，第 4 步（04-lib.rkt）收进这里供后面复用。
;; =========================================================
(define cube-verts
  (let ([pos8 (list (vec3 -1.0 -1.0  1.0) (vec3  1.0 -1.0  1.0) (vec3  1.0  1.0  1.0) (vec3 -1.0  1.0  1.0)
                    (vec3 -1.0 -1.0 -1.0) (vec3  1.0 -1.0 -1.0) (vec3  1.0  1.0 -1.0) (vec3 -1.0  1.0 -1.0))]
        [faces (list (list (vec3 0.85 0.20 0.20) '(0 1 2 3))
                     (list (vec3 0.20 0.80 0.25) '(5 4 7 6))
                     (list (vec3 0.95 0.60 0.10) '(1 5 6 2))
                     (list (vec3 0.95 0.85 0.15) '(4 0 3 7))
                     (list (vec3 0.20 0.60 0.95) '(3 2 6 7))
                     (list (vec3 0.75 0.30 0.90) '(4 5 1 0)))])
    (apply concat-vecs
           (apply append
                  (map (lambda (f)
                         (apply append
                                (for/list ([i (cadr f)])
                                  (list (list-ref pos8 i) (car f)))))
                       faces)))))

(define cube-idx
  (apply u16vector
         (apply append
                (for/list ([i (in-range 6)])
                  (define b (* i 4))
                  (list b (+ b 1) (+ b 2)  b  (+ b 2) (+ b 3))))))
