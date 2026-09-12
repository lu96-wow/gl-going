#lang racket/base
;; =========================================================
;; lib.rkt —— GL/GLSL 工具库（第一版）
;;
;; 本文件从 03-pipeline 复制：build-program 直接当黑盒用
;;   （编译 + 链接的通用版；03 课 06 步收进 lib，本课先用起来）。
;; 02-triangle/02-vbo.rkt 一步添加：
;;   转发 rename-vector —— 拿到 vec2/vec3/vec4 构造器和 concat-vecs
;;   （顶点数据统一用 vec2 写，不再裸写 f32vector）。
;; 后面每一课会复制本文件，并在需要时往里加功能。
;; =========================================================
;;
;; 本课（18-model）从 17-text 复制，并新增 OBJ 模型解析器：
;;   第 4 步（04-lib.rkt）把 obj-load-file / obj-mesh 收进本文件。
;;   obj-load-file 是纯 CPU 文本解析（不碰 GL），返回 verts（8 float/顶点）
;;   + idx（索引），见文件末尾。
;; =========================================================

(require opengl ffi/vector)                  ; gl* 常量；s32vector（glShaderSource 用）
(require racket/draw racket/class)            ; read-bitmap / bitmap% / send（load-tex 读图用）
(require racket/string racket/path)           ; string-split / file-name-from-path（OBJ 解析用）
(require "../../racket-glsl/rewrite.rkt")       ; (glsl ...) 宏 + glsl-pretty
(require "../../racket-glsl/rename-vector.rkt") ; vec2/vec3/vec4、concat-vecs、mat4

(provide build-program
         mat4-identity mat4-translate mat4-rot-x mat4-rot-y mat4-rot-z
         mat4-scale mat4-mult mat4-ortho mat4-perspective mat4-look-at
         cube-verts cube-idx grid-verts
         load-tex cube-uv-verts cube-uv-idx
         cube-normal-verts cube-normal-idx
         (struct-out obj-mesh) obj-load-file
         (all-from-out "../../racket-glsl/rewrite.rkt")        ; (glsl ...) 宏、glsl-pretty
         (all-from-out "../../racket-glsl/rename-vector.rkt")) ; vec2…、concat-vecs、mat4…

;; 把一段 GLSL 文本编译成一个"着色器对象"（03 课 03 步裸写讲的原始过程）
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
    (error 'compile-shader "着色器编译失败：\n~a" (shader-info-log shader)))
  shader)

;; 编译 + 链接任意阶段为一个程序（每段 = (阶段类型 源码)）：
;;   (build-program (GL_VERTEX_SHADER vs-src) (GL_FRAGMENT_SHADER fs-src))
;;   (build-program (GL_VERTEX_SHADER vs) (GL_GEOMETRY_SHADER gs) (GL_FRAGMENT_SHADER fs))
;; 阶段类型：GL_VERTEX_SHADER / GL_TESS_CONTROL_SHADER / GL_TESS_EVALUATION_SHADER
;;          / GL_GEOMETRY_SHADER / GL_FRAGMENT_SHADER。
;; （= 03 课讲的 compile-shader + link-program 收成的通用版）
(define (build-program . stages)
  (define prog (glCreateProgram))
  (for ([s stages])
    (glAttachShader prog (compile-shader (car s) (cadr s))))
  (glLinkProgram prog)
  ;; ★链接失败：打印日志并停在这里
  (when (zero? (glGetProgramiv prog GL_LINK_STATUS))
    (error 'build-program "程序链接失败：\n~a" (program-info-log prog)))
  prog)

;; =========================================================
;; mat4-*：4×4 矩阵工具（列主序 mat4 = f32vector[16]，元素 (r行,c列) 存下标 c*4+r）
;; 前 3 步裸写、第 4 步收进这里；第 5 步加 mat4-ortho。
;; 数学用 f32，与 GL 的 float 一致，上传零转换。
;; =========================================================

(define (mat4-identity)
  (mat4 1.0 0.0 0.0 0.0
             0.0 1.0 0.0 0.0
             0.0 0.0 1.0 0.0
             0.0 0.0 0.0 1.0))

(define (mat4-translate tx ty tz)
  (mat4 1.0 0.0 0.0 0.0
             0.0 1.0 0.0 0.0
             0.0 0.0 1.0 0.0
             tx  ty  tz  1.0))

;; 旋转（绕各轴，角度制）。绕 z 就是 2D 旋转；绕 x/y 是 3D 新增的。
(define (mat4-rot-z deg)
  (define r (* (/ (acos -1.0) 180.0) deg))
  (define c (cos r))
  (define s (sin r))
  (mat4 c     s     0.0 0.0
             (- s) c     0.0 0.0
             0.0   0.0   1.0 0.0
             0.0   0.0   0.0 1.0))

(define (mat4-rot-x deg)
  (define r (* (/ (acos -1.0) 180.0) deg))
  (define c (cos r))
  (define s (sin r))
  (mat4 1.0 0.0    0.0   0.0
             0.0 c     s     0.0
             0.0 (- s) c     0.0
             0.0 0.0    0.0   1.0))

(define (mat4-rot-y deg)
  (define r (* (/ (acos -1.0) 180.0) deg))
  (define c (cos r))
  (define s (sin r))
  (mat4 c    0.0 (- s) 0.0
             0.0  1.0 0.0    0.0
             s    0.0 c      0.0
             0.0  0.0 0.0    1.0))

(define (mat4-scale sx sy sz)
  (mat4 sx  0.0 0.0 0.0
             0.0 sy  0.0 0.0
             0.0 0.0 sz  0.0
             0.0 0.0 0.0 1.0))

;; A·B（先作用 B，再作用 A）
(define (mat4-mult A B)
  (define R (make-f32vector 16 0.0))
  (for* ([c (in-range 4)] [r (in-range 4)] [k (in-range 4)])
    (f32vector-set! R (+ (* 4 c) r)
                    (+ (f32vector-ref R (+ (* 4 c) r))
                       (* (f32vector-ref A (+ (* 4 k) r))
                          (f32vector-ref B (+ (* 4 c) k))))))
  R)

;; 正交投影：把 [l,r]×[b,t]（深度 [n,f]）映射到 NDC。
;; 像素世界（左上原点、y 向下）用 (mat4-ortho 0 w h 0 -1 1)。
(define (mat4-ortho l r b t n f)
  (define rl (- r l)) (define tb (- t b)) (define fn (- f n))
  (mat4 (/ 2.0 rl) 0.0 0.0 0.0
             0.0 (/ 2.0 tb) 0.0 0.0
             0.0 0.0 (/ -2.0 fn) 0.0
             (- (/ (+ r l) rl)) (- (/ (+ t b) tb)) (- (/ (+ f n) fn)) 1.0))

;; 透视投影：fovy=垂直视角(度)、aspect=宽/高、near/far=近远平面(正数)。
;; 让 w=-z，GPU 透视除法后产生"近大远小"。
(define (mat4-perspective fovy aspect near far)
  (define f (/ 1.0 (tan (* 0.5 (/ (acos -1.0) 180.0) fovy))))
  (define nf (/ (+ near far) (- near far)))
  (define n2f (/ (* 2.0 near far) (- near far)))
  (mat4 (/ f aspect) 0.0 0.0 0.0
             0.0 f 0.0 0.0
             0.0 0.0 nf -1.0
             0.0 0.0 n2f 0.0))

;; 视图矩阵 lookAt：eye=(ex,ey,ez) 看向 center=(cx,cy,cz)，up=(ux,uy,uz)。
;; 用 f（前）、s（右）、u（上）三个正交基向量 + 平移拼成"把世界搬到相机面前"的矩阵。
(define (mat4-look-at ex ey ez cx cy cz ux uy uz)
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
  (mat4 sxx uxx (- fxx) 0.0
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

;; =========================================================
;; 纹理工具（10 课）：load-tex 读图上传；cube-uv 贴图立方体
;; =========================================================

;; 读 PNG → 重排成 RGBA 字节 → glTexImage2D 上传，返回纹理对象编号。
;; wrap-mode = 'repeat / 'clamp；mipmap? = 是否生成多级缩小图。
;; ★必须在 with-gl-context 里调用。
(define (load-tex path wrap-mode mipmap?)
  (define bm (read-bitmap path))
  (define w (send bm get-width)) (define h (send bm get-height))
  (define argb (make-bytes (* w h 4)))
  (send bm get-argb-pixels 0 0 w h argb)
  (define rgba (make-bytes (* w h 4)))
  (for ([i (in-range (* w h))])
    (bytes-set! rgba (* i 4)      (bytes-ref argb (+ (* i 4) 1)))
    (bytes-set! rgba (+ (* i 4) 1) (bytes-ref argb (+ (* i 4) 2)))
    (bytes-set! rgba (+ (* i 4) 2) (bytes-ref argb (+ (* i 4) 3)))
    (bytes-set! rgba (+ (* i 4) 3) (bytes-ref argb (* i 4))))
  (define tex (u32vector-ref (glGenTextures 1) 0))
  (glBindTexture GL_TEXTURE_2D tex)
  (glPixelStorei GL_UNPACK_ALIGNMENT 1)
  (glTexImage2D GL_TEXTURE_2D 0 GL_RGBA w h 0 GL_RGBA GL_UNSIGNED_BYTE rgba)
  (define wrap (if (eq? wrap-mode 'repeat) GL_REPEAT GL_CLAMP_TO_EDGE))
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S wrap)
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T wrap)
  (if mipmap?
      (begin (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR_MIPMAP_LINEAR)
             (glGenerateMipmap GL_TEXTURE_2D))
      (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR))
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
  tex)

;; 贴图立方体：6 面 × 4 顶点（每个 = 位置 vec3 + uv vec2），36 索引。
;; 每个面把整张图 (0..1) 铺满。
(define cube-uv-verts
  (let ([pos8 (list (vec3 -1.0 -1.0  1.0) (vec3  1.0 -1.0  1.0) (vec3  1.0  1.0  1.0) (vec3 -1.0  1.0  1.0)
                    (vec3 -1.0 -1.0 -1.0) (vec3  1.0 -1.0 -1.0) (vec3  1.0  1.0 -1.0) (vec3 -1.0  1.0 -1.0))]
        [faces '((0 1 2 3) (5 4 7 6) (1 5 6 2) (4 0 3 7) (3 2 6 7) (4 5 1 0))]
        [uv4 (list (vec2 0.0 0.0) (vec2 1.0 0.0) (vec2 1.0 1.0) (vec2 0.0 1.0))])
    (apply concat-vecs
           (apply append
                  (for/list ([f faces])
                    (apply append
                           (for/list ([j (in-range 4)])
                             (list (list-ref pos8 (list-ref f j))
                                   (list-ref uv4 j)))))))))

(define cube-uv-idx
  (apply u16vector
         (apply append
                (for/list ([i (in-range 6)])
                  (define b (* i 4))
                  (list b (+ b 1) (+ b 2)  b  (+ b 2) (+ b 3))))))

;; =========================================================
;; 带法线的立方体（11 课）：6 面 × 4 顶点（每个 = 位置 vec3 + 法线 vec3）。
;; 法线逐面朝外（+z/-z/+x/-x/+y/-y），所以面要拆开存（法线不同不能共享角）。
;; =========================================================
(define cube-normal-verts
  (let ([pos8 (list (vec3 -1.0 -1.0  1.0) (vec3  1.0 -1.0  1.0) (vec3  1.0  1.0  1.0) (vec3 -1.0  1.0  1.0)
                    (vec3 -1.0 -1.0 -1.0) (vec3  1.0 -1.0 -1.0) (vec3  1.0  1.0 -1.0) (vec3 -1.0  1.0 -1.0))]
        [faces (list (list (vec3  0.0  0.0  1.0) '(0 1 2 3))
                     (list (vec3  0.0  0.0 -1.0) '(5 4 7 6))
                     (list (vec3  1.0  0.0  0.0) '(1 5 6 2))
                     (list (vec3 -1.0  0.0  0.0) '(4 0 3 7))
                     (list (vec3  0.0  1.0  0.0) '(3 2 6 7))
                     (list (vec3  0.0 -1.0  0.0) '(4 5 1 0)))])
    (apply concat-vecs
           (apply append
                  (map (lambda (f)
                         (apply append
                                (for/list ([i (cadr f)])
                                  (list (list-ref pos8 i) (car f)))))
                       faces)))))

(define cube-normal-idx
  (apply u16vector
         (apply append
                (for/list ([i (in-range 6)])
                  (define b (* i 4))
                  (list b (+ b 1) (+ b 2)  b  (+ b 2) (+ b 3))))))

;; =========================================================
;; vec3 数学：GLSL 的 sub / dot / cross / normalize 的 CPU 版。
;; 作用在 vec3（f32vector）上，返回 vec3 或数。
;; =========================================================
(define (vec3-sub a b)
  (vec3 (- (f32vector-ref a 0) (f32vector-ref b 0))
        (- (f32vector-ref a 1) (f32vector-ref b 1))
        (- (f32vector-ref a 2) (f32vector-ref b 2))))

(define (vec3-dot a b)
  (+ (* (f32vector-ref a 0) (f32vector-ref b 0))
     (* (f32vector-ref a 1) (f32vector-ref b 1))
     (* (f32vector-ref a 2) (f32vector-ref b 2))))

(define (vec3-cross a b)
  (vec3 (- (* (f32vector-ref a 1) (f32vector-ref b 2))
           (* (f32vector-ref a 2) (f32vector-ref b 1)))
        (- (* (f32vector-ref a 2) (f32vector-ref b 0))
           (* (f32vector-ref a 0) (f32vector-ref b 2)))
        (- (* (f32vector-ref a 0) (f32vector-ref b 1))
           (* (f32vector-ref a 1) (f32vector-ref b 0)))))

(define (vec3-length a) (sqrt (vec3-dot a a)))

(define (vec3-normalize a)
  (define l (vec3-length a))
  (if (zero? l)
      (vec3 0.0 1.0 0.0)
      (vec3 (/ (f32vector-ref a 0) l)
            (/ (f32vector-ref a 1) l)
            (/ (f32vector-ref a 2) l))))

;; =========================================================
;; OBJ（Wavefront）模型解析 —— 文本模型文件 → 顶点/索引数组
;; =========================================================
;; 18 课主题：GL 不认"文件"，只认顶点数组。OBJ 是最常见的文本模型格式，
;; 逐行读即可：
;;     v  x  y  z       顶点坐标（1 起始编号）
;;     vt u  v          uv（1 起始，可没有）
;;     vn nx ny nz      法线（1 起始，可没有）
;;     f  1/2/3 4/5/6   面：每格 = 顶点/uv/法线 三个编号；可缺项（v//vn、
;;                       v/vt）；多边形按扇形三角化
;; 其余行（# 注释、mtllib、o、s、usemtl…）一律跳过。
;;
;; 返回 (obj-mesh ...)（纯数据，不碰 GL）：
;;   verts = f32vector，每 8 个 float 一个顶点 [xyz | nxyz | uv]
;;           （模型已居中并等比缩放到最宽边 ~1.6，方便放进相机视野）
;;   idx   = u32vector 索引
;; 关键设计两处（18 课正文讲"为什么"）：
;;   * 角点去重（索引化）："角点" = (顶点,uv,法线) 三编号；三编号相同的
;;     角点只存一份顶点、用索引引用，避免重复存。
;;   * 法线：文件有 vn 就用（建模软件烘焙好的平滑/平面法线）；某角点没有
;;     vn 时现场用两条边叉积算面法线（flat），且该角点不与其它的共享 →
;;     相邻面之间是硬边。
;; =========================================================
(struct obj-mesh (verts idx tris verts-used flat? summary) #:transparent)

(define (obj-load-file path)
  ;; ---- ① 逐行读：只收 v/vt/vn/f 四类，其余行丢弃 ----
  (define ip (open-input-file path))
  (define vs '()) (define vts '()) (define vns '()) (define fs '())
  (let loop ([l (read-line ip 'any)])
    (unless (eof-object? l)
      (define toks (string-split l))
      (when (pair? toks)
        (case (car toks)
          [("v")  (set! vs  (cons (map (lambda (x) (exact->inexact (string->number x))) (cdr toks)) vs))]
          [("vt") (set! vts (cons (map (lambda (x) (exact->inexact (string->number x))) (cdr toks)) vts))]
          [("vn") (set! vns (cons (map (lambda (x) (exact->inexact (string->number x))) (cdr toks)) vns))]
          [("f")  (set! fs  (cons (cdr toks) fs))]
          [else #f]))
      (loop (read-line ip 'any))))
  (close-input-port ip)
  (define vtab  (list->vector (reverse vs)))    ; 均 1 起始 → 取用时要 sub1
  (define vttab (list->vector (reverse vts)))
  (define vntab (list->vector (reverse vns)))

  ;; ---- ② 平移居中 + 等比缩放：包围盒最宽边缩到 1.6 单位 ----
  (define bb
    (for/fold ([b (list +inf.0 -inf.0 +inf.0 -inf.0 +inf.0 -inf.0)])
              ([q (in-vector vtab)])
      (list (min (list-ref b 0) (car q))   (max (list-ref b 1) (car q))
            (min (list-ref b 2) (cadr q))  (max (list-ref b 3) (cadr q))
            (min (list-ref b 4) (caddr q)) (max (list-ref b 5) (caddr q)))))
  (define cx (/ (+ (list-ref bb 0) (list-ref bb 1)) 2.0))
  (define cy (/ (+ (list-ref bb 2) (list-ref bb 3)) 2.0))
  (define cz (/ (+ (list-ref bb 4) (list-ref bb 5)) 2.0))
  (define wd (- (list-ref bb 1) (list-ref bb 0)))
  (define ht (- (list-ref bb 3) (list-ref bb 2)))
  (define dp (- (list-ref bb 5) (list-ref bb 4)))
  (define maxdim (max wd ht dp))
  (define sc (if (zero? maxdim) 1.0 (/ 1.6 maxdim)))

  ;; ---- ③ 编号 → 局部坐标/uv（本课 GL 用的值）----
  (define (ploc i)                     ; 顶点编号(1起始) → vec3
    (define q (vector-ref vtab (sub1 i)))
    (vec3 (* (- (car q) cx) sc) (* (- (cadr q) cy) sc) (* (- (caddr q) cz) sc)))
  (define (uvl i)                      ; vt 编号 → vec2；#f/缺 vt → (0,0)
    (if (and i (pair? vts))
        (let ([q (vector-ref vttab (sub1 i))])
          (vec2 (car q) (cadr q)))
        (vec2 0.0 0.0)))
  (define (nloc i)                     ; vn 编号 → vec3
    (define q (vector-ref vntab (sub1 i)))
    (vec3 (car q) (cadr q) (caddr q)))

  ;; ---- ④ 面 → 角点流（扇形三角化 + 去重）----
  (define lookup (make-hash))          ; 角点键 → 顶点下标
  (define entries '())                 ; (下标 . (pos nrm uv)) 暂存
  (define total 0)
  (define any-flat? #f)
  (define (ensure! key pos nrm uv)
    (define hit (hash-ref lookup key #f))
    (cond [hit hit]
          [else (set! entries (cons (cons total (list pos nrm uv)) entries))
                (hash-set! lookup key total)
                (set! total (add1 total))
                (sub1 total)]))
  (define idxl '())
  (define fidx 0)
  (for ([raw (reverse fs)])
    ;; 角点串 "v[/vt][/vn]" 或 "v//vn" → (v vt vn)，缺的给 #f
    (define (parse t)
      (define parts (string-split t "/"))
      (list (string->number (car parts))
            (and (>= (length parts) 2) (not (string=? "" (list-ref parts 1)))
                 (string->number (list-ref parts 1)))
            (and (>= (length parts) 3) (not (string=? "" (list-ref parts 2)))
                 (string->number (list-ref parts 2)))))
    (define corners (map parse raw))
    (define n (length corners))
    (when (>= n 3)
      (define p0 (ploc (car (list-ref corners 0))))
      (define p1 (ploc (car (list-ref corners 1))))
      (define p2 (ploc (car (list-ref corners 2))))
      (define face-n (vec3-normalize (vec3-cross (vec3-sub p1 p0) (vec3-sub p2 p0))))   ; 面法线
      (define (emit c)
        (define vn-idx (caddr c))
        (define key (if vn-idx
                        (list (car c) (cadr c) vn-idx)
                        (begin (set! any-flat? #t)
                               (list 'flat fidx (car c) (cadr c)))))
        (define id (ensure! key (ploc (car c))
                            (if vn-idx (nloc vn-idx) face-n)
                            (uvl (cadr c))))
        (set! idxl (cons id idxl)))
      (emit (list-ref corners 0))                        ; 扇形三角化
      (for ([j (in-range 1 (- n 1))])
        (emit (list-ref corners j))
        (emit (list-ref corners (add1 j))))
      (set! fidx (add1 fidx))))
  ;; ---- ⑤ 定稿：按下标顺序铺成 f32vector + u32vector ----
  (define placed (make-vector total #f))
  (for ([e entries]) (vector-set! placed (car e) (cdr e)))
  (define ordered (for/list ([id (in-range total)]) (vector-ref placed id)))
  (define verts (apply concat-vecs (apply append ordered)))
  (define idx   (apply u32vector (reverse idxl)))
  (define tris  (quotient (length idxl) 3))
  (define summary
    (format "OBJ ~a：~a 顶点 ~a uv ~a vn → ~a 三角形；索引化后 ~a 顶点(省 ~a%)~a"
            (file-name-from-path path) (vector-length vtab) (vector-length vttab)
            (vector-length vntab) tris total
            (if (zero? (* tris 3)) 0 (round (/ (* 100.0 (- (* tris 3) total)) (* tris 3))))
            (if any-flat? "；缺 vn 的面已现场算面法线(flat)" "")))
  (obj-mesh verts idx tris total any-flat? summary))
