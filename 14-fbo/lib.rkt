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
;; 本课（14-fbo）从 13-instancing 原样复制，未改动：
;;   FBO 的 glGenFramebuffers/glFramebufferTexture2D/glRenderbufferStorage 等
;;   都是标准 gl* 调用，内联在步骤文件里；场景复用彩色立方体 cube-verts。
;; =========================================================

(require opengl ffi/vector)                  ; gl* 常量；s32vector（glShaderSource 用）
(require racket/draw racket/class)            ; read-bitmap / bitmap% / send（load-tex 读图用）
(require "../racket-glsl/rewrite.rkt")       ; (glsl ...) 宏 + glsl-pretty
(require "../racket-glsl/rename-vector.rkt") ; vec2/vec3/vec4、concat-vecs、mat4

(provide build-program
         mat4-identity mat4-translate mat4-rot-x mat4-rot-y mat4-rot-z
         mat4-scale mat4-mult mat4-ortho mat4-perspective mat4-look-at
         cube-verts cube-idx grid-verts
         load-tex cube-uv-verts cube-uv-idx
         cube-normal-verts cube-normal-idx
         (all-from-out "../racket-glsl/rewrite.rkt")        ; (glsl ...) 宏、glsl-pretty
         (all-from-out "../racket-glsl/rename-vector.rkt")) ; vec2…、concat-vecs、mat4…

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
