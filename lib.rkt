#lang racket/base
;; =========================================================
;; lib.rkt —— v2 课程共享库（只放"与窗口无关"的 GL 工具）
;;   1) build-program：把两段 GLSL 源码字符串编译链接成着色器程序
;;   2) print-gl-info：打印 GL 版本信息（验证 core profile 用）
;;   3) mat4 工具：列主序 f64vector[16]，与 GL uniform mat4 一致
;;   4) OBJ 模型解析：文本文件 → 顶点/索引数组（详见文末 obj-load-file）
;;   5) GLSL S 表达式 DSL 桥：转发 racket-glsl 的 (glsl ...)/vec2…/mat4…，
;;      让每课只 require 一个 lib.rkt 就能用 S 表达式写 shader。
;; 窗口/事件循环在每课里直接用 racket/gui 写（保持每课独立可运行）。
;; =========================================================

(require opengl opengl/util)   ; gl* 常量/函数；load-shader/create-program
(require ffi/vector)           ; f64vector/f32vector
(require racket/string racket/path)  ; string-split / file-name-from-path
(require "racket-glsl/rewrite.rkt")        ; (glsl ...) 宏、glsl-pretty
(require "racket-glsl/rename-vector.rkt")  ; vec2/3/4、ivec*/uvec*/bvec*、mat2/3/4、glsl-size…

(provide build-program build-program-stages print-gl-info
         m4-identity m4-mult m4-perspective m4-ortho m4-look-at
         m4-translate m4-scale m4-rot-x m4-rot-y m4-rot-z
         (struct-out obj-mesh) obj-load-file
         (all-from-out "racket-glsl/rewrite.rkt")
         (all-from-out "racket-glsl/rename-vector.rkt"))

(define PI (acos -1.0))

(define (print-gl-info)
  (printf "GL_VERSION   ~a~%" (glGetString GL_VERSION))
  (printf "GL_RENDERER  ~a~%" (glGetString GL_RENDERER))
  (printf "GLSL_VERSION ~a~%" (glGetString GL_SHADING_LANGUAGE_VERSION)))

;; 把两段 GLSL 源码（字符串）编译成程序；失败时 util 会打印信息日志。
;; ★必须在 with-gl-context 里调用（需要当前 GL 上下文）
(define (build-program vs-src fs-src)
  (define vs (load-shader (open-input-string vs-src) GL_VERTEX_SHADER))
  (define fs (load-shader (open-input-string fs-src) GL_FRAGMENT_SHADER))
  (create-program vs fs))

;; 通用多阶段：stages = (list (list GL_VERTEX_SHADER vs) (list GL_FRAGMENT_SHADER fs) ...)
;; compute/几何/细分都走这里；失败时 util 打印信息日志。
;; ★必须在 with-gl-context 里调用
(define (build-program-stages stages)
  (apply create-program
         (map (lambda (s) (load-shader (open-input-string (cadr s)) (car s))) stages)))

;; =========================================================
;; mat4：4x4 矩阵小工具（列主序，元素 (row r, col c) 存于下标 c*4+r）
;; =========================================================

(define (m4-identity)
  (f64vector 1.0 0.0 0.0 0.0
             0.0 1.0 0.0 0.0
             0.0 0.0 1.0 0.0
             0.0 0.0 0.0 1.0))

;; A·B（列向量约定：先作用 B，再作用 A）
(define (m4-mult A B)
  (define R (make-f64vector 16 0.0))
  (for* ([c (in-range 4)]
         [r (in-range 4)]
         [k (in-range 4)])
    (define a (f64vector-ref A (+ (* 4 k) r)))  ; A 的 [r][k]
    (define b (f64vector-ref B (+ (* 4 c) k)))  ; B 的 [k][c]
    (f64vector-set! R (+ (* 4 c) r)
                    (+ (f64vector-ref R (+ (* 4 c) r)) (* a b))))
  R)

;; 透视投影：fovy=垂直视角(度)，aspect=宽/高，near/far=近远平面(正数)
(define (m4-perspective fovy aspect near far)
  (define f (/ 1.0 (tan (* 0.5 (/ PI 180.0) fovy))))
  (define nf (/ (+ near far) (- near far)))
  (define n2f (/ (* 2.0 near far) (- near far)))
  (f64vector (/ f aspect) 0.0 0.0 0.0
             0.0 f 0.0 0.0
             0.0 0.0 nf -1.0
             0.0 0.0 n2f 0.0))

;; 正交投影：把屏幕像素矩形 [l,r]×[b,t]（左上原点、y 向下）映射到 NDC
(define (m4-ortho l r b t n f)
  (define rl (- r l)) (define tb (- t b)) (define fn (- f n))
  (f64vector (/ 2.0 rl) 0.0 0.0 0.0
             0.0 (/ 2.0 tb) 0.0 0.0
             0.0 0.0 (/ -2.0 fn) 0.0
             (- (/ (+ r l) rl)) (- (/ (+ t b) tb)) (- (/ (+ f n) fn)) 1.0))

;; 视图矩阵 lookAt：相机在 eye，看向 center，up 为上方向
;; （把世界坐标变换成"以相机为原点的视图坐标"）
(define (m4-look-at ex ey ez cx cy cz ux uy uz)
  (define fx (- cx ex)) (define fy (- cy ey)) (define fz (- cz ez))
  (define fl (sqrt (+ (* fx fx) (* fy fy) (* fz fz))))
  (define fxx (/ fx fl)) (define fyy (/ fy fl)) (define fzz (/ fz fl))
  ;; s = normalize(f × up)，u = s × f
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

(define (m4-translate x y z)
  (f64vector 1.0 0.0 0.0 0.0
             0.0 1.0 0.0 0.0
             0.0 0.0 1.0 0.0
             x y z 1.0))

(define (m4-scale sx sy sz)
  (f64vector sx 0.0 0.0 0.0
             0.0 sy 0.0 0.0
             0.0 0.0 sz 0.0
             0.0 0.0 0.0 1.0))

(define (rot a) (define r (* (/ PI 180.0) a)) (list (cos r) (sin r)))
(define (m4-rot-x a)
  (define-values (c s) (apply values (rot a)))
  (f64vector 1.0 0.0 0.0 0.0
             0.0 c s 0.0
             0.0 (- s) c 0.0
             0.0 0.0 0.0 1.0))
(define (m4-rot-y a)
  (define-values (c s) (apply values (rot a)))
  (f64vector c 0.0 (- s) 0.0
             0.0 1.0 0.0 0.0
             s 0.0 c 0.0
             0.0 0.0 0.0 1.0))
(define (m4-rot-z a)
  (define-values (c s) (apply values (rot a)))
  (f64vector c s 0.0 0.0
             (- s) c 0.0 0.0
             0.0 0.0 1.0 0.0
             0.0 0.0 0.0 1.0))

;; =========================================================
;; ④ OBJ（Wavefront）模型解析 —— 把文本模型变成 GL 顶点/索引数组
;; =========================================================
;; 17 课主题：GL 不认"文件"，只认顶点数组。OBJ 是 GitHub 上最常见的
;; 文本模型格式，逐行读即可：
;;     v  x  y  z       顶点坐标（1 起始编号）
;;     vt u  v          uv（1 起始，可没有）
;;     vn nx ny nz      法线（1 起始，可没有）
;;     f  1/2/3 4/5/6   面：每格 = 顶点/uv/法线 三个编号；可缺项（v//vn、
;;                       v/vt）；多边形按扇形三角化
;; 其余行（# 注释、mtllib、o、s、usemtl…）一律跳过 —— 一个能用的加载器
;; 重点是"宽容"：真实世界的 OBJ 什么怪样都有。
;;
;; 返回 (obj-mesh ...)（纯数据，不碰 GL）：
;;   verts = f32vector，每 8 个 float 一个顶点 [xyz | nxyz | uv]（模型已
;;           居中并等比缩放到最宽边 ~1.6，方便放进相机视野）
;;   idx   = u32vector 索引
;;   tris / verts-used / flat? / summary：统计信息
;; 关键设计两处（17 课正文讲"为什么"）：
;;   * 角点去重（索引化）："角点" = (顶点,uv,法线) 三编号；三编号相同的
;;     角点只存一份顶点、用索引引用。OBJ 的面是共享角点的，直接展开会
;;     把同一份数据重复存好几份。
;;   * uv：直接取文件里的 (u, v) 原样上传。注意：vt 的 v 方向各家建模
;;     软件/导出设置可能相反（有的 v=0 在图片底部、有的在顶部），要不要
;;     翻转是模型加载里著名的"贴图上下颠倒"坑 —— 本课先用对称纹理绕开
;;     它，方向约定的彻底研究留到以后专门一课。
;;   * 法线：文件有 vn 就用（建模软件烘焙好的平滑/平面法线）；某角点
;;     没有 vn 时现场用两条边叉积算面法线（flat），且该角点键里带上面
;;     编号 → 不与其它面共享 → 相邻面之间是硬边（光滑才需要共享）。
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
          [("v")  (set! vs  (cons (map string->number (cdr toks)) vs))]
          [("vt") (set! vts (cons (map string->number (cdr toks)) vts))]
          [("vn") (set! vns (cons (map string->number (cdr toks)) vns))]
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

  ;; ---- ③ 原始编号 → 局部坐标/uv（本课 GL 用的值）----
  (define (ploc i)                     ; 顶点编号(1起始)
    (define q (vector-ref vtab (sub1 i)))
    (list (* (- (car q) cx) sc) (* (- (cadr q) cy) sc) (* (- (caddr q) cz) sc)))
  (define (uvl i)                      ; vt 编号；#f/缺 vt → (0,0)
    (if (and i (pair? vts))
        (let ([q (vector-ref vttab (sub1 i))])
          (list (car q) (cadr q)))
        '(0.0 0.0)))
  (define (nloc i) (vector-ref vntab (sub1 i)))
  (define (cross a b)                  ; a×b
    (list (- (* (cadr a) (caddr b)) (* (caddr a) (cadr b)))
          (- (* (caddr a) (car b)) (* (car a) (caddr b)))
          (- (* (car a) (cadr b)) (* (cadr a) (car b)))))
  (define (sub a b) (list (- (car a) (car b)) (- (cadr a) (cadr b)) (- (caddr a) (caddr b))))
  (define (norm a)
    (define l (sqrt (+ (* (car a) (car a)) (* (cadr a) (cadr a)) (* (caddr a) (caddr a)))))
    (if (zero? l) '(0.0 1.0 0.0) (list (/ (car a) l) (/ (cadr a) l) (/ (caddr a) l))))

  ;; ---- ④ 面 → 角点流（扇形三角化 + 去重）----
  (define lookup (make-hash))          ; 角点键 → 顶点下标
  (define entries '())                 ; (下标 . 8-float) 暂存
  (define total 0)
  (define any-flat? #f)
  (define (ensure! key pos nrm uv)
    (define hit (hash-ref lookup key #f))
    (cond [hit hit]
          [else (set! entries (cons (cons total (list (car pos) (cadr pos) (caddr pos)
                                                      (car nrm) (cadr nrm) (caddr nrm)
                                                      (car uv) (cadr uv))) entries))
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
      (define face-n (norm (cross (sub p1 p0) (sub p2 p0))))   ; 面法线
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
  (define verts (apply f32vector (map exact->inexact (apply append ordered))))
  (define idx   (apply u32vector (reverse idxl)))
  (define tris  (quotient (length idxl) 3))
  (define summary
    (format "OBJ ~a：~a 顶点 ~a uv ~a vn → ~a 三角形；索引化后 ~a 顶点(省 ~a%)~a"
            (file-name-from-path path) (vector-length vtab) (vector-length vttab)
            (vector-length vntab) tris total
            (if (zero? (* tris 3)) 0 (round (/ (* 100.0 (- (* tris 3) total)) (* tris 3))))
            (if any-flat? "；缺 vn 的面已现场算面法线(flat)" "")))
  (obj-mesh verts idx tris total any-flat? summary))
