#lang racket/base
;; =========================================================
;; 17-model/03-normal.rkt —— 第三步：法线 + 归一化，凑齐完整 obj-mesh
;; 运行：racket 17-model/03-normal.rkt
;; =========================================================
;; 前两步：读出了四类行，并搞懂了索引化去重。本步补上最后两块拼图，
;; 得到**完整的 obj-mesh**（纯数据，还不碰 GL）：
;;
;; 本步新增（2 个，同属"数据整理"这一件事）：
;;   法线 —— 文件有 vn 直接用（建模软件烘焙好的）；缺 vn 现场叉积算面法线
;;   包围盒归一化 —— 居中 + 等比缩放，让任意大小的模型都能放进相机视野
;;
;; ★法线的两种来源（这是模型加载里最重要的细节）：
;;   - 文件带了 vn：那是建模软件烘焙好的法线。cube.obj 的 vn 是**逐面**法线
;;     （同一个角在不同面上法线不同，所以硬边棱角）；suzanne.obj 的 vn 是
;;     **平滑**法线（相邻面共享，所以圆润）。
;;   - 文件缺 vn：本课现场用两条边叉积算出面法线（flat）。并且让这个角点
;;     不与其它的共享 → 相邻面之间是硬边（光滑才需要共享法线）。
;;
;; ★包围盒归一化：模型作者用厘米、米还是英寸写坐标，你事先不知道。
;;   办法：算所有顶点的包围盒，平移到原点、再等比缩放到最宽边 ~1.6，
;;   放进默认相机的视野里。位置信息丢了（本来也不需要），形状保留。
;;
;; 本步输出（暂用 list 装四个字段，04 步收进 lib 时才定义成 obj-mesh 结构）：
;;   (list verts idx summary any-flat?)
;;     verts = f32vector，每 8 个 float 一个顶点 [x y z | nx ny nz | u v]
;;     idx   = u32vector 索引
;;     summary / any-flat? = 统计与“是否含缺 vn 的面”
;; 打印两个模型的摘要，看 flat/平滑两种法线的判断结果。
;; =========================================================

(require racket/base)
(require racket/string racket/list racket/path)
(require ffi/vector)                       ; f32vector / u32vector
(require racket/runtime-path)

(define-runtime-path cube-obj "assets/cube.obj")
(define-runtime-path suz-obj  "assets/suzanne.obj")

;; 一个顶点 = 8 个 float：位置(3) + 法线(3) + uv(2)
(define (obj-load-file path)
  ;; ---- ① 逐行读（01 步）----
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
  (define vtab  (list->vector (reverse vs)))   ; 编号 1 起始 → 取用 sub1
  (define vttab (list->vector (reverse vts)))
  (define vntab (list->vector (reverse vns)))

  ;; ---- ② 包围盒居中 + 等比缩放 ----
  (define bb
    (for/fold ([b (list +inf.0 -inf.0 +inf.0 -inf.0 +inf.0 -inf.0)])
              ([q (in-vector vtab)])
      (list (min (list-ref b 0) (car q))   (max (list-ref b 1) (car q))
            (min (list-ref b 2) (cadr q))  (max (list-ref b 3) (cadr q))
            (min (list-ref b 4) (caddr q)) (max (list-ref b 5) (caddr q)))))
  (define cx (/ (+ (list-ref bb 0) (list-ref bb 1)) 2.0))
  (define cy (/ (+ (list-ref bb 2) (list-ref bb 3)) 2.0))
  (define cz (/ (+ (list-ref bb 4) (list-ref bb 5)) 2.0))
  (define maxdim (max (- (list-ref bb 1) (list-ref bb 0))
                      (- (list-ref bb 3) (list-ref bb 2))
                      (- (list-ref bb 5) (list-ref bb 4))))
  (define sc (if (zero? maxdim) 1.0 (/ 1.6 maxdim)))

  ;; ---- ③ 编号 → 局部坐标 / uv（1 起始 → sub1；缺 vt 给 (0,0)）----
  (define (ploc i)
    (define q (vector-ref vtab (sub1 i)))
    (list (* (- (car q) cx) sc) (* (- (cadr q) cy) sc) (* (- (caddr q) cz) sc)))
  (define (uvl i)
    (if (and i (pair? vts))
        (let ([q (vector-ref vttab (sub1 i))]) (list (car q) (cadr q)))
        '(0.0 0.0)))
  (define (nloc i) (vector-ref vntab (sub1 i)))

  ;; 向量小工具：叉积 / 减法 / 归一化（叉积给"缺 vn 的面"现场算面法线）
  (define (cross a b)
    (list (- (* (cadr a) (caddr b)) (* (caddr a) (cadr b)))
          (- (* (caddr a) (car b)) (* (car a) (caddr b)))
          (- (* (car a) (cadr b)) (* (cadr a) (car b)))))
  (define (sub a b) (list (- (car a) (car b)) (- (cadr a) (cadr b)) (- (caddr a) (caddr b))))
  (define (norm a)
    (define l (sqrt (+ (* (car a) (car a)) (* (cadr a) (cadr a)) (* (caddr a) (caddr a)))))
    (if (zero? l) '(0.0 1.0 0.0) (list (/ (car a) l) (/ (cadr a) l) (/ (caddr a) l))))

  ;; ---- ④ 面 → 角点流：扇形三角化 + 去重（02 步）+ 法线 ----
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
    ;; 角点串 "v/vt/vn" 或 "v//vn" 或 "v/vt" → (v vt vn)，缺的给 #f
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
      (define face-n (norm (cross (sub p1 p0) (sub p2 p0))))   ; 面法线（缺 vn 时用）
      (define (emit c)
        (define vn-idx (caddr c))
        ;; ★有 vn 就用文件法线；没有就用面法线，且键里带面编号 → 硬边
        (define key (if vn-idx
                        (list (car c) (cadr c) vn-idx)
                        (begin (set! any-flat? #t)
                               (list 'flat fidx (car c) (cadr c)))))
        (define id (ensure! key (ploc (car c))
                            (if vn-idx (nloc vn-idx) face-n)
                            (uvl (cadr c))))
        (set! idxl (cons id idxl)))
      (emit (list-ref corners 0))
      (for ([j (in-range 1 (- n 1))])
        (emit (list-ref corners j))
        (emit (list-ref corners (add1 j))))
      (set! fidx (add1 fidx))))

  ;; ---- ⑤ 定稿：按下标铺成 f32vector + u32vector ----
  (define placed (make-vector total #f))
  (for ([e entries]) (vector-set! placed (car e) (cdr e)))
  (define ordered (for/list ([id (in-range total)]) (vector-ref placed id)))
  (define verts (apply f32vector (map exact->inexact (apply append ordered))))
  (define idx   (apply u32vector (reverse idxl)))
  (define tris  (quotient (length idxl) 3))
  (define summary
    (format "~a：~a 三角形 → ~a 顶点（8 float/顶点）~a"
            (file-name-from-path path) tris total
            (if any-flat? "；含缺 vn 的面（已叉积补 flat 法线）" "；全部有 vn（烘焙法线）")))
  (list verts idx summary any-flat?))

;; ---- 解析两个模型，打印摘要 ----
(define cube (obj-load-file cube-obj))
(define suz  (obj-load-file suz-obj))
(printf "~a~%" (caddr cube))
(printf "  前 8 个 float（第一个顶点 = 位置+法线+uv）：~a~%" (take (f32vector->list (car cube)) 8))
(printf "~a~%" (caddr suz))
(printf "  suzanne 法线是平滑烘焙的 → 05 步光照下会看到圆润表面~%")
