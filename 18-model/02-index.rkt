#lang racket/base
;; =========================================================
;; 18-model/02-index.rkt —— 第二步：索引化去重（省下 80% 的顶点）
;; 运行：racket 18-model/02-index.rkt
;; =========================================================
;; 上一步：读出了 v/vt/vn/f 四类行。本步做模型加载里最有价值的一步——
;; **索引化去重**，把重复存的顶点合并成"一份数据 + 一张编号表"。
;;
;; 本步新增（1 个核心概念）：
;;   索引化去重 —— "角点" = (顶点, uv, 法线) 三编号；三编号相同的角点只存一份
;;
;; ★问题在哪：OBJ 的面是**共享角点**的。比如立方体 8 个角、12 个三角形，
;;   如果每个三角形都存满 3 个顶点（"直接展开"），要存 12×3 = 36 个顶点，
;;   但真正的角点只有 8 个（uv/法线不同时会多一点）——同一份坐标被重复
;;   存了好几遍。
;;
;; ★解法（= 06 课 EBO 的通用版）：把"唯一的角点"存一遍，再存一张编号表
;;   （索引），GPU 按编号去取。一个角点 = 顶点/uv/法线 **三编号都相同**，
;;   才算同一个（三编号有一个不同，就要分开存，因为数据不同）。
;;
;; 本步只统计"去重省多少"，不构建最终数组（03 步构建）。
;; =========================================================

(require racket/base)
(require racket/string racket/list racket/path)
(require racket/runtime-path)

(define-runtime-path cube-obj "assets/cube.obj")
(define-runtime-path suz-obj  "assets/suzanne.obj")

;; 读一个 OBJ 文件，收集 v / vt / vn / f 四类行（同 01 步）
(define (read-obj path)
  (define ip (open-input-file path))
  (define vs '()) (define vts '()) (define vns '()) (define fs '())
  (let loop ([line (read-line ip 'any)])
    (unless (eof-object? line)
      (define toks (string-split line))
      (when (pair? toks)
        (case (car toks)
          [("v")  (set! vs  (cons (map string->number (cdr toks)) vs))]
          [("vt") (set! vts (cons (map string->number (cdr toks)) vts))]
          [("vn") (set! vns (cons (map string->number (cdr toks)) vns))]
          [("f")  (set! fs  (cons (cdr toks) fs))]
          [else #f]))
      (loop (read-line ip 'any))))
  (close-input-port ip)
  (values vs vts vns fs))

;; 统计一个模型的：三角形数 / 直接展开顶点数 / 去重后顶点数
(define (count-mesh path)
  (define-values (vs vts vns fs) (read-obj path))
  (define lookup (make-hash))        ; 角点键 → #t（只数种类）
  (define tri 0)
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
      ;; 扇形三角化：角点 0 + 角点 j + 角点 j+1，n 边形拆成 n-2 个三角形
      (for ([j (in-range 1 (- n 1))])
        (for ([c (list (list-ref corners 0) (list-ref corners j) (list-ref corners (add1 j)))])
          (hash-set! lookup c #t))    ; ★去重：三编号相同的角点只记一次
        (set! tri (add1 tri)))))
  (values tri (* tri 3) (hash-count lookup)))

;; ---- 对比两个模型 ----
(for ([path (list cube-obj suz-obj)])
  (define-values (tris expanded dedup) (count-mesh path))
  (define saved (round (/ (* 100.0 (- expanded dedup)) expanded)))
  (printf "~a：~a 个三角形~%" (file-name-from-path path) tris)
  (printf "  直接展开 ~a 顶点 → 去重后 ~a 顶点，省 ~a%~%" expanded dedup saved))
