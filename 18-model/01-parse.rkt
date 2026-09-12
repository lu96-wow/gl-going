#lang racket/base
;; =========================================================
;; 18-model/01-parse.rkt —— 第一步：OBJ 是什么 + 逐行读
;; 运行：racket 18-model/01-parse.rkt
;; =========================================================
;; 前面的课里，立方体/四边形都是"手写顶点数组"。真实场景里模型在文件里
;; （OBJ / glTF…）。本课把模型文件变成 GL 能画的顶点数组。
;;
;; ★核心一句话：GL 根本不认识"文件"，它只认 VAO/VBO/EBO 里的数组。
;;   所以"导入模型" = 一条三步流水线：
;;
;;     ① 解析（CPU）：文本文件 → 纯数据数组        ← 本课 01/02/03 步
;;     ② 上传（GPU）：数组 → VBO/EBO/VAO           ← 05 步（旧知识拼装）
;;     ③ 绘制：      glDrawElements               ← 05 步（旧知识）
;;
;;   本步做流水线第①步的前半段：**把文件读进来，认出里面有哪些行**。
;;
;; 本步新增（2 个，同属"读 OBJ 文件"这一件事）：
;;   OBJ 文本格式 —— 四种行 v / vt / vn / f，其余行跳过
;;   逐行解析      —— open-input-file + read-line + string-split
;;
;; ★OBJ（Wavefront）是 GitHub 上最常见的文本模型格式，一个文件就是几类行：
;;     v  x  y  z        顶点坐标（编号从 1 开始）
;;     vt u  v           uv 坐标（可没有）
;;     vn nx ny nz       法线（可没有）
;;     f  1/2/3 4/5/6 ... 面：每格 = 顶点/uv/法线 三个编号，用 / 分隔
;;   # 开头是注释；mtllib / o / s / usemtl 等其它行本课一律跳过。
;;   本步只"数"这四类行各有多少，并把 f 那行的编号格式看明白。
;; =========================================================

(require racket/base)
(require racket/string racket/list racket/path)  ; string-split / take / file-name-from-path
(require racket/runtime-path)            ; define-runtime-path：相对本脚本目录找文件

;; 模型文件：相对"本脚本所在目录"解析，任意目录下运行都能找到 assets/
(define-runtime-path cube-obj "assets/cube.obj")

;; ---- 逐行读：只收 v / vt / vn / f 四类，其余行丢弃 ----
(define ip (open-input-file cube-obj))          ; 打开文件
(define vs '()) (define vts '()) (define vns '()) (define fs '())
(let loop ([line (read-line ip 'any)])          ; 一行一行读，读到 EOF 为止
  (unless (eof-object? line)
    (define toks (string-split line))           ; 按空白拆成一个个词
    (when (pair? toks)
      (case (car toks)                          ; 看第一个词是哪类行
        [("v")  (set! vs  (cons (map string->number (cdr toks)) vs))]   ; v → 顶点
        [("vt") (set! vts (cons (map string->number (cdr toks)) vts))]  ; vt → uv
        [("vn") (set! vns (cons (map string->number (cdr toks)) vns))]  ; vn → 法线
        [("f")  (set! fs  (cons (cdr toks) fs))]                        ; f → 面
        [else #f]))                              ; 其它行（# 注释等）跳过
    (loop (read-line ip 'any))))
(close-input-port ip)

;; ---- 打印统计：看看这个文件里到底有什么 ----
(printf "文件：~a~%" (file-name-from-path cube-obj))
(printf "  顶点 v   ：~a 个~%" (length vs))
(printf "  uv vt    ：~a 个~%" (length vts))
(printf "  法线 vn  ：~a 个~%" (length vns))
(printf "  面 f     ：~a 个~%" (length fs))

;; 前 3 个顶点坐标（读的时候是倒着 cons 的，reverse 回来才是文件顺序）
(printf "前 3 个 v：~a~%" (take (reverse vs) 3))

;; 前 2 个面——每格 "顶点/uv/法线" 三个编号，用 / 分隔
(printf "前 2 个 f：~a~%" (take (reverse fs) 2))
(printf "（f 的每格 = 顶点/uv/法线 三编号；下一步就拿它做索引化去重）~%")
