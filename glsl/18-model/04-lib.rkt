#lang racket/base
;; =========================================================
;; 18-model/04-lib.rkt —— 第四步：把 OBJ 解析器收进 lib.rkt
;; 运行：racket glsl/18-model/04-lib.rkt
;; =========================================================
;; 前 3 步裸写了 OBJ 解析器（读行 → 去重 → 法线+归一化）。这套逻辑以后
;; 每课都可能复用，所以收进本文件夹 lib.rkt 的 obj-load-file。
;;
;; obj-load-file 的实现 = 前 3 步的逐块拼装，见 lib.rkt 末尾：
;;   01 步  open-input-file + read-line 收集 v/vt/vn/f
;;   02 步  角点三编号去重（索引化）
;;   03 步  法线（vn 直接 / 叉积补）+ 包围盒归一化
;;
;; ★节奏（整门课通用）：
;;   ① 裸写新机制（前 3 步）
;;   ② 发现它可复用 → 收进 lib（本步）
;;   ③ 后面的课直接调用，聚焦本课真正的新东西
;;
;; ★注意：obj-load-file 是**纯 CPU** 函数——读文件、算数组，完全不碰 GL。
;;   所以本步不需要窗口、不需要 GL 上下文，直接打印验证即可。
;;   （raylib 的 LoadModel 也是先纯 CPU 解析出 Mesh，rlLoadMesh 才碰 GPU。）
;; =========================================================

(require "lib.rkt")              ; obj-load-file + obj-mesh-* + (glsl ...) + mat4-*…
(require racket/runtime-path)

(define-runtime-path cube-obj "assets/cube.obj")
(define-runtime-path suz-obj  "assets/suzanne.obj")

;; 调用收进 lib 的加载器，打印摘要（和前 3 步裸写版结果一致）
(define cube (obj-load-file cube-obj))
(define suz  (obj-load-file suz-obj))

(printf "~a~%" (obj-mesh-summary cube))
(printf "  verts ~a float，idx ~a 个索引~%"
        (f32vector-length (obj-mesh-verts cube)) (u32vector-length (obj-mesh-idx cube)))
(printf "~a~%" (obj-mesh-summary suz))
(printf "  verts ~a float，idx ~a 个索引~%"
        (f32vector-length (obj-mesh-verts suz)) (u32vector-length (obj-mesh-idx suz)))
(printf "下一步：把这些数组灌进 VBO/EBO，配上 attribute，模型就画出来了~%")
