#lang racket
;; =========================================================
;; 10-texture/assets/gen-textures.rkt —— 重新生成本课测试纹理
;; 运行：racket glsl/10-texture/assets/gen-textures.rkt
;; 产出（在本文件夹）：
;;   cube.png   256x256 方块图案（给立方体贴图）
;;   floor.png  256x256 绿棋盘（给地板贴图，靠 GL_REPEAT 平铺）
;; 说明：这只是"造素材"用的 racket/draw 代码，不是 GL 代码。
;;       真正的 GL 纹理用法见 10-texture/ 下的步骤文件。
;; =========================================================

(require racket/draw)

(define (save-png file w h draw-fn)
  (define bmp (make-object bitmap% w h))
  (define dc  (make-object bitmap-dc% bmp))
  (draw-fn dc w h)
  (send bmp save-file file 'png)
  (printf "已生成 ~a (~ax~a)~%" file w h))

;; ---- 立方体贴图：深蓝底 + 橙圈 + 黄叉 + 边框，非对称好认方向 ----
(save-png "cube.png" 256 256
  (lambda (dc w h)
    (send dc set-brush (make-color 25 40 90) 'solid)
    (send dc set-pen (make-color 25 40 90) 1 'solid)
    (send dc draw-rectangle 0 0 w h)
    (send dc set-pen (make-color 240 200 90) 14 'solid)
    (send dc draw-rectangle 6 6 (- w 12) (- h 12))
    (send dc set-pen (make-color 240 240 200) 10 'solid)
    (send dc draw-line 20 20 (- w 20) (- h 20))
    (send dc draw-line (- w 20) 20 20 (- h 20))
    (send dc set-brush (make-color 235 120 60) 'solid)
    (send dc set-pen (make-color 235 120 60) 1 'solid)
    (send dc draw-ellipse 88 88 80 80)))

;; ---- 地板棋盘格：4x4 两色，边缘正好对齐 ----
(save-png "floor.png" 256 256
  (lambda (dc w h)
    (define cell (/ w 4))
    (for* ([r (in-range 4)] [c (in-range 4)])
      (define g (if (even? (+ r c)) 110 150))
      (send dc set-brush (make-color 46 g 46) 'solid)
      (send dc set-pen (make-color 46 g 46) 1 'solid)
      (send dc draw-rectangle (* c cell) (* r cell) cell cell))))
