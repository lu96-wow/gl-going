#lang racket/base
;; =========================================================
;; 08-transform/lib.rkt —— 矩阵工具库（本课版）
;; =========================================================
;; 本课 01-03 步裸写了 平移/旋转/缩放/矩阵乘法，04 步收进本文件；
;; 05 步裸写 mat4-ortho，06 步也收进来。之后画东西直接调 mat4-*。
;;
;; 矩阵约定：列主序 mat4 = f32vector[16]，元素 (r行,c列) 存下标 c*4+r，
;; 与 GL 的 uniform mat4 一致，直接上传，零转换。
;; =========================================================

(require "../racket-glsl/rename-vector.rkt")   ; mat4 / f32vector 工具

(define PI (acos -1.0))   ; lib.rkt 不 require racket/gui，pi 自己定义

(provide mat4-identity mat4-translate mat4-rot-z mat4-scale mat4-mult mat4-ortho
         (all-from-out "../racket-glsl/rename-vector.rkt"))   ; mat4 等

;; 单位矩阵（mat4 单标量 = 对角矩阵）
(define (mat4-identity) (mat4 1.0))

;; 平移：单位阵的第 4 列放 (tx, ty, 0, 1)
(define (mat4-translate tx ty)
  (mat4 1.0 0.0 0.0 0.0
             0.0 1.0 0.0 0.0
             0.0 0.0 1.0 0.0
             tx  ty  0.0 1.0))

;; 绕 z 轴旋转（角度制）
(define (mat4-rot-z deg)
  (define r (* (/ PI 180.0) deg))
  (define c (cos r))
  (define s (sin r))
  (mat4 c     s     0.0 0.0
             (- s) c     0.0 0.0
             0.0   0.0   1.0 0.0
             0.0   0.0   0.0 1.0))

;; 缩放：对角线放倍数（z 保持 1）
(define (mat4-scale sx sy)
  (mat4 sx  0.0 0.0 0.0
             0.0 sy  0.0 0.0
             0.0 0.0 1.0 0.0
             0.0 0.0 0.0 1.0))

;; 矩阵乘法 A·B（列主序，元素 (r行,c列) 存下标 c*4+r）
(define (mat4-mult A B)
  (define R (make-f32vector 16 0.0))
  (for* ([c (in-range 4)] [r (in-range 4)] [k (in-range 4)])
    (f32vector-set! R (+ (* 4 c) r)
                    (+ (f32vector-ref R (+ (* 4 c) r))
                       (* (f32vector-ref A (+ (* 4 k) r))
                          (f32vector-ref B (+ (* 4 c) k))))))
  R)

;; 正交投影：把 [l,r]×[b,t] 映射到 NDC（像素世界常用 l=0, r=w, b=h, t=0）
(define (mat4-ortho l r b t n f)
  (define rl (- r l)) (define tb (- t b)) (define fn (- f n))
  (mat4 (/ 2.0 rl) 0.0 0.0 0.0
             0.0 (/ 2.0 tb) 0.0 0.0
             0.0 0.0 (/ -2.0 fn) 0.0
             (- (/ (+ r l) rl)) (- (/ (+ t b) tb)) (- (/ (+ f n) fn)) 1.0))
