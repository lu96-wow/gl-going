#lang racket/base
;; =========================================================
;; 09-3d-depth/lib.rkt —— 矩阵工具库（3D 版）+ 立方体网格
;; =========================================================
;; 从 08-transform/lib.rkt 复制，本课把矩阵升级到 3D：
;;   mat4-translate / mat4-scale 加 z 参数；新增 mat4-rot-x / mat4-rot-y。
;;   03 步裸写 mat4-perspective，04 步收进本文件；
;;   02 步裸写立方体数据，04 步也收进来（cube-verts / cube-idx）。
;; 矩阵约定不变：列主序 mat4 = f32vector[16]，元素 (r行,c列) 存下标 c*4+r。
;; =========================================================

(require "../racket-glsl/rename-vector.rkt")   ; mat4 / vec3 / concat-vecs / f32vector 工具

(provide mat4-identity mat4-translate mat4-rot-x mat4-rot-y mat4-rot-z
         mat4-scale mat4-mult mat4-ortho mat4-perspective
         cube-verts cube-idx
         (all-from-out "../racket-glsl/rename-vector.rkt"))   ; mat4 等

(define PI (acos -1.0))

;; 单位矩阵
(define (mat4-identity) (mat4 1.0))

;; 平移：第 4 列放 (tx, ty, tz, 1)
(define (mat4-translate tx ty tz)
  (mat4 1.0 0.0 0.0 0.0
             0.0 1.0 0.0 0.0
             0.0 0.0 1.0 0.0
             tx  ty  tz  1.0))

;; 旋转（绕各轴，角度制）。绕 z 就是 2D 旋转；绕 x/y 是 3D 新增的。
(define (mat4-rot-z deg)
  (define r (* (/ PI 180.0) deg))
  (define c (cos r)) (define s (sin r))
  (mat4 c     s     0.0 0.0
             (- s) c     0.0 0.0
             0.0   0.0   1.0 0.0
             0.0   0.0   0.0 1.0))

(define (mat4-rot-x deg)
  (define r (* (/ PI 180.0) deg))
  (define c (cos r)) (define s (sin r))
  (mat4 1.0 0.0    0.0   0.0
             0.0 c     s     0.0
             0.0 (- s) c     0.0
             0.0 0.0    0.0   1.0))

(define (mat4-rot-y deg)
  (define r (* (/ PI 180.0) deg))
  (define c (cos r)) (define s (sin r))
  (mat4 c    0.0 (- s) 0.0
             0.0  1.0 0.0    0.0
             s    0.0 c      0.0
             0.0  0.0 0.0    1.0))

;; 缩放：对角线放 (sx, sy, sz, 1)
(define (mat4-scale sx sy sz)
  (mat4 sx  0.0 0.0 0.0
             0.0 sy  0.0 0.0
             0.0 0.0 sz  0.0
             0.0 0.0 0.0 1.0))

;; 矩阵乘法 A·B（先作用 B，再作用 A）
(define (mat4-mult A B)
  (define R (make-f32vector 16 0.0))
  (for* ([c (in-range 4)] [r (in-range 4)] [k (in-range 4)])
    (f32vector-set! R (+ (* 4 c) r)
                    (+ (f32vector-ref R (+ (* 4 c) r))
                       (* (f32vector-ref A (+ (* 4 k) r))
                          (f32vector-ref B (+ (* 4 c) k))))))
  R)

;; 正交投影：把 [l,r]×[b,t]（深度 [n,f]）映射到 NDC
(define (mat4-ortho l r b t n f)
  (define rl (- r l)) (define tb (- t b)) (define fn (- f n))
  (mat4 (/ 2.0 rl) 0.0 0.0 0.0
             0.0 (/ 2.0 tb) 0.0 0.0
             0.0 0.0 (/ -2.0 fn) 0.0
             (- (/ (+ r l) rl)) (- (/ (+ t b) tb)) (- (/ (+ f n) fn)) 1.0))

;; 透视投影：fovy=垂直视角(度)、aspect=宽/高、near/far=近远平面(正数)。
;; 让 w=-z，GPU 透视除法后产生"近大远小"。
(define (mat4-perspective fovy aspect near far)
  (define f (/ 1.0 (tan (* 0.5 (/ PI 180.0) fovy))))
  (define nf (/ (+ near far) (- near far)))
  (define n2f (/ (* 2.0 near far) (- near far)))
  (mat4 (/ f aspect) 0.0 0.0 0.0
             0.0 f 0.0 0.0
             0.0 0.0 nf -1.0
             0.0 0.0 n2f 0.0))

;; =========================================================
;; 立方体网格：6 面 × 4 顶点（每个 = 位置 vec3 + 颜色 vec3），36 索引。
;; 02 步裸写过构造过程，04 步收进这里供后面复用。
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
