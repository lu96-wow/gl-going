#lang racket/base
;; =========================================================
;; 10-camera/lib.rkt —— 矩阵工具库（3D 版）+ 立方体网格 + 相机
;; =========================================================
;; 从 09-3d-depth/lib.rkt 复制，本课新增相机工具：
;;   01 步裸写 mat4-look-at 和网格地面 grid-verts，04 步收进本文件。
;;   矩阵约定不变：列主序 mat4 = f32vector[16]，元素 (r行,c列) 存下标 c*4+r。
;; =========================================================

(require "../racket-glsl/rename-vector.rkt")   ; mat4 / vec3 / concat-vecs / f32vector 工具

(provide mat4-identity mat4-translate mat4-rot-x mat4-rot-y mat4-rot-z
         mat4-scale mat4-mult mat4-ortho mat4-perspective mat4-look-at
         cube-verts cube-idx grid-verts
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

;; 视图矩阵 lookAt：eye=(ex,ey,ez) 看向 center=(cx,cy,cz)，up=(ux,uy,uz)。
;; 用 f（前）、s（右）、u（上）三个互相垂直的基向量 + 平移拼成
;; "把整个世界搬到相机面前"的矩阵 V。之后 gl_Position = P·V·M·v。
(define (mat4-look-at ex ey ez cx cy cz ux uy uz)
  ;; f = normalize(center - eye) —— 相机看的方向（前）
  (define fx (- cx ex)) (define fy (- cy ey)) (define fz (- cz ez))
  (define fl (sqrt (+ (* fx fx) (* fy fy) (* fz fz))))
  (define fxx (/ fx fl)) (define fyy (/ fy fl)) (define fzz (/ fz fl))
  ;; s = normalize(f × up) —— 相机右方向
  (define sx (- (* fyy uz) (* fzz uy)))
  (define sy (- (* fzz ux) (* fxx uz)))
  (define sz (- (* fxx uy) (* fyy ux)))
  (define sl (sqrt (+ (* sx sx) (* sy sy) (* sz sz))))
  (define sxx (/ sx sl)) (define syy (/ sy sl)) (define szz (/ sz sl))
  ;; u = s × f —— 重新正交化后的上方向（避免 f 和 up 不垂直时歪斜）
  (define uxx (- (* syy fzz) (* szz fyy)))
  (define uyy (- (* szz fxx) (* sxx fzz)))
  (define uzz (- (* sxx fyy) (* syy fxx)))
  ;; 三个基向量拼成旋转，眼睛位置投影取负拼成平移，合起来就是 V：
  ;;
  ;;      [ sx   sy   sz   -s.e ]
  ;;  V = [ ux   uy   uz   -u.e ]
  ;;      [ -fx  -fy  -fz  f.e  ]
  ;;      [ 0    0    0    1    ]
  ;;
  ;;  其中 s.e = sx*ex + sy*ey + sz*ez（s 与眼睛位置的点积），u.e / f.e 同理；
  ;;  矩阵里的 sx 就是代码里的 sxx（归一化后的 s 分量），u / f 同理。
  ;;  列主序：第 c 列第 r 个元素存下标 c*4+r，所以下面的 mat4 依次写
  ;;  列 0=(sx,ux,-fx,0)  列 1=(sy,uy,-fy,0)  列 2=(sz,uz,-fz,0)  列 3=(-s.e,-u.e,f.e,1)
  (mat4 sxx uxx (- fxx) 0.0
             syy uyy (- fyy) 0.0
             szz uzz (- fzz) 0.0
             (- (+ (* sxx ex) (* syy ey) (* szz ez)))
             (- (+ (* uxx ex) (* uyy ey) (* uzz ez)))
             (+ (* fxx ex) (* fyy ey) (* fzz ez))
             1.0))

;; 网格地面：XZ 平面（y=0）上一堆 GL_LINES 顶点（位置 vec3 + 颜色 vec3），
;; 从 -span 到 span 每 step 一条，共 2 个方向（沿 x、沿 z）。
(define (grid-verts span step)
  (define color (vec3 0.30 0.32 0.50))
  (apply concat-vecs
         (apply append
                (for/list ([s (in-range (- span) (+ span step) step)])
                  (list (vec3 (- span) 0.0 s) color   ; 沿 x 的线
                        (vec3 span 0.0 s) color
                        (vec3 s 0.0 (- span)) color   ; 沿 z 的线
                        (vec3 s 0.0 span) color)))))

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
