#lang racket/base
;; =========================================================
;; 09-3d-depth/02-solid.rkt —— 第二步：实心面 + 深度缓冲
;; 运行：racket 09-3d-depth/02-solid.rkt    点 X = 退出
;; =========================================================

;; 上一步：线框立方体"透亮"（背面的边也可见）。本步画**实心**立方体，并解决
;; 随之而来的新问题：哪个面在前、哪个面在后。
;;
;; 本步新增（2 个，同属"实心 3D"这一件事）：
;;   ① 实心立方体 —— 6 个面 × 4 顶点 = 24 顶点、36 索引，每面一色
;;   ② 深度缓冲 —— gl-enable gl-depth-test + 每帧清 gl-depth-buffer-bit
;;
;; ★注意：深度缓冲要"两半"——窗口侧申请 + 使用时开启。窗口侧（gl-config% 的
;;   set-depth-size 24）已由本课 gui-tool 申请好（见 gui-tool.rkt）；本步学的
;;   是"使用"那一半：先 gl-enable gl-depth-test 打开比较，再每帧清掉上一帧的 z。
;;
;; ★为什么需要深度缓冲：实心立方体有 6 个面，前面的面必须挡住后面的面。
;;   但 GPU 按你提交的顺序画，谁后画谁盖在上面——顺序错了就"后面画到前面"。
;;   深度缓冲 = 给每个像素记一个"离相机多近"的 z；画新片元前先比较，
;;   更远的画不过更近的 → 遮挡关系自动正确，与画图顺序无关。
;;   （线框不需要，因为你要看穿它；实心必须开。）
;;
;; ★立方体为什么 24 个顶点而不是 8 个：8 个角是共享的，但每个面颜色不同，
;;   同一个角在不同面上颜色不一样 → 必须拆开存（每面 4 个）。
;;
;; 本步视觉：实心立方体翻滚（绕 y 再绕 x），六个面六种颜色，前后遮挡正确。
;; =========================================================

(require "gui-tool.rkt")              ; make-window（带深度缓冲）+ start-animation
(require "../racket-glsl/rewrite.rkt")
(require "../racket-glsl/rename-vector.rkt")  ; vec3 / concat-vecs / u16vector / glsl-*
(require "../racket-glsl/tool.rkt")
(require "lib.rkt")                   ; mat4-*

(define start-ms (current-inexact-milliseconds))

(define vert-src
  (glsl (version 330 core)
        (layout (location 0) in vec3 aPos)
        (layout (location 1) in vec3 aColor)
        (uniform mat4 uMVP)
        (out vec3 vColor)
        (define (main) void
          (set! vColor aColor)
          (set! gl_Position (* uMVP (vec4 aPos 1.0))))))

(define frag-src
  (glsl (version 330 core)
        (in vec3 vColor)
        (out vec4 FragColor)
        (define (main) void
          (set! FragColor (vec4 vColor 1.0)))))

;; 8 个角（pos8）
(define pos8
  (list (vec3 -1.0 -1.0  1.0) (vec3  1.0 -1.0  1.0) (vec3  1.0  1.0  1.0) (vec3 -1.0  1.0  1.0)
        (vec3 -1.0 -1.0 -1.0) (vec3  1.0 -1.0 -1.0) (vec3  1.0  1.0 -1.0) (vec3 -1.0  1.0 -1.0)))
;; 6 个面：(颜色, 四个角编号)
(define faces
  (list (list (vec3 0.85 0.20 0.20) '(0 1 2 3))   ; +z 前 红
        (list (vec3 0.20 0.80 0.25) '(5 4 7 6))   ; -z 后 绿
        (list (vec3 0.95 0.60 0.10) '(1 5 6 2))   ; +x 右 橙
        (list (vec3 0.95 0.85 0.15) '(4 0 3 7))   ; -x 左 黄
        (list (vec3 0.20 0.60 0.95) '(3 2 6 7))   ; +y 上 蓝
        (list (vec3 0.75 0.30 0.90) '(4 5 1 0)))) ; -y 下 紫

;; 展开成 24 顶点：每个面 4 个，每个顶点 = 位置 vec3 + 颜色 vec3
(define (face-verts f)
  (define color (car f))
  (define is (cadr f))
  (apply append (for/list ([i is]) (list (list-ref pos8 i) color))))
(define verts (apply concat-vecs (apply append (map face-verts faces))))

;; 36 索引：每个面两个三角形（0,1,2 和 0,2,3，基准 +i*4）
(define idx
  (apply u16vector
         (apply append
                (for/list ([i (in-range 6)])
                  (define b (* i 4))
                  (list b (+ b 1) (+ b 2)  b  (+ b 2) (+ b 3))))))

(define (draw)
  (define t (/ (- (current-inexact-milliseconds) start-ms) 1000.0))
  (define V (mat4-translate 0.0 0.0 -6.0))
  (define P (mat4-ortho -2.0 2.0 -2.0 2.0 0.1 100.0))
  (define M (mat4-mult (mat4-rot-y (* t 40.0)) (mat4-rot-x (* t 30.0))))  ; 绕 y 再绕 x 翻滚
  (gl-clear-color 0.10 0.12 0.20 1.0)
  ;; ★每帧同时清 颜色 和 深度（深度不清的话上一帧的 z 还留着，会画错）
  (gl-clear (bitwise-ior gl-color-buffer-bit gl-depth-buffer-bit))
  (use-program prog)
  (gl-uniform-matrix-4fv loc-mvp 1 #f (mat4-mult (mat4-mult P V) M))
  (gl-bind-vertex-array vao)
  (gl-draw-elements gl-triangles 36 gl-unsigned-short 0))

(define-values (frame canvas)
  (make-window #:title "09-02 实心立方体（深度）" #:width 400 #:height 400 #:draw draw))

(define prog
  (send canvas with-gl-context
    (lambda () (build-program (gl-vertex-shader vert-src) (gl-fragment-shader frag-src)))))
(define loc-mvp
  (send canvas with-gl-context (lambda () (uniform-location prog "uMVP"))))
(define vao
  (send canvas with-gl-context
    (lambda ()
      (gl-enable gl-depth-test)   ; ★开深度测试（一次即可）
      (define vbo (u32vector-ref (gl-gen-buffers 1) 0))
      (gl-bind-buffer gl-array-buffer vbo)
      (gl-buffer-data gl-array-buffer (gl-vector-sizeof verts) verts gl-static-draw)
      (define v (u32vector-ref (gl-gen-vertex-arrays 1) 0))
      (gl-bind-vertex-array v)
      (gl-vertex-attrib-pointer 0 (glsl-size 'vec3) gl-float #f (glsl-stride-bytes 'vec3 'vec3) 0)
      (gl-enable-vertex-attrib-array 0)
      (gl-vertex-attrib-pointer 1 (glsl-size 'vec3) gl-float #f
                                (glsl-stride-bytes 'vec3 'vec3)
                                (glsl-stride-bytes 'vec3))
      (gl-enable-vertex-attrib-array 1)
      (define ebo (u32vector-ref (gl-gen-buffers 1) 0))
      (gl-bind-buffer gl-element-array-buffer ebo)
      (gl-buffer-data gl-element-array-buffer (gl-vector-sizeof idx) idx gl-static-draw)
      (gl-bind-vertex-array 0)
      v)))

(define ticker (start-animation canvas 16))

(send frame show #t)
