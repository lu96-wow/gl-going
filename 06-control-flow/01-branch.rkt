#lang racket/base
;; =========================================================
;; 06-control-flow/01-branch.rkt —— 第一步：分情况画（when/unless/cond）
;; 运行：racket 06-control-flow/01-branch.rkt    点 X = 退出
;; =========================================================

;; 05 课的 shader 都是"一条公式算到底"。本课给 shader 加控制流，
;; 先学"分情况"：让 shader 能看条件走不同的路。
;;
;; 本步新增（1 组，同属"分支语句"这一件事）：
;;   when   —— 条件为真时执行体（没有 else）
;;   unless —— 条件为假时执行体（when 的反面）
;;   cond   —— 多分支（编译成 else if 链）
;;
;; ★语法（DSL 写成 S 表达式，展开成 GLSL 的 if / else if）：
;;   (when 条件 语句...)          →  if (条件) { ... }
;;   (unless 条件 语句...)        →  if (!(条件)) { ... }
;;   (cond [条件1 语句...]        →  if (条件1) { ... }
;;         [条件2 语句...]              else if (条件2) { ... }
;;         [else 语句...])              else { ... }
;;   条件里的 (and a b) (or a b) (not a) 是 05 课学过的逻辑运算符。
;;
;; ★注意：这里的 when / unless / cond 是"语句"（挑一段代码执行），
;;   05 课的三元 if 是"表达式"（挑一个值）。两者名字像，用途不同：
;;   要"算出一个值"用三元 if；要"有选择地执行几句话"用 when/unless/cond。
;;
;; 本步视觉：在会动的圈圈右上角，用 (when ...) 挖出一块"色板"——把那块区域的
;;   坐标 p 直接当颜色涂（红随 x、绿随 y、蓝随时间波）。这直观证明：
;;   "像素只是数据，shader 是对它算公式"。整块区域 = when 的分支。
;; =========================================================

(require "../02-triangle/04-gui-tool.rkt")   ; make-window（带视口）
(require "../racket-glsl/rewrite.rkt")        ; (glsl ...) 宏 + glsl-program-src
(require "../racket-glsl/rename-vector.rkt")  ; vec / vec4 / glsl-stride-bytes
(require "../racket-glsl/tool.rkt")           ; build-program / use-program / uniform-location

(define start-ms (current-inexact-milliseconds))

;; 顶点着色器（同 05 课：位置 + uv 两个属性，vUV 原样交给片元）。
(define vert-src
  (glsl (version 330 core)
        (layout (location 0) in vec2 aPos)
        (layout (location 1) in vec2 aUV)
        (out vec2 vUV)
        (define (main) void
          (set! vUV aUV)
          (set! gl_Position (vec4 aPos 0.0 1.0)))))

;; 片元着色器（本步主角）：
;;   前面的"会动的圈圈"= 05 课的环，再加 04 课学过的 uTime（每帧减它 → 环往外跑）。
;;   新增最后一段 (when ...)：
;;   (and (> (x p) 0.15) (> (y p) 0.15))  = "p.x 和 p.y 都大于 0.15"
;;     也就是右上角那一块（其余三个区域至少一个不满足）。
;;   体里把 c 改成"坐标当颜色"：红随 x、绿随 y、蓝随时间波。
(define frag-src
  (glsl (version 330 core)
        (in vec2 vUV)
        (uniform float uTime)
        (out vec4 FragColor)
        (define (main) void
          (vec2 p (- (* vUV 2.0) 1.0))
          (float d (length p))
          (float ring (fract (- (* d 6.0) uTime)))
          (vec3 c (mix (vec3 0.10 0.15 0.40) (vec3 0.10 0.70 1.00) ring))
          (set! c (* c (- 1.0 (* 0.55 d))))
          (set! c (+ c (* (vec3 0.15) (+ 0.5 (* 0.5 (sin (* uTime 2.0)))))))
          (when (and (> (x p) 0.15) (> (y p) 0.15))
            (set! c (vec3 (+ (* (x p) 0.5) 0.5)
                          (+ (* (y p) 0.5) 0.5)
                          (+ 0.5 (* 0.4 (sin (+ uTime (* (x p) 3.0))))))))
          (set! FragColor (vec4 (clamp c 0.0 1.0) 1.0)))))

(printf "片元着色器展开为：\n~a\n" (glsl-program-src frag-src))

;; 铺满窗口的四边形（同 05 课：6 个顶点 = 两个三角形）。
(define verts
  (vec (vec4 -1.0 -1.0  0.0 0.0)
       (vec4  1.0 -1.0  1.0 0.0)
       (vec4  1.0  1.0  1.0 1.0)
       (vec4 -1.0 -1.0  0.0 0.0)
       (vec4  1.0  1.0  1.0 1.0)
       (vec4 -1.0  1.0  0.0 1.0)))

;; 每帧：量时间 → 上传 uTime → 画。
(define (draw)
  (define t (/ (- (current-inexact-milliseconds) start-ms) 1000.0))
  (gl-clear-color 0.10 0.12 0.20 1.0)
  (gl-clear gl-color-buffer-bit)
  (use-program prog)
  (gl-uniform-1f loc-time t)
  (gl-bind-vertex-array vao)
  (gl-draw-arrays gl-triangles 0 6))

(define-values (frame canvas)
  (make-window #:title "06-01 右上角色板（分支）" #:width 400 #:height 400 #:draw draw))

;; 初始化：程序 + 查 uniform 位置 + 交错数据 VAO（同 04/05 课）。
(define prog
  (send canvas with-gl-context
    (lambda () (build-program (gl-vertex-shader vert-src) (gl-fragment-shader frag-src)))))
(define loc-time
  (send canvas with-gl-context (lambda () (uniform-location prog "uTime"))))
(define vao
  (send canvas with-gl-context
    (lambda ()
      (define data (vec->f32vector verts))
      (define vbo (u32vector-ref (gl-gen-buffers 1) 0))
      (gl-bind-buffer gl-array-buffer vbo)
      (gl-buffer-data gl-array-buffer (gl-vector-sizeof data) data gl-static-draw)
      (define v (u32vector-ref (gl-gen-vertex-arrays 1) 0))
      (gl-bind-vertex-array v)
      (gl-vertex-attrib-pointer 0 2 gl-float #f (glsl-stride-bytes 'vec2 'vec2) 0)
      (gl-enable-vertex-attrib-array 0)
      (gl-vertex-attrib-pointer 1 2 gl-float #f
                                (glsl-stride-bytes 'vec2 'vec2)
                                (glsl-stride-bytes 'vec2))
      (gl-enable-vertex-attrib-array 1)
      (gl-bind-vertex-array 0)
      v)))

;; 定时器（同 04 课）：每 16ms 触发重画。
(define ticker
  (new timer% (interval 16) (notify-callback (lambda () (send canvas refresh)))))

(send frame show #t)
