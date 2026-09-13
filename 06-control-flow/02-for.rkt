#lang racket/base
;; =========================================================
;; 06-control-flow/02-for.rkt —— 第二步：for 循环画条纹
;; 运行：racket 06-control-flow/02-for.rkt    点 X = 退出
;; =========================================================

;; 上一步：会用 when 分情况。本步学"重复"——把同一段公式跑很多遍，参数每次变。
;;
;; 本步新增（1 个）：
;;   for —— 循环：初始化循环变量 → 每次检查条件 → 执行体 → 更新变量
;;
;; ★语法（DSL 写法 → 展开成 GLSL）：
;;   (for (int i 1) (<= i 6) (++ i) 语句...)
;;     →  for (int i = 1; i <= 6; ++i) { ... }
;;   三部分：初始化（声明循环变量 int i = 1）、条件（继续跑？）、更新（每次 ++i）。
;;
;; ★★★ GLSL 最重要的纪律：循环边界必须是"编译期常数"★★★
;;   这里的 6 是写死的数字。GPU 需要**提前知道**循环会跑几次（编译期展开、
;;   调度），所以不能写"跑到某个 uniform 为止"（如 i < uCount 在多数驱动上
;;   直接编译失败）。这是 GLSL 和 C/Racket 最大的区别：循环次数在写代码时
;;   就定死。
;;
;; 本步视觉（会动的条纹）：s 从 0 开始，循环 6 次、每次叠加一个正弦：
;;   (* 6.28 i)  = 2π·i = 频率（i=1 时一条波横跨屏幕，i=6 时六条）
;;   (* uTime i) = 相位随时间走，速度 i（频率越高动得越快）
;;   6 条不同频率的波叠在一起 → 明暗相间的竖条纹，而且随时间流动。
;;   你看到的是"循环"在数学上最直观的用法：同样的公式，i 每次不同。
;; =========================================================

(require "../02-triangle/04-gui-tool.rkt")   ; make-window（带视口）
(require "../racket-glsl/rewrite.rkt")        ; (glsl ...) 宏 + glsl-program-src
(require "../racket-glsl/rename-vector.rkt")  ; vec / vec4 / glsl-stride-bytes
(require "../racket-glsl/tool.rkt")           ; build-program / use-program / uniform-location

(define start-ms (current-inexact-milliseconds))

;; 顶点着色器（同 01 步）。
(define vert-src
  (glsl (version 330 core)
        (layout (location 0) in vec2 aPos)
        (layout (location 1) in vec2 aUV)
        (out vec2 vUV)
        (define (main) void
          (set! vUV aUV)
          (set! gl_Position (vec4 aPos 0.0 1.0)))))

;; 片元着色器（本步主角）：
;;   (float s 0.0)                    累加器，从 0 开始
;;   (for (int i 1) (<= i 6) (++ i)   i 从 1 到 6，每次 +1
;;     (set! s (+ s (* 0.15 ...))))   每轮叠加一条正弦（频率随 i 升高）
;;   注意 DSL 的 (set! s (+ s x)) 就是 GLSL 的 s = s + x。
(define frag-src
  (glsl (version 330 core)
        (in vec2 vUV)
        (uniform float uTime)
        (out vec4 FragColor)
        (define (main) void
          (vec2 p (- (* vUV 2.0) 1.0))
          (float s 0.0)
          (for (int i 1) (<= i 6) (++ i)
            (set! s (+ s (* 0.15 (sin (+ (* (x p) (* 6.28 i)) (* uTime i)))))))
          (set! FragColor (vec4 (vec3 (+ 0.5 (* 0.5 s))) 1.0)))))

(printf "片元着色器展开为：\n~a\n" (glsl-program-src frag-src))

(define verts
  (vec (vec4 -1.0 -1.0  0.0 0.0)
       (vec4  1.0 -1.0  1.0 0.0)
       (vec4  1.0  1.0  1.0 1.0)
       (vec4 -1.0 -1.0  0.0 0.0)
       (vec4  1.0  1.0  1.0 1.0)
       (vec4 -1.0  1.0  0.0 1.0)))

(define (draw)
  (define t (/ (- (current-inexact-milliseconds) start-ms) 1000.0))
  (gl-clear-color 0.10 0.12 0.20 1.0)
  (gl-clear gl-color-buffer-bit)
  (use-program prog)
  (gl-uniform-1f loc-time t)
  (gl-bind-vertex-array vao)
  (gl-draw-arrays gl-triangles 0 6))

(define-values (frame canvas)
  (make-window #:title "06-02 for 条纹" #:width 400 #:height 400 #:draw draw))

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

(define ticker
  (new timer% (interval 16) (notify-callback (lambda () (send canvas refresh)))))

(send frame show #t)
