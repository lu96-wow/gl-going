#lang racket/base
;; =========================================================
;; 06-control-flow/04-shadertoy.rkt —— 第四步：综合，会动的着色玩具
;; 运行：racket 06-control-flow/04-shadertoy.rkt    点 X = 退出
;; =========================================================

;; 本课前三步分别讲了：分支 when(01)、for 循环(02)、自写函数(03)。
;; 本步**不引入任何新语法**，把"控制流 + 动起来"全部串进一个完整作品里。
;;
;; 逐行读（每一行都能在前几步 / 前几课找到出处）：
;;   uniform uTime              = 04 课：CPU 每帧传来的时间
;;   p = vUV*2-1                = 05 课算术：像素坐标，中心 (0,0)
;;   d = length(p)              = 05 课几何函数：到中心的距离 → 同心圆
;;   ring = fract(d*6-uTime)    = 05 课 fract + 减时间 → 环往外跑
;;   c = mix(深蓝,亮蓝,ring)     = 05 课混色：给环上色
;;   c *= (1-0.55*d)            = 05 课算术：越靠边越暗
;;   for 叠加波纹                = 02 步：同一公式跑 4 遍，参数 i 每次不同
;;   c += vec3(0.15)*pulse(...)  = 03 步自写函数 + 05 课 sin：整体明暗呼吸
;;   (when 右上角 ...)           = 01 步分支：挖出色板，坐标当颜色
;;   clamp(c,0,1)               = 05 课钳制：夹回合法颜色
;;
;; 现在你的 shader 已经"活"了：环在跑、波纹在动、亮度在呼吸、右上角一块
;; 实时色板。它只用到了 类型、表达式、uniform、分支、循环、函数。
;; =========================================================

(require "../04-animate/05-gui-tool.rkt")  ; make-window + start-animation（04 课收的工具）
(require "../racket-glsl/rewrite.rkt")        ; (glsl ...) 宏 + glsl-program-src
(require "../racket-glsl/rename-vector.rkt")  ; vec / vec4 / glsl-stride-bytes
(require "../racket-glsl/tool.rkt")           ; build-program / use-program / uniform-location

(define start-ms (current-inexact-milliseconds))

;; 顶点着色器（同前几步）。
(define vert-src
  (glsl (version 330 core)
        (layout (location 0) in vec2 aPos)
        (layout (location 1) in vec2 aUV)
        (out vec2 vUV)
        (define (main) void
          (set! vUV aUV)
          (set! gl_Position (vec4 aPos 0.0 1.0)))))

;; 片元着色器：本课全部知识的综合（动圈 + 波纹 + 呼吸 + 右上角色板）。
(define frag-src
  (glsl (version 330 core)
        (in vec2 vUV)
        (uniform float uTime)
        (out vec4 FragColor)
        ;; 自写函数：把 sin(-1..1) 映射到 0..1（03 步）
        (define (pulse (float x) (float phase)) float
          (* 0.5 (+ 1.0 (sin (+ x phase)))))
        (define (main) void
          (vec2 p (- (* vUV 2.0) 1.0))
          (float d (length p))
          (float ring (fract (- (* d 6.0) uTime)))
          (vec3 c (mix (vec3 0.10 0.15 0.40) (vec3 0.10 0.70 1.00) ring))
          (set! c (* c (- 1.0 (* 0.55 d))))
          ;; for 叠加波纹（02 步）：4 圈频率不同的涟漪
          (float s 0.0)
          (for (int i 1) (<= i 4) (++ i)
            (set! s (+ s (* 0.06 (sin (+ (* d 24.0) (* uTime i)))))))
          (set! c (+ c (vec3 s)))
          ;; 呼吸：用自写函数 pulse 让整体明暗起伏
          (set! c (+ c (* (vec3 0.15) (pulse (* uTime 2.0) 0.0))))
          ;; 分支（01 步）：右上角挖色板，坐标当颜色
          (when (and (> (x p) 0.15) (> (y p) 0.15))
            (set! c (vec3 (+ (* (x p) 0.5) 0.5)
                          (+ (* (y p) 0.5) 0.5)
                          (+ 0.5 (* 0.4 (sin (+ uTime (* (x p) 3.0))))))))
          (set! FragColor (vec4 (clamp c 0.0 1.0) 1.0)))))

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
  (make-window #:title "06-04 会动的着色玩具（综合）" #:width 400 #:height 400 #:draw draw))

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

(define ticker (start-animation canvas 16))

(send frame show #t)
