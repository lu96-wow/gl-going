#lang racket/base
;; =========================================================
;; 05-glsl-types/06-shadertoy.rkt —— 第六步：综合，把本课全串起来
;; 运行：racket 05-glsl-types/06-shadertoy.rkt    点 X = 退出
;; =========================================================

;; 前五步分别讲了：铺满窗口的四边形(01)、类型与构造器(02)、swizzle(03)、
;; 字面量与运算符(04)、内建数学函数(05)。本步**不引入任何新语法**，
;; 把它们全部用进一个"着色玩具"里，验证你读 shader 的能力已经够用。
;;
;; 逐行读（每一行都能在前五步找到出处）：
;;   vec2 p = vUV*2-1         算术(04)：像素坐标，中心 = (0,0)
;;   d = length(p)            几何函数(05)：到中心的距离 → 同心圆
;;   ring = fract(d*6)        取小数(05)：半径上 0..1 循环 6 次 → 环
;;   c = mix(深蓝,亮蓝,ring)   混色(05)：按 ring 给环上色
;;   c *= (1 - 0.55*d)        算术(04)+构造器(02)：越靠边越暗
;;   c += vec3(0.15)*(0.5+0.5*sin(d*12))  单标量构造器(02)+sin(05)：一圈圈明暗涟漪
;;   clamp(c,0,1)             钳制(05)：夹回合法颜色
;;   vec4(c,1.0)              升维构造器(02)：补 alpha 输出
;;
;; 现在你能把一段 GLSL 当"像素的数学公式"来读了。下一课（06 控制流）给它加
;; if/for/函数；再往后用 uTime 让这些环真正动起来。

(require "../02-triangle/04-gui-tool.rkt")   ; make-window（带视口）
(require "../racket-glsl/rewrite.rkt")        ; (glsl ...) 宏 + glsl-program-src
(require "../racket-glsl/rename-vector.rkt")  ; vec / vec4 / glsl-stride-bytes
(require "../racket-glsl/tool.rkt")           ; build-program / use-program

(define vert-src
  (glsl (version 330 core)
        (layout (location 0) in vec2 aPos)
        (layout (location 1) in vec2 aUV)
        (out vec2 vUV)
        (define (main) void
          (set! vUV aUV)
          (set! gl_Position (vec4 aPos 0.0 1.0)))))

;; 片元着色器：本课全部知识的综合（静态画面，无控制流、无 uniform）
(define frag-src
  (glsl (version 330 core)
        (in vec2 vUV)
        (out vec4 FragColor)
        (define (main) void
          (vec2 p (- (* vUV 2.0) 1.0))
          (float d (length p))
          (float ring (fract (* d 6.0)))
          (vec3 c (mix (vec3 0.10 0.15 0.40) (vec3 0.10 0.70 1.00) ring))
          (set! c (* c (- 1.0 (* 0.55 d))))
          (set! c (+ c (* (vec3 0.15) (+ 0.5 (* 0.5 (sin (* d 12.0)))))))
          (set! FragColor (vec4 (clamp c 0.0 1.0) 1.0)))))

;; 打印展开出的完整 GLSL（本课的"毕业作品"）
(printf "片元着色器展开为：\n~a\n" (glsl-program-src frag-src))

(define verts
  (vec (vec4 -1.0 -1.0  0.0 0.0)
       (vec4  1.0 -1.0  1.0 0.0)
       (vec4  1.0  1.0  1.0 1.0)
       (vec4 -1.0 -1.0  0.0 0.0)
       (vec4  1.0  1.0  1.0 1.0)
       (vec4 -1.0  1.0  0.0 1.0)))

(define (draw)
  (gl-clear-color 0.10 0.12 0.20 1.0)
  (gl-clear gl-color-buffer-bit)
  (use-program prog)
  (gl-bind-vertex-array vao)
  (gl-draw-arrays gl-triangles 0 6))

(define-values (frame canvas)
  (make-window #:title "05-06 着色玩具（综合）" #:width 400 #:height 400 #:draw draw))

(define prog
  (send canvas with-gl-context
    (lambda () (build-program (gl-vertex-shader vert-src) (gl-fragment-shader frag-src)))))
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

(send frame show #t)
