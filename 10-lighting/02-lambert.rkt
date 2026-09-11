#lang racket/base
;; =========================================================
;; 10-lighting/02-lambert.rkt —— 第二步：Lambert 余弦定律（漫反射）
;; 运行：racket 10-lighting/02-lambert.rkt    点 X = 退出
;; =========================================================
;; 上一步：认识了法线。本步讲光照最核心的一条物理定律——**Lambert 余弦定律**，
;; 它解释了"为什么面与光的夹角决定亮度"。
;;
;; 本步新增（1 个）：
;;   Lambert 余弦定律 N·L —— 亮度 ∝ cos(法线与光线的夹角) = N·L
;;
;; ★原理（为什么光照能看出形状）：
;;   想象一束手电光正对着墙（垂直照），光斑小、能量集中、很亮；把手电斜着
;;   照，同样的能量铺到一个更大的椭圆光斑上 → 单位面积分到的能量变少 → 变暗。
;;   数学上：单位面积能量 = 正射能量 × cos θ，其中 θ 是"表面法线 N"和
;;   "指向光源的方向 L"的夹角。两个都是单位向量时，点积 dot(N,L) 正好 = cos θ。
;;   所以：亮度 = max(N·L, 0)。正对（θ=0，N·L=1）最亮；擦边（θ=90°，N·L=0）
;;   全黑；背对（N·L<0）用 max 夹成 0（不夹会变"负亮度"，越背光越黑，反了）。
;;   这就是"漫反射"——光均匀散射到各个方向，你从哪个角度看亮度都一样，只跟
;;   面的朝向有关。立方体六个面朝向不同 → N·L 不同 → 六个面明暗不同 → 立体感。
;;
;; 本步视觉：灰度 = 亮度。朝光的面亮、背光的面黑、侧面半亮。转起来能看到
;;   每面亮度随它转到不同朝向而连续变化。
;; =========================================================

(require "lib-gui.rkt")
(require "lib.rkt")

(define start-ms (current-inexact-milliseconds))

(define vert-src
  (glsl (version 330 core)
        (layout (location 0) in vec3 aPos)
        (layout (location 1) in vec3 aNormal)
        (uniform mat4 uModel)
        (uniform mat4 uMVP)
        (out vec3 vNormalW)
        (define (main) void
          (set! vNormalW (* (mat3 uModel) aNormal))
          (set! gl_Position (* uMVP (vec4 aPos 1.0))))))

(define frag-src
  (glsl (version 330 core)
        (in vec3 vNormalW)
        (out vec4 FragColor)
        (define (main) void
          (vec3 n (normalize vNormalW))             ; 法线（单位化，点积才有意义）
          (vec3 l (normalize (vec3 0.5 0.8 0.6)))   ; ★指向光源的方向（本步写死，下一步可动）
          (float diff (max (dot n l) 0.0))          ; ★N·L，夹掉背光
          (set! FragColor (vec4 (vec3 diff) 1.0))))) ; 灰度 = 亮度

(define (draw)
  (define t (/ (- (current-inexact-milliseconds) start-ms) 1000.0))
  (define-values (w h) (send canvas get-gl-client-size))
  (define aspect (/ (exact->inexact w) (exact->inexact h)))
  (define P (mat4-perspective 45.0 aspect 0.1 100.0))
  (define V (mat4-look-at 0.0 0.0 5.0  0.0 0.0 0.0  0.0 1.0 0.0))
  (define M (mat4-mult (mat4-rot-y (* t 40.0)) (mat4-rot-x (* t 30.0))))

  (glClearColor 0.06 0.07 0.12 1.0)
  (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
  (glUseProgram prog)
  (glUniformMatrix4fv loc-model 1 #f M)
  (glUniformMatrix4fv loc-mvp   1 #f (mat4-mult (mat4-mult P V) M))
  (glBindVertexArray vao)
  (glDrawElements GL_TRIANGLES 36 GL_UNSIGNED_SHORT 0))

(define-values (frame canvas)
  (make-window #:title "10-02 Lambert 余弦定律" #:width 600 #:height 600 #:draw draw))

(define prog (send canvas with-gl-context (lambda () (build-program vert-src frag-src))))
(define loc-model (send canvas with-gl-context (lambda () (glGetUniformLocation prog "uModel"))))
(define loc-mvp   (send canvas with-gl-context (lambda () (glGetUniformLocation prog "uMVP"))))
(define vao
  (send canvas with-gl-context
        (lambda ()
          (glEnable GL_DEPTH_TEST)
          (define vbo (u32vector-ref (glGenBuffers 1) 0))
          (glBindBuffer GL_ARRAY_BUFFER vbo)
          (glBufferData GL_ARRAY_BUFFER (gl-vector-sizeof cube-normal-verts) cube-normal-verts GL_STATIC_DRAW)
          (define v (u32vector-ref (glGenVertexArrays 1) 0))
          (glBindVertexArray v)
          (glVertexAttribPointer 0 (glsl-size 'vec3) GL_FLOAT #f (glsl-stride-bytes 'vec3 'vec3) 0)
          (glEnableVertexAttribArray 0)
          (glVertexAttribPointer 1 (glsl-size 'vec3) GL_FLOAT #f (glsl-stride-bytes 'vec3 'vec3) (glsl-stride-bytes 'vec3))
          (glEnableVertexAttribArray 1)
          (define ebo (u32vector-ref (glGenBuffers 1) 0))
          (glBindBuffer GL_ELEMENT_ARRAY_BUFFER ebo)
          (glBufferData GL_ELEMENT_ARRAY_BUFFER (gl-vector-sizeof cube-normal-idx) cube-normal-idx GL_STATIC_DRAW)
          (glBindVertexArray 0)
          v)))

(define ticker
  (new timer% (interval 16)
       (notify-callback (lambda () (send canvas refresh)))))

(send frame show #t)
