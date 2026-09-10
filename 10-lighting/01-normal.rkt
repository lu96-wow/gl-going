#lang racket/base
;; =========================================================
;; 10-lighting/01-normal.rkt —— 第一步：法线（表面朝向）
;; 运行：racket 10-lighting/01-normal.rkt    点 X = 退出
;; =========================================================
;; 光照之前先认识"法线"——光照的一切都建立在它上面。
;;
;; 本步新增（2 个）：
;;   ① 法线 aNormal —— 每个顶点多一个"表面朝外方向"（vec3 单位向量）
;;   ② 世界空间变换 —— 法线要跟物体一起转（mat3(uModel)），光照方向才对齐
;;
;; ★法线是什么：一个点所在表面的"朝向"。光照问的第一个问题就是"这个面朝哪"，
;;   面朝光源就亮、背对就暗。立方体每个面法线固定朝外（+z/-z/+x/-x/+y/-y），
;;   因为每个面法线不同，角不能共享 → 6 面 × 4 顶点 = 24 顶点（和 07 课
;;   "每面颜色不同要拆面"同理）。
;;
;; ★为什么法线要乘 mat3(uModel)：物体旋转时，法线方向也跟着转（朝 +z 的面
;;   转 90° 后朝 +x）。法线是世界空间里算光照要用到的方向，所以顶点着色器
;;   里先用 mat3(uModel)（模型矩阵的 3×3 旋转部分）把它转到世界空间。
;;   （本课模型只有旋转+等比缩放，mat3 就够；非等比缩放才需要"逆转置"，
;;     那是更进阶的话题，本课先记住这个约定。）
;;
;; 本步视觉：把法线直接当颜色（normal*0.5+0.5 映射到 0..1）——每个面一种
;;   颜色（+x 红、+y 绿、+z 蓝……），立方体转起来颜色**跟着面走**，证明
;;   法线变换对了（如果法线没跟着转，面转过去颜色就错了）。
;; =========================================================

(require "lib-gui.rkt")
(require "lib.rkt")     ; cube-normal-verts / cube-normal-idx 在 lib 里

(define start-ms (current-inexact-milliseconds))

(define vert-src
  (glsl (version 330 core)
        (layout (location 0) in vec3 aPos)
        (layout (location 1) in vec3 aNormal)
        (uniform mat4 uModel)
        (uniform mat4 uMVP)
        (out vec3 vNormalW)
        (define (main) void
          (set! vNormalW (* (mat3 uModel) aNormal))   ; ★法线转世界空间（只取旋转）
          (set! gl_Position (* uMVP (vec4 aPos 1.0))))))

(define frag-src
  (glsl (version 330 core)
        (in vec3 vNormalW)
        (out vec4 FragColor)
        (define (main) void
          ;; 法线 -1..1 → 颜色 0..1：每个面一种颜色，直观看到"朝向"
          (set! FragColor (vec4 (+ (* vNormalW 0.5) (vec3 0.5)) 1.0)))))

(define (draw)
  (define t (/ (- (current-inexact-milliseconds) start-ms) 1000.0))
  (define-values (w h) (send canvas get-gl-client-size))
  (define aspect (/ (exact->inexact w) (exact->inexact h)))
  (define P (m4-perspective 45.0 aspect 0.1 100.0))
  (define V (m4-look-at 0.0 0.0 5.0  0.0 0.0 0.0  0.0 1.0 0.0))
  (define M (m4-mult (m4-rot-y (* t 40.0)) (m4-rot-x (* t 30.0))))

  (glClearColor 0.06 0.07 0.12 1.0)
  (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
  (glUseProgram prog)
  (glUniformMatrix4fv loc-model 1 #f (mat4 M))
  (glUniformMatrix4fv loc-mvp   1 #f (mat4 (m4-mult (m4-mult P V) M)))
  (glBindVertexArray vao)
  (glDrawElements GL_TRIANGLES 36 GL_UNSIGNED_SHORT 0))

(define-values (frame canvas)
  (make-window #:title "10-01 法线可视化" #:width 600 #:height 600 #:draw draw))

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
          (glVertexAttribPointer 0 (glsl-size 'vec3) GL_FLOAT #f (glsl-stride-bytes 'vec3 'vec3) 0)    ; 位置
          (glEnableVertexAttribArray 0)
          (glVertexAttribPointer 1 (glsl-size 'vec3) GL_FLOAT #f (glsl-stride-bytes 'vec3 'vec3) (glsl-stride-bytes 'vec3))   ; 法线
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
