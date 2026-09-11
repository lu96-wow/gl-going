#lang racket/base
;; =========================================================
;; 07-3d-depth/01-cube.rkt —— 第一步：3D 顶点 + 立方体（线框）
;; 运行：racket 07-3d-depth/01-cube.rkt    点 X = 退出
;; =========================================================
;; 06 课所有顶点都是 2D 的 (vec2 aPos)。本步升到 3D，画第一个 3D 物体：立方体。
;;
;; 本步新增（2 个，同属"3D 数据"这一件事）：
;;   ① vec3 aPos —— 顶点位置多了一个 z（深度）
;;   ② 立方体线框 —— 8 个角 + 12 条边的索引（GL_LINES）
;;
;; ★立方体的数据：8 个角（每个 (x,y,z) ∈ {-1,1}），12 条边连接它们。
;;   用 EBO 存"边的两个端点编号"，glDrawElements(GL_LINES, ...) 一次画出。
;;   ★注意顶点着色器里的 (vec4 aPos 1.0)：3D 位置补 1.0（不是 06 课的
;;   0.0 1.0 两段），因为平移要作用在 w=1 上，z 也要跟着矩阵走。
;;
;; ★"假相机"：顶点要落在 -z 方向才能被看见，所以先把世界往后推 6 个单位
;;   （V = 平移 (0,0,-6)），立方体仍放在原点。真正的相机 08 课讲 lookAt。
;;   本步用正交投影（06 课的 mat4-ortho，只是给 z 也设了范围）——透视下一步。
;;
;; ★线框不启用深度测试：这样背面的边也画得出来，立方体"透亮"、能看穿。
;;   （实心面就需要深度了——下一步。）
;; =========================================================

(require "lib-gui.rkt")
(require "lib.rkt")

(define start-ms (current-inexact-milliseconds))

(define vert-src
  (glsl (version 330 core)
        (layout (location 0) in vec3 aPos)
        (uniform mat4 uMVP)
        (define (main) void
          (set! gl_Position (* uMVP (vec4 aPos 1.0))))))

(define frag-src
  (glsl (version 330 core)
        (uniform vec3 uColor)
        (out vec4 FragColor)
        (define (main) void
          (set! FragColor (vec4 uColor 1.0)))))

;; 8 个角（单位立方体：每个坐标 ±1）
(define verts
  (concat-vecs (vec3 -1.0 -1.0 -1.0) (vec3  1.0 -1.0 -1.0)   ; 0 1 底后
               (vec3  1.0  1.0 -1.0) (vec3 -1.0  1.0 -1.0)   ; 2 3
               (vec3 -1.0 -1.0  1.0) (vec3  1.0 -1.0  1.0)   ; 4 5 底前
               (vec3  1.0  1.0  1.0) (vec3 -1.0  1.0  1.0))) ; 6 7

;; 12 条边 = 24 个端点编号（每边 2 个）：底面 4、顶面 4、竖边 4
(define idx
  (u16vector 0 1  1 2  2 3  3 0      ; 底面（z=-1）
             4 5  5 6  6 7  7 4      ; 顶面（z=+1）
             0 4  1 5  2 6  3 7))    ; 四条竖边

(define (draw)
  (define t (/ (- (current-inexact-milliseconds) start-ms) 1000.0))
  (define V (mat4-translate 0.0 0.0 -6.0))                 ; 假相机：世界往后推 6
  (define P (mat4-ortho -2.0 2.0 -2.0 2.0 0.1 100.0))      ; 正交投影
  (define M (mat4-rot-y (* t 40.0)))                       ; 绕 y 旋转
  (glClearColor 0.07 0.08 0.14 1.0)
  (glClear GL_COLOR_BUFFER_BIT)
  (glUseProgram prog)
  (glUniformMatrix4fv loc-mvp 1 #f (mat4-mult (mat4-mult P V) M))
  (glUniform3f loc-color 0.40 0.75 0.95)
  (glBindVertexArray vao)
  (glDrawElements GL_LINES 24 GL_UNSIGNED_SHORT 0))

(define-values (frame canvas)
  (make-window #:title "07-01 3D 立方体（线框）" #:width 400 #:height 400 #:draw draw))

(define prog (send canvas with-gl-context (lambda () (build-program vert-src frag-src))))
(define loc-mvp   (send canvas with-gl-context (lambda () (glGetUniformLocation prog "uMVP"))))
(define loc-color (send canvas with-gl-context (lambda () (glGetUniformLocation prog "uColor"))))
(define vao
  (send canvas with-gl-context
        (lambda ()
          (define vbo (u32vector-ref (glGenBuffers 1) 0))
          (glBindBuffer GL_ARRAY_BUFFER vbo)
          (glBufferData GL_ARRAY_BUFFER (gl-vector-sizeof verts) verts GL_STATIC_DRAW)
          (define v (u32vector-ref (glGenVertexArrays 1) 0))
          (glBindVertexArray v)
          (glVertexAttribPointer 0 (glsl-size 'vec3) GL_FLOAT #f (glsl-stride-bytes 'vec3) 0)   ; 每顶点 3 float = 12 字节
          (glEnableVertexAttribArray 0)
          (define ebo (u32vector-ref (glGenBuffers 1) 0))
          (glBindBuffer GL_ELEMENT_ARRAY_BUFFER ebo)
          (glBufferData GL_ELEMENT_ARRAY_BUFFER (gl-vector-sizeof idx) idx GL_STATIC_DRAW)
          (glBindVertexArray 0)
          v)))

(define ticker
  (new timer% (interval 16)
       (notify-callback (lambda () (send canvas refresh)))))

(send frame show #t)
