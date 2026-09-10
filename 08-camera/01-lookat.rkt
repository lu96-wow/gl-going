#lang racket/base
;; =========================================================
;; 08-camera/01-lookat.rkt —— 第一步：视图矩阵 m4-look-at
;; 运行：racket 08-camera/01-lookat.rkt    点 X = 退出
;; =========================================================
;; 07 课的相机是写死的"把世界往后推 6 格"。本步换真正的相机：视图矩阵。
;;
;; 本步新增（2 个）：
;;   ① m4-look-at —— 由"相机位置 + 看向目标 + 上方向"生成视图矩阵 V
;;   ② 网格地面 —— GL_LINES 画 XZ 平面，让相机朝向一眼可见
;;
;; ★核心心智：相机不动，世界动。想从"相机在 (5,3,5)、看向原点"的视角看世界，
;;   等价于把整个世界做一次相反的变换（旋转 + 平移），把相机"搬"到原点、朝向 -z。
;;   这个"相反变换"就是视图矩阵 V。之后 gl_Position = P · V · M · v，
;;   场景代码（立方体）完全不知道相机在哪——CPU 只改 V，GPU 无脑乘。
;;
;; ★lookAt 怎么拼（裸写，本步主角）：用三个互相垂直的基向量描述相机朝向——
;;   f = 前方向 = normalize(目标 - 眼睛)
;;   s = 右方向 = normalize(f × up)
;;   u = 上方向 = s × f（重新正交化，避免 f 和 up 不垂直时的歪斜）
;;   旋转部分 = 这三个基向量；平移部分 = 眼睛位置的投影取负。合起来就是 V。
;; =========================================================

(require "lib-gui.rkt")
(require "lib.rkt")

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

;; 视图矩阵（裸写，本步主角）：eye=(ex,ey,ez)，center=(cx,cy,cz)，up=(ux,uy,uz)
(define (m4-look-at ex ey ez cx cy cz ux uy uz)
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
  ;; u = s × f —— 重新正交化后的上方向
  (define uxx (- (* syy fzz) (* szz fyy)))
  (define uyy (- (* szz fxx) (* sxx fzz)))
  (define uzz (- (* sxx fyy) (* syy fxx)))
  (f64vector sxx uxx (- fxx) 0.0
             syy uyy (- fyy) 0.0
             szz uzz (- fzz) 0.0
             (- (+ (* sxx ex) (* syy ey) (* szz ez)))
             (- (+ (* uxx ex) (* uyy ey) (* uzz ez)))
             (+ (* fxx ex) (* fyy ey) (* fzz ez))
             1.0))

;; 网格地面：XZ 平面（y=0）上一堆 GL_LINES，从 -span 到 span 每 step 一条。
(define (grid-verts span step)
  (define color (vec3 0.30 0.32 0.50))
  (apply concat-vecs
         (apply append
                (for/list ([s (in-range (- span) (+ span step) step)])
                  (list (vec3 (- span) 0.0 s) color    ; 沿 x 的线
                        (vec3 span 0.0 s) color
                        (vec3 s 0.0 (- span)) color    ; 沿 z 的线
                        (vec3 s 0.0 span) color)))))
(define grid (grid-verts 5.0 0.5))
(define grid-count (quotient (f32vector-length grid) 6))

(define (draw)
  (define t (/ (- (current-inexact-milliseconds) start-ms) 1000.0))
  (define-values (w h) (send canvas get-gl-client-size))
  (define aspect (/ (exact->inexact w) (exact->inexact h)))
  (define P (m4-perspective 45.0 aspect 0.1 100.0))
  ;; ★固定相机：站在 (5,3,5)，看向原点，上方向 (0,1,0)
  (define V (m4-look-at 5.0 3.0 5.0  0.0 0.0 0.0  0.0 1.0 0.0))

  (glClearColor 0.07 0.08 0.14 1.0)
  (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
  (glUseProgram prog)

  ;; 地面网格（模型矩阵 = 单位阵，直接 P·V）
  (glUniformMatrix4fv loc-mvp 1 #f (mat4 (m4-mult P V)))
  (glBindVertexArray vao-grid)
  (glDrawArrays GL_LINES 0 grid-count)

  ;; 中央翻滚立方体
  (define M (m4-mult (m4-translate 0.0 1.0 0.0)
                     (m4-mult (m4-mult (m4-rot-y (* t 60.0)) (m4-rot-x (* t 40.0)))
                              (m4-scale 0.8 0.8 0.8))))
  (glUniformMatrix4fv loc-mvp 1 #f (mat4 (m4-mult (m4-mult P V) M)))
  (glBindVertexArray vao-cube)
  (glDrawElements GL_TRIANGLES 36 GL_UNSIGNED_SHORT 0))

(define-values (frame canvas)
  (make-window #:title "08-01 视图矩阵" #:width 600 #:height 600 #:draw draw))

(define prog (send canvas with-gl-context (lambda () (build-program vert-src frag-src))))
(define loc-mvp (send canvas with-gl-context (lambda () (glGetUniformLocation prog "uMVP"))))

;; 网格 VAO（GL_LINES，位置+颜色）
(define vao-grid
  (send canvas with-gl-context
        (lambda ()
          (glEnable GL_DEPTH_TEST)
          (define vbo (u32vector-ref (glGenBuffers 1) 0))
          (glBindBuffer GL_ARRAY_BUFFER vbo)
          (glBufferData GL_ARRAY_BUFFER (gl-vector-sizeof grid) grid GL_STATIC_DRAW)
          (define v (u32vector-ref (glGenVertexArrays 1) 0))
          (glBindVertexArray v)
          (glVertexAttribPointer 0 (glsl-size 'vec3) GL_FLOAT #f (glsl-stride-bytes 'vec3 'vec3) 0)
          (glEnableVertexAttribArray 0)
          (glVertexAttribPointer 1 (glsl-size 'vec3) GL_FLOAT #f (glsl-stride-bytes 'vec3 'vec3) (glsl-stride-bytes 'vec3))
          (glEnableVertexAttribArray 1)
          (glBindVertexArray 0)
          v)))

;; 立方体 VAO（复用 07 课的 cube-verts/cube-idx）
(define vao-cube
  (send canvas with-gl-context
        (lambda ()
          (define vbo (u32vector-ref (glGenBuffers 1) 0))
          (glBindBuffer GL_ARRAY_BUFFER vbo)
          (glBufferData GL_ARRAY_BUFFER (gl-vector-sizeof cube-verts) cube-verts GL_STATIC_DRAW)
          (define v (u32vector-ref (glGenVertexArrays 1) 0))
          (glBindVertexArray v)
          (glVertexAttribPointer 0 (glsl-size 'vec3) GL_FLOAT #f (glsl-stride-bytes 'vec3 'vec3) 0)
          (glEnableVertexAttribArray 0)
          (glVertexAttribPointer 1 (glsl-size 'vec3) GL_FLOAT #f (glsl-stride-bytes 'vec3 'vec3) (glsl-stride-bytes 'vec3))
          (glEnableVertexAttribArray 1)
          (define ebo (u32vector-ref (glGenBuffers 1) 0))
          (glBindBuffer GL_ELEMENT_ARRAY_BUFFER ebo)
          (glBufferData GL_ELEMENT_ARRAY_BUFFER (gl-vector-sizeof cube-idx) cube-idx GL_STATIC_DRAW)
          (glBindVertexArray 0)
          v)))

(define ticker
  (new timer% (interval 16)
       (notify-callback (lambda () (send canvas refresh)))))

(send frame show #t)
