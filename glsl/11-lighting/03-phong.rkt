#lang racket/base
;; =========================================================
;; 11-lighting/03-phong.rkt —— 第三步：Blinn-Phong 三件套
;; 运行：racket glsl/11-lighting/03-phong.rkt    点 X = 退出
;; =========================================================
;; 上一步：漫反射（N·L）。本步凑齐经典光照模型的三项：环境光 + 漫反射 + 高光。
;;
;; 本步新增（2 个）：
;;   ① 环境光 ambient —— 常数项，近似"间接光"
;;   ② 镜面高光 specular —— 半向量 h = normalize(l+v)，pow 控制光斑锐度
;;
;; ★三项各解决什么（这是理解光照模型的关键）：
;;   ambient（环境光）= 常数 × 光色
;;     真实世界光会到处反弹（间接光），背光面也不是全黑。精确算反弹很贵，
;;     所以用一个常数近似，让背光面不至于全黑。它是"假"的，但便宜好用。
;;   diffuse（漫反射）= max(N·L,0) × 光色     ← 02 步，看形状
;;   specular（镜面高光）= pow(max(N·H,0), 64) × 光色
;;     光滑表面会像镜子一样反射光，当反射方向对准你的眼睛时，看到亮斑。
;;     算"反射方向是否对准视线"有两条路（下一步专门讲）：
;;       本步用 Blinn-Phong 的半向量 h = normalize(l + v)（光线+视线方向，
;;        角平分线），用 N·H 近似"反射是否对准视线"。pow 的指数 = 锐度：
;;        64 = 小而亮的光斑，8 = 大而柔（可自己改 64 试）。
;;
;; 本步视觉：灰白色立方体 + 绕圈的点光源。你能看到：朝光的面亮、背光面靠
;;   环境光撑着不全黑、正对反射方向时有一小块亮斑跟着光源转。
;; =========================================================

(require "lib-gui.rkt")
(require "lib.rkt")

(define PI (acos -1.0))
(define start-ms (current-inexact-milliseconds))

(define vert-src
  (glsl (version 330 core)
        (layout (location 0) in vec3 aPos)
        (layout (location 1) in vec3 aNormal)
        (uniform mat4 uModel)
        (uniform mat4 uMVP)
        (out vec3 vNormalW)
        (out vec3 vFragW)                          ; ★世界坐标（光照要在世界空间算方向）
        (define (main) void
          (set! vNormalW (* (mat3 uModel) aNormal))
          (vec4 wp (* uModel (vec4 aPos 1.0)))
          (set! vFragW (xyz wp))
          (set! gl_Position (* uMVP (vec4 aPos 1.0))))))

(define frag-src
  (glsl (version 330 core)
        (in vec3 vNormalW)
        (in vec3 vFragW)
        (uniform vec3 uAlbedo)     ; 物体底色
        (uniform vec3 uLightPos)   ; 点光源位置（世界）
        (uniform vec3 uLightColor) ; 光色
        (uniform vec3 uViewPos)    ; 相机位置（世界）
        (out vec4 FragColor)
        (define (main) void
          (vec3 n (normalize vNormalW))
          (vec3 l (normalize (- uLightPos vFragW)))   ; 指向光源
          (vec3 v (normalize (- uViewPos vFragW)))    ; 指向相机
          (vec3 h (normalize (+ l v)))                ; ★半向量（Blinn-Phong）
          (float diff (max (dot n l) 0.0))            ; 漫反射
          (float spec (pow (max (dot n h) 0.0) 64.0)) ; 高光
          (vec3 ambient (* 0.15 uLightColor))         ; 环境光
          (vec3 diffuse (* diff uLightColor))
          (vec3 specular (* (* spec uLightColor) 0.8))
          (set! FragColor (vec4 (+ (* uAlbedo (+ ambient diffuse)) specular) 1.0)))))

(define (draw)
  (define t (/ (- (current-inexact-milliseconds) start-ms) 1000.0))
  (define-values (w h) (send canvas get-gl-client-size))
  (define aspect (/ (exact->inexact w) (exact->inexact h)))
  (define P (mat4-perspective 45.0 aspect 0.1 100.0))
  (define V (mat4-look-at 0.0 0.0 5.0  0.0 0.0 0.0  0.0 1.0 0.0))
  (define M (mat4-rot-y (* t 30.0)))

  ;; 点光源绕圈
  (define la (* (/ PI 180.0) (* t 70.0)))
  (define lx (* 4.5 (cos la)))
  (define lz (* 4.5 (sin la)))

  (glClearColor 0.06 0.07 0.12 1.0)
  (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
  (glUseProgram prog)
  (glUniformMatrix4fv loc-model 1 #f M)
  (glUniformMatrix4fv loc-mvp   1 #f (mat4-mult (mat4-mult P V) M))
  (glUniform3f loc-albedo 0.82 0.84 0.90)
  (glUniform3f loc-lcol 1.0 0.96 0.85)
  (glUniform3f loc-lpos lx 3.2 lz)
  (glUniform3f loc-view 0.0 0.0 5.0)
  (glBindVertexArray vao)
  (glDrawElements GL_TRIANGLES 36 GL_UNSIGNED_SHORT 0))

(define-values (frame canvas)
  (make-window #:title "11-03 Blinn-Phong 光照" #:width 600 #:height 600 #:draw draw))

(define prog (send canvas with-gl-context (lambda () (build-program (GL_VERTEX_SHADER vert-src) (GL_FRAGMENT_SHADER frag-src)))))
(define loc-model  (send canvas with-gl-context (lambda () (glGetUniformLocation prog "uModel"))))
(define loc-mvp    (send canvas with-gl-context (lambda () (glGetUniformLocation prog "uMVP"))))
(define loc-albedo (send canvas with-gl-context (lambda () (glGetUniformLocation prog "uAlbedo"))))
(define loc-lpos   (send canvas with-gl-context (lambda () (glGetUniformLocation prog "uLightPos"))))
(define loc-lcol   (send canvas with-gl-context (lambda () (glGetUniformLocation prog "uLightColor"))))
(define loc-view   (send canvas with-gl-context (lambda () (glGetUniformLocation prog "uViewPos"))))
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
