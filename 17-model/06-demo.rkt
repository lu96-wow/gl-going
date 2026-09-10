#lang racket/base
;; =========================================================
;; 17-model/06-demo.rkt —— 第六步：综合演示（相机环绕 + 暂停 + 切换）
;; 运行：racket 17-model/06-demo.rkt
;;   1/2 = cube / suzanne   Space = 暂停/继续旋转   ESC/点X = 退出
;; =========================================================
;; 本步无新语法，把前面几块拼起来（综合步）：
;;   模型导入（本课）      obj-load-file + mesh->vao
;;   光照（10 课）         N·L 漫反射
;;   轨道相机（08 课）     相机绕场缓转
;;   输入（08 课）         on-char 切模型 / 暂停
;;
;; 本步视觉：模型在原点自转，相机绕着它缓转。1/2 切换 cube / suzanne，
;;   对比"逐面法线（硬边）"与"平滑法线（圆润）"。Space 暂停自转 + 相机，
;;   方便你凑近看某一条棱。
;; =========================================================

(require "lib-gui.rkt")
(require "lib.rkt")
(require racket/runtime-path)

(define-runtime-path cube-obj "assets/cube.obj")
(define-runtime-path suz-obj  "assets/suzanne.obj")

(define PI (acos -1.0))
(define start-ms (current-inexact-milliseconds))
(define cur-model (box 1))        ; 1 = cube，2 = suzanne
(define paused? (box #f))
(define paused-at (box 0.0))      ; 暂停那一刻的 now
(define paused-acc (box 0.0))     ; 累计暂停掉的秒数

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
        (uniform vec3 uAlbedo)
        (out vec4 FragColor)
        (define (main) void
          (vec3 n (normalize vNormalW))
          (vec3 l (normalize (vec3 0.4 0.9 0.6)))
          (float diff (max (dot n l) 0.0))
          (vec3 ambient (* 0.22 (vec3 1.0)))
          (vec3 diffuse (* diff (vec3 0.85)))
          (set! FragColor (vec4 (* uAlbedo (+ ambient diffuse)) 1.0)))))

(define (mesh->vao verts idx)
  (define vao (u32vector-ref (glGenVertexArrays 1) 0))
  (glBindVertexArray vao)
  (define vbo (u32vector-ref (glGenBuffers 1) 0))
  (glBindBuffer GL_ARRAY_BUFFER vbo)
  (glBufferData GL_ARRAY_BUFFER (gl-vector-sizeof verts) verts GL_STATIC_DRAW)
  (glVertexAttribPointer 0 (glsl-size 'vec3) GL_FLOAT #f
                         (glsl-stride-bytes 'vec3 'vec3 'vec2) 0)
  (glEnableVertexAttribArray 0)
  (glVertexAttribPointer 1 (glsl-size 'vec3) GL_FLOAT #f
                         (glsl-stride-bytes 'vec3 'vec3 'vec2) (glsl-stride-bytes 'vec3))
  (glEnableVertexAttribArray 1)
  (define ebo (u32vector-ref (glGenBuffers 1) 0))
  (glBindBuffer GL_ELEMENT_ARRAY_BUFFER ebo)
  (glBufferData GL_ELEMENT_ARRAY_BUFFER (gl-vector-sizeof idx) idx GL_STATIC_DRAW)
  (glBindVertexArray 0)
  (values vao (u32vector-length idx)))

(define (on-char e)
  (define code (send e get-key-code))
  (when (not (eq? code 'release))
    (cond
      [(eq? code #\1) (set-box! cur-model 1) (printf "cube（硬边）~%")]
      [(eq? code #\2) (set-box! cur-model 2) (printf "suzanne（圆润）~%")]
      [(eq? code #\space)
       (define now (/ (- (current-inexact-milliseconds) start-ms) 1000.0))
       (if (unbox paused?)
           (begin (set-box! paused-acc (+ (unbox paused-acc) (- now (unbox paused-at))))
                  (set-box! paused? #f) (printf "继续~%"))
           (begin (set-box! paused-at now) (set-box! paused? #t) (printf "暂停~%")))]
      [(eq? code 'escape) (exit 0)])))

(define (draw)
  ;; 有效时间 = 真实时间 − 暂停掉的秒数
  (define t (- (/ (- (current-inexact-milliseconds) start-ms) 1000.0) (unbox paused-acc)))
  (define-values (w h) (send canvas get-gl-client-size))
  (define aspect (/ (exact->inexact w) (exact->inexact h)))
  (define P (m4-perspective 45.0 aspect 0.1 100.0))
  ;; 相机绕场缓转（08 课球坐标），模型在原点自转
  (define cam-rad (* (/ PI 180.0) (* t 18.0)))
  (define V (m4-look-at (* 4.5 (sin cam-rad)) 1.1 (* 4.5 (cos cam-rad))
                        0.0 0.0 0.0  0.0 1.0 0.0))
  (define M (m4-rot-y (* t 40.0)))

  (glClearColor 0.07 0.08 0.14 1.0)
  (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
  (glUseProgram prog)
  (glUniformMatrix4fv loc-model 1 #f (mat4 M))
  (glUniformMatrix4fv loc-mvp   1 #f (mat4 (m4-mult (m4-mult P V) M)))
  (glUniform3f loc-albedo 0.82 0.84 0.88)

  (define-values (vao cnt) (if (= (unbox cur-model) 1)
                               (values vao-cube cnt-cube)
                               (values vao-suz cnt-suz)))
  (glBindVertexArray vao)
  (glDrawElements GL_TRIANGLES cnt GL_UNSIGNED_INT 0))

(define-values (frame canvas)
  (make-window #:title "17-06 模型导入（综合）" #:width 800 #:height 600
               #:draw draw #:on-char on-char))

(define prog (send canvas with-gl-context (lambda () (build-program vert-src frag-src))))
(define loc-model  (send canvas with-gl-context (lambda () (glGetUniformLocation prog "uModel"))))
(define loc-mvp    (send canvas with-gl-context (lambda () (glGetUniformLocation prog "uMVP"))))
(define loc-albedo (send canvas with-gl-context (lambda () (glGetUniformLocation prog "uAlbedo"))))

(define cube-mesh (obj-load-file cube-obj))
(define suz-mesh  (obj-load-file suz-obj))
(define-values (vao-cube cnt-cube)
  (send canvas with-gl-context (lambda () (glEnable GL_DEPTH_TEST)
                                (mesh->vao (obj-mesh-verts cube-mesh) (obj-mesh-idx cube-mesh)))))
(define-values (vao-suz cnt-suz)
  (send canvas with-gl-context (lambda ()
                                (mesh->vao (obj-mesh-verts suz-mesh) (obj-mesh-idx suz-mesh)))))

(printf "按键：1 = cube  2 = suzanne  Space = 暂停  ESC = 退出~%")
(define ticker
  (new timer% (interval 16)
       (notify-callback (lambda () (send canvas refresh)))))

(send frame show #t)
(send canvas focus)
