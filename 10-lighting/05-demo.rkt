#lang racket/base
;; =========================================================
;; 10-lighting/05-demo.rkt —— 第五步：综合，光照场景
;; 运行：racket 10-lighting/05-demo.rkt    点 X = 退出
;; =========================================================
;; 本课前四步：法线(01)、Lambert(02)、Blinn-Phong(03)、模型谱系(04)。
;; 本步**不引入新语法**，把老教程 09-light-phong 的成品拼出来。
;;
;; 场景：中央翻滚的灰白立方体 + 三颗绕行的彩色小立方体 + 绕圈的点光源，
;;   相机缓缓环绕。光照全部在世界空间、逐片元计算（03 步的 Blinn-Phong）。
;;   光源位置每帧上传——记住：光源也是"数据"，和物体一样可以动。
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
        (out vec3 vFragW)
        (define (main) void
          (set! vNormalW (* (mat3 uModel) aNormal))
          (vec4 wp (* uModel (vec4 aPos 1.0)))
          (set! vFragW (xyz wp))
          (set! gl_Position (* uMVP (vec4 aPos 1.0))))))

(define frag-src
  (glsl (version 330 core)
        (in vec3 vNormalW)
        (in vec3 vFragW)
        (uniform vec3 uAlbedo)
        (uniform vec3 uLightPos)
        (uniform vec3 uLightColor)
        (uniform vec3 uViewPos)
        (out vec4 FragColor)
        (define (main) void
          (vec3 n (normalize vNormalW))
          (vec3 l (normalize (- uLightPos vFragW)))
          (vec3 v (normalize (- uViewPos vFragW)))
          (vec3 h (normalize (+ l v)))
          (float diff (max (dot n l) 0.0))
          (float spec (pow (max (dot n h) 0.0) 64.0))
          (vec3 ambient (* 0.15 uLightColor))
          (vec3 diffuse (* diff uLightColor))
          (vec3 specular (* (* spec uLightColor) 0.8))
          (set! FragColor (vec4 (+ (* uAlbedo (+ ambient diffuse)) specular) 1.0)))))

(define (draw)
  (define t (/ (- (current-inexact-milliseconds) start-ms) 1000.0))
  (define-values (w h) (send canvas get-gl-client-size))
  (define aspect (/ (exact->inexact w) (exact->inexact h)))
  (define P (m4-perspective 45.0 aspect 0.1 100.0))
  ;; 相机缓缓环绕
  (define cam-rad (* (/ PI 180.0) (* t 18.0)))
  (define V (m4-look-at (* 8.0 (sin cam-rad)) 3.0 (* 8.0 (cos cam-rad))
                        0.0 0.0 0.0  0.0 1.0 0.0))
  ;; 点光源绕圈
  (define la (* (/ PI 180.0) (* t 70.0)))

  (glClearColor 0.06 0.07 0.12 1.0)
  (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
  (glUseProgram prog)
  ;; 每帧上传的光照参数
  (glUniform3f loc-view (f64vector-ref V 12) (f64vector-ref V 13) (f64vector-ref V 14))
  (glUniform3f loc-lcol 1.0 0.96 0.85)
  (glUniform3f loc-lpos (* 4.5 (cos la)) 3.2 (* 4.5 (sin la)))

  (define (draw-cube m r g b)
    (glUniformMatrix4fv loc-model 1 #f (mat4 m))
    (glUniformMatrix4fv loc-mvp   1 #f (mat4 (m4-mult (m4-mult P V) m)))
    (glUniform3f loc-albedo r g b)
    (glBindVertexArray vao)
    (glDrawElements GL_TRIANGLES 36 GL_UNSIGNED_SHORT 0))

  ;; 中央翻滚的灰白立方体（缩到 0.7）
  (draw-cube (m4-mult (m4-mult (m4-rot-y (* t 50.0)) (m4-rot-x (* t 40.0)))
                      (m4-scale 0.7 0.7 0.7))
             0.82 0.84 0.90)
  ;; 三颗彩色小立方体绕行
  (for ([k (in-range 3)])
    (define a (* (/ PI 180.0) (+ (* k 120.0) (* t 90.0))))
    (define m (m4-mult (m4-translate (* 2.7 (cos a)) 0.6 (* 2.7 (sin a)))
                       (m4-mult (m4-rot-y (* t -90.0)) (m4-scale 0.45 0.45 0.45))))
    (draw-cube m (list-ref '(0.95 0.3 0.3) k)
               (list-ref '(0.3 0.9 0.4) k)
               (list-ref '(0.3 0.6 0.95) k))))

(define-values (frame canvas)
  (make-window #:title "10-05 光照场景（综合）" #:width 800 #:height 600 #:draw draw))

(define prog (send canvas with-gl-context (lambda () (build-program vert-src frag-src))))
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
(send canvas focus)
