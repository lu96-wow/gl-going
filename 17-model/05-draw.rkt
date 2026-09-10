#lang racket/base
;; =========================================================
;; 17-model/05-draw.rkt —— 第五步：模型画出来了！（mesh → VAO + 光照）
;; 运行：racket 17-model/05-draw.rkt    1/2 = cube / suzanne   点X = 退出
;; =========================================================
;; 前 4 步把 OBJ 文件变成了 CPU 侧的数组。本步把它们灌进 GPU 画出来——
;; 这一步**没有任何新 GL 函数**：VBO/EBO/VAO 是 02/05/07 课学过的，
;; 光照是 10 课学过的。新东西只是"把 obj-mesh 的数组接上去"。
;;
;; ★三步流水线的后两步（raylib 的对应关系）：
;;   ② 上传 rlLoadMesh → mesh->vao：verts→VBO、idx→EBO、配 attribute
;;   ③ 绘制 rlDrawMesh  → glDrawElements（索引绘制，05 课 EBO 同款）
;;
;; ★顶点排布（8 float/顶点，stride = 32 字节）：
;;   offset 0  → aPos    (vec3)
;;   offset 12 → aNormal (vec3)     ← 本课光照用
;;   offset 24 → uv      (vec2)     ← 数据在 VBO 里，但本课不贴图所以不绑定
;;   注意 idx 是 u32vector → glDrawElements 用 GL_UNSIGNED_INT。
;;
;; 本步视觉：一个灰白色模型缓慢自转，固定方向光打亮表面。按 1/2 切换：
;;   cube.obj 的 vn 是**逐面法线** → 每个面一种明暗，棱角分明（硬边）
;;   suzanne.obj 的 vn 是**平滑法线** → 明暗连续过渡，圆润
;; =========================================================

(require "lib-gui.rkt")
(require "lib.rkt")
(require racket/runtime-path)

(define-runtime-path cube-obj "assets/cube.obj")
(define-runtime-path suz-obj  "assets/suzanne.obj")

(define PI (acos -1.0))
(define start-ms (current-inexact-milliseconds))
(define cur-model (box 1))   ; 1 = cube，2 = suzanne

;; 顶点着色器：法线只受旋转影响 → 乘 mat3(uModel)（10 课同款）
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

;; 片元着色器：固定方向光 + 漫反射 N·L（10 课同款，不贴图）
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

;; ② 上传：obj-mesh → VAO（raylib 的 rlLoadMesh 这一步）
(define (mesh->vao verts idx)
  (define vao (u32vector-ref (glGenVertexArrays 1) 0))
  (glBindVertexArray vao)
  (define vbo (u32vector-ref (glGenBuffers 1) 0))
  (glBindBuffer GL_ARRAY_BUFFER vbo)
  (glBufferData GL_ARRAY_BUFFER (gl-vector-sizeof verts) verts GL_STATIC_DRAW)
  ;; 8 float/顶点：pos(vec3) + normal(vec3) + uv(vec2)，stride 用工具算
  (glVertexAttribPointer 0 (glsl-size 'vec3) GL_FLOAT #f
                         (glsl-stride-bytes 'vec3 'vec3 'vec2) 0)
  (glEnableVertexAttribArray 0)
  (glVertexAttribPointer 1 (glsl-size 'vec3) GL_FLOAT #f
                         (glsl-stride-bytes 'vec3 'vec3 'vec2) (glsl-stride-bytes 'vec3))
  (glEnableVertexAttribArray 1)
  ;; uv 在 offset 24，本课不贴图所以不绑定 location 2
  (define ebo (u32vector-ref (glGenBuffers 1) 0))
  (glBindBuffer GL_ELEMENT_ARRAY_BUFFER ebo)
  (glBufferData GL_ELEMENT_ARRAY_BUFFER (gl-vector-sizeof idx) idx GL_STATIC_DRAW)
  (glBindVertexArray 0)
  (values vao (u32vector-length idx)))

(define (on-char e)
  (define code (send e get-key-code))
  (when (not (eq? code 'release))
    (cond
      [(eq? code #\1) (set-box! cur-model 1)
       (printf "模型：cube.obj（vn = 逐面法线 → 硬边）~%")]
      [(eq? code #\2) (set-box! cur-model 2)
       (printf "模型：suzanne.obj（vn = 平滑法线 → 圆润）~%")]
      [(eq? code 'escape) (exit 0)])))

(define (draw)
  (define t (/ (- (current-inexact-milliseconds) start-ms) 1000.0))
  (define-values (w h) (send canvas get-gl-client-size))
  (define aspect (/ (exact->inexact w) (exact->inexact h)))
  (define P (m4-perspective 45.0 aspect 0.1 100.0))
  (define V (m4-translate 0.0 0.0 -3.0))      ; 相机往后退 3 格
  (define M (m4-rot-y (* t 40.0)))            ; 模型自转

  (glClearColor 0.07 0.08 0.14 1.0)
  (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
  (glUseProgram prog)
  (glUniformMatrix4fv loc-model 1 #f (mat4 M))
  (glUniformMatrix4fv loc-mvp   1 #f (mat4 (m4-mult (m4-mult P V) M)))
  (glUniform3f loc-albedo 0.82 0.84 0.88)

  ;; ③ 绘制：切模型只换"绑哪个 VAO、画几个索引"
  (define-values (vao cnt) (if (= (unbox cur-model) 1)
                               (values vao-cube cnt-cube)
                               (values vao-suz cnt-suz)))
  (glBindVertexArray vao)
  (glDrawElements GL_TRIANGLES cnt GL_UNSIGNED_INT 0))

(define-values (frame canvas)
  (make-window #:title "17-05 模型导入" #:width 800 #:height 600
               #:draw draw #:on-char on-char))

(define prog (send canvas with-gl-context (lambda () (build-program vert-src frag-src))))
(define loc-model  (send canvas with-gl-context (lambda () (glGetUniformLocation prog "uModel"))))
(define loc-mvp    (send canvas with-gl-context (lambda () (glGetUniformLocation prog "uMVP"))))
(define loc-albedo (send canvas with-gl-context (lambda () (glGetUniformLocation prog "uAlbedo"))))

;; ① 解析（CPU）+ ② 上传（GPU）
(define cube-mesh (obj-load-file cube-obj))
(define suz-mesh  (obj-load-file suz-obj))
(printf "~a~%" (obj-mesh-summary cube-mesh))
(printf "~a~%" (obj-mesh-summary suz-mesh))
(define-values (vao-cube cnt-cube)
  (send canvas with-gl-context
        (lambda ()
          (glEnable GL_DEPTH_TEST)
          (mesh->vao (obj-mesh-verts cube-mesh) (obj-mesh-idx cube-mesh)))))
(define-values (vao-suz cnt-suz)
  (send canvas with-gl-context
        (lambda ()
          (mesh->vao (obj-mesh-verts suz-mesh) (obj-mesh-idx suz-mesh)))))

(define ticker
  (new timer% (interval 16)
       (notify-callback (lambda () (send canvas refresh)))))

(send frame show #t)
(send canvas focus)
