#lang racket/base
;; =========================================================
;; 18-moss/01-noise.rkt —— 第一步：斑驳 + 毛绒质感（噪声）
;; 运行：racket 18-moss/01-noise.rkt    点 X = 退出
;; =========================================================
;; 目标：把 17 课的立方体变成"蓝色苔藓"。本步先做苔藓的两个特征：
;;   **斑驳**（颜色深浅不一）+ **毛绒质感**（表面毛乎乎、边缘发亮）。
;;
;; 本步新增（2 个，同属"程序化纹理"这一件事）：
;;   ① hash 噪声 + FBM —— 用数学生成"自然随机"的纹理，不用图片
;;   ② 噪声扰动法线 + Fresnel —— 模拟绒毛的粗糙散射与边缘透光
;;
;; ★为什么不用图片纹理：苔藓的斑驳是"连续、无重复、每个面都不一样"的
;;   随机图案。手画一张图会重复、会拼接缝。程序化噪声是**一个数学函数**
;;   p → 0..1，输入表面坐标、输出明暗——03/04 课 shadertoy 用的 sin/fract
;;   就是它的雏形。本步用 shadertoy 圈子里最经典的"hash 噪声 + FBM"：
;;     hash(p) = fract(sin(dot(p, 大数)) * 更大数)   —— 把坐标搅成伪随机
;;     noise   = 对 8 个格子角点的 hash 做三线性插值  —— 连续、不跳跃
;;     fbm     = 把 noise 按 2 倍频率叠 4 层           —— 细节 + 大结构都有
;;
;; ★毛绒质感从哪来（还不是一根根毛）：
;;   - 噪声扰动法线：绒毛朝四面八方乱翘，光照方向被"抖"一下 → 表面不再
;;     光滑，而是毛乎乎的漫反射。
;;   - Fresnel 边缘光：绒毛在物体边缘会透光/反光，所以越侧着看越亮——
;;     用 pow(1 - N·V, k) 模拟（10 课 N·V 的近亲）。
;; =========================================================

(require "lib-gui.rkt")
(require "lib.rkt")
(require racket/runtime-path)

(define-runtime-path cube-obj "assets/cube.obj")

(define PI (acos -1.0))
(define start-ms (current-inexact-milliseconds))

;; 顶点着色器：多输出一个 vFragW（世界坐标）——噪声要用"表面在 3D 空间
;; 的哪里"当输入，这样苔藓连续地长在六个面上，而不是每个面重复同样图案。
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

;; 片元着色器：
;;   噪声函数是纯 GLSL 数学，用 (raw "...") 逃逸口直接写原文（DSL 覆盖
;;   不了的大段数学，raw 就是干这个的）；main 里的材质逻辑用 DSL 写。
;;   ★注意：raw 里不能写中文注释（Mesa 的 GLSL 编译器会报错），解释都放
;;   在 raw 外面的 ;; 注释里。hash 故意不用 sin（sin 在软件渲染 llvmpipe 下
;;   每像素算几百次会直接卡死），改用无三角函数的整数 hash。
(define frag-src
  (glsl (version 330 core)
        (in vec3 vNormalW)
        (in vec3 vFragW)
        (uniform vec3 uViewPos)
        (out vec4 FragColor)
        (raw "
float hash(vec3 p) {
  p = fract(p * 0.3183099 + 0.1);
  p *= 17.0;
  return fract(p.x * p.y * p.z * (p.x + p.y + p.z));
}
float noise(vec3 p) {
  vec3 i = floor(p); vec3 f = fract(p);
  vec3 u = f * f * (3.0 - 2.0 * f);
  return mix(mix(mix(hash(i),            hash(i + vec3(1,0,0)), u.x),
                 mix(hash(i + vec3(0,1,0)), hash(i + vec3(1,1,0)), u.x), u.y),
             mix(mix(hash(i + vec3(0,0,1)), hash(i + vec3(1,0,1)), u.x),
                 mix(hash(i + vec3(0,1,1)), hash(i + vec3(1,1,1)), u.x), u.y), u.z);
}
float fbm(vec3 p) {
  float v = 0.0; float a = 0.5;
  for (int i = 0; i < 3; i++) { v += a * noise(p); p *= 2.0; a *= 0.5; }
  return v;
}")
        (define (main) void
          ;; ① 斑驳：fbm 值 0..1，调制蓝色 albedo（深蓝 → 亮蓝）
          (float m (fbm (* vFragW 2.5)))
          (vec3 albedo (mix (vec3 0.05 0.12 0.25) (vec3 0.20 0.55 1.0) m))
          ;; ② 毛绒：噪声梯度扰动法线（只采样 x/y 两个方向，省一半采样）
          (float e 0.08)
          (float dx (- (fbm (+ vFragW (vec3 e 0.0 0.0))) (fbm (- vFragW (vec3 e 0.0 0.0)))))
          (float dy (- (fbm (+ vFragW (vec3 0.0 e 0.0))) (fbm (- vFragW (vec3 0.0 e 0.0)))))
          (vec3 n (normalize (+ (normalize vNormalW) (* 0.5 (vec3 dx dy 0.0)))))
          ;; ③ 光照：N·L 漫反射 + 环境光（10 课）
          (vec3 l (normalize (vec3 0.4 0.9 0.6)))
          (float diff (max (dot n l) 0.0))
          ;; ④ Fresnel 边缘光：绒毛边缘透光，越侧越亮
          (vec3 v (normalize (- uViewPos vFragW)))
          (float fres (pow (- 1.0 (max (dot n v) 0.0)) 3.0))
          (vec3 col (+ (* albedo (+ (* 0.15 (vec3 1.0)) (* diff (vec3 0.85))))
                       (* 0.35 (vec3 0.5 0.8 1.0) fres)))
          (set! FragColor (vec4 col 1.0)))))

;; ② 上传：obj-mesh → VAO（17 课 05 步同款）
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

(define (draw)
  (define t (/ (- (current-inexact-milliseconds) start-ms) 1000.0))
  (define-values (w h) (send canvas get-gl-client-size))
  (define aspect (/ (exact->inexact w) (exact->inexact h)))
  (define P (m4-perspective 45.0 aspect 0.1 100.0))
  (define V (m4-translate 0.0 0.0 -3.0))      ; 相机在 z=+3
  (define M (m4-rot-y (* t 40.0)))            ; 模型自转

  (glClearColor 0.07 0.08 0.14 1.0)
  (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
  (glUseProgram prog)
  (glUniformMatrix4fv loc-model 1 #f (mat4 M))
  (glUniformMatrix4fv loc-mvp   1 #f (mat4 (m4-mult (m4-mult P V) M)))
  (glUniform3f loc-view 0.0 0.0 3.0)         ; 相机世界位置（Fresnel 用）
  (glBindVertexArray vao)
  (glDrawElements GL_TRIANGLES cnt GL_UNSIGNED_INT 0))

(define (on-char e)
  (define code (send e get-key-code))
  (when (or (eq? code 'escape) (eq? code #\q))
    (exit 0)))

(define-values (frame canvas)
  (make-window #:title "18-01 斑驳苔藓 + 毛绒质感" #:width 800 #:height 600
               #:draw draw #:on-char on-char))

(define prog (send canvas with-gl-context (lambda () (build-program vert-src frag-src))))
(define loc-model (send canvas with-gl-context (lambda () (glGetUniformLocation prog "uModel"))))
(define loc-mvp   (send canvas with-gl-context (lambda () (glGetUniformLocation prog "uMVP"))))
(define loc-view  (send canvas with-gl-context (lambda () (glGetUniformLocation prog "uViewPos"))))

(define mesh (obj-load-file cube-obj))
(printf "~a~%" (obj-mesh-summary mesh))
(define-values (vao cnt)
  (send canvas with-gl-context
        (lambda ()
          ;; 打印渲染器：llvmpipe = 软件渲染（CPU，帧率低）；其它 = 硬件 GPU（快）
          (printf "渲染器：~a~%" (glGetString GL_RENDERER))
          (glEnable GL_DEPTH_TEST)
          (mesh->vao (obj-mesh-verts mesh) (obj-mesh-idx mesh)))))

(define ticker
  (new timer% (interval 16)
       (notify-callback (lambda () (send canvas refresh)))))

(send frame show #t)
(send canvas focus)
