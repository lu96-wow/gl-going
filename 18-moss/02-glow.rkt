#lang racket/base
;; =========================================================
;; 18-moss/02-glow.rkt —— 第二步：发光（emissive 自发光）
;; 运行：racket 18-moss/02-glow.rkt    ESC/Q = 退出   点X = 退出
;; =========================================================
;; 上一步：斑驳的蓝色苔藓 + 毛绒质感。本步让它**自己发光**。
;;
;; 本步新增（1 个概念）：
;;   emissive（自发光）—— 材质自己发出的光，直接"加"进最终颜色，不乘光照
;;
;; ★发光和前面光照的区别（这是理解"发光"的关键）：
;;   之前所有颜色都来自"反射外部光"：albedo × 光照。背光面黑、朝光面亮。
;;   emissive 是**材质自己发光**，和外部光照无关——就算环境全黑，它也是
;;   亮的。公式上就是最终颜色额外 + 一项：
;;
;;     颜色 = albedo × (环境 + 漫反射) + Fresnel + emissive
;;                                    ↑ 光照部分        ↑ 自己发的光
;;
;; ★但要注意一个重要的"做不到"：emissive 只让苔藓**自己看起来亮**，
;;   它**不会照亮周围的物体**（蓝光不会洒到旁边的地面上）。真实的"光照
;;   亮环境"是全局光照，离线渲染才认真算。实时渲染的补救 = 03 步的 bloom
;;   （让亮部在屏幕上晕开一圈，制造"发光"的视觉错觉）。
;;
;; 本步视觉：苔藓斑驳地发出蓝光——用另一个频率的噪声控制发光强度，
;;   有些苔藓点更亮、有些暗，像有生命的发光菌斑。
;; =========================================================

(require "lib-gui.rkt")
(require "lib.rkt")
(require racket/runtime-path)

(define-runtime-path cube-obj "assets/cube.obj")

(define PI (acos -1.0))
(define start-ms (current-inexact-milliseconds))

;; 顶点着色器：同 01（输出 vNormalW 世界法线 + vFragW 世界坐标）
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

;; 片元着色器：01 的苔藓 + 本步的 emissive。
;;   hash 用无 sin 整数版（软件渲染 llvmpipe 下 sin 会卡死）；raw 里不写
;;   中文注释（Mesa 会报错），解释都在 raw 外面的 ;; 里。
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
          ;; ① 斑驳 albedo（01 步）
          (float m (fbm (* vFragW 2.5)))
          (vec3 albedo (mix (vec3 0.05 0.12 0.25) (vec3 0.20 0.55 1.0) m))
          ;; ② 毛绒：噪声梯度扰动法线（01 步）
          (float e 0.08)
          (float dx (- (fbm (+ vFragW (vec3 e 0.0 0.0))) (fbm (- vFragW (vec3 e 0.0 0.0)))))
          (float dy (- (fbm (+ vFragW (vec3 0.0 e 0.0))) (fbm (- vFragW (vec3 0.0 e 0.0)))))
          (vec3 n (normalize (+ (normalize vNormalW) (* 0.5 (vec3 dx dy 0.0)))))
          ;; ③ 光照（10 课）
          (vec3 l (normalize (vec3 0.4 0.9 0.6)))
          (float diff (max (dot n l) 0.0))
          ;; ④ Fresnel 边缘光（01 步）
          (vec3 v (normalize (- uViewPos vFragW)))
          (float fres (pow (- 1.0 (max (dot n v) 0.0)) 3.0))
          ;; ⑤ 发光 emissive：另一个频率的噪声控制强度 → 斑驳地发蓝光
          (float glow (fbm (+ (* vFragW 3.0) (vec3 5.2 1.3 7.7))))
          (vec3 emissive (* (vec3 0.12 0.6 1.2) glow))
          ;; 最终 = 光照部分 + Fresnel + emissive
          (vec3 col (+ (* albedo (+ (* 0.15 (vec3 1.0)) (* diff (vec3 0.85))))
                       (+ (* 0.35 (vec3 0.5 0.8 1.0) fres) emissive)))
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

(define (on-char e)
  (define code (send e get-key-code))
  (when (or (eq? code 'escape) (eq? code #\q))
    (exit 0)))

(define (draw)
  (define t (/ (- (current-inexact-milliseconds) start-ms) 1000.0))
  (define-values (w h) (send canvas get-gl-client-size))
  (define aspect (/ (exact->inexact w) (exact->inexact h)))
  (define P (m4-perspective 45.0 aspect 0.1 100.0))
  (define V (m4-translate 0.0 0.0 -3.0))
  (define M (m4-rot-y (* t 40.0)))

  (glClearColor 0.07 0.08 0.14 1.0)
  (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
  (glUseProgram prog)
  (glUniformMatrix4fv loc-model 1 #f (mat4 M))
  (glUniformMatrix4fv loc-mvp   1 #f (mat4 (m4-mult (m4-mult P V) M)))
  (glUniform3f loc-view 0.0 0.0 3.0)
  (glBindVertexArray vao)
  (glDrawElements GL_TRIANGLES cnt GL_UNSIGNED_INT 0))

(define-values (frame canvas)
  (make-window #:title "18-02 发光苔藓（emissive）" #:width 800 #:height 600
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
          (printf "渲染器：~a~%" (glGetString GL_RENDERER))
          (glEnable GL_DEPTH_TEST)
          (mesh->vao (obj-mesh-verts mesh) (obj-mesh-idx mesh)))))

(define ticker
  (new timer% (interval 16)
       (notify-callback (lambda () (send canvas refresh)))))

(send frame show #t)
(send canvas focus)
