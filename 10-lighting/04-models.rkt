#lang racket/base
;; =========================================================
;; 10-lighting/04-models.rkt —— 第四步：光照不止一种实现（模型谱系）
;; 运行：racket 10-lighting/04-models.rkt     P/B = 切高光模型   点X = 退出
;; =========================================================
;; 前三步走完了 Phong 一族。但**光照远不止这一种实现**——本步把谱系铺开，
;; 并用 P/B 键当场切换两种高光，让你"看见"它们确实是两种算法。
;;
;; ★光照模型谱系（这是进阶课的核心视野）：
;;
;;   ① 在哪里算光照（"着色"位置，三个层级，成本递增）：
;;        flat（逐面）    整个三角形一个法线一个色——最省，面与面有硬边
;;        Gouraud（逐顶点）在顶点着色器算光，颜色在面内插值——平滑但高光在
;;                        三角形内部会被"抹平"（大三角里高光可能整块消失）
;;        Phong shading（逐片元）在片元着色器算光——本课用的，能捕捉高光，
;;                        最平滑也最贵（03 步就是逐片元）
;;
;;   ② 高光怎么算（本步切换的两种，只是 specular 那一项的不同）：
;;        Phong       R = reflect(-L,N)，spec = pow(max(R·V,0), n)
;;        Blinn-Phong H = normalize(L+V)，spec = pow(max(N·H,0), n)
;;        ——Blinn-Phong 用半向量 H 近似反射方向，省一次 reflect，且某些角度
;;          更接近真实。03 步用的就是 Blinn-Phong。两者光斑形状略有不同。
;;
;;   ③ 再往上（现代游戏/电影的真实方向）：
;;        PBR（physically based rendering）从物理定律出发（能量守恒、微表面
;;        模型、BRDF），用 albedo/metallic/roughness 等真实材质参数，而不是
;;        Phong 的经验参数。Phong 是"看着像"的经验模型，PBR 是"物理正确"。
;;        目标不同、代价不同——没有"唯一正确"，只有"够用且便宜"。
;;
;; 本步视觉：P 键切 Phong（reflect），B 键切 Blinn-Phong（半向量），盯着高光
;;   光斑的边缘形状看差异。
;; =========================================================

(require "lib-gui.rkt")
(require "lib.rkt")

(define PI (acos -1.0))
(define start-ms (current-inexact-milliseconds))
(define blinn? (box #t))   ; #t = Blinn-Phong，#f = Phong

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
        (uniform float uBlinn)   ; 1 = Blinn-Phong，0 = Phong
        (out vec4 FragColor)
        (define (main) void
          (vec3 n (normalize vNormalW))
          (vec3 l (normalize (- uLightPos vFragW)))
          (vec3 v (normalize (- uViewPos vFragW)))
          (vec3 h (normalize (+ l v)))
          (float diff (max (dot n l) 0.0))
          ;; ★两种高光：Phong 用 reflect(-l,n)·v；Blinn-Phong 用 n·h
          (float spec
                (if (> uBlinn 0.5)
                    (pow (max (dot n h) 0.0) 64.0)
                    (pow (max (dot (reflect (- l) n) v) 0.0) 64.0)))
          (vec3 ambient (* 0.15 uLightColor))
          (vec3 diffuse (* diff uLightColor))
          (vec3 specular (* (* spec uLightColor) 0.8))
          (set! FragColor (vec4 (+ (* uAlbedo (+ ambient diffuse)) specular) 1.0)))))

(define (on-char e)
  (define code (send e get-key-code))
  (when (not (eq? code 'release))
    (cond
      [(or (eq? code #\b) (eq? code #\B))
       (set-box! blinn? #t) (printf "Blinn-Phong（半向量 H）~%")]
      [(or (eq? code #\p) (eq? code #\P))
       (set-box! blinn? #f) (printf "Phong（reflect）~%")]
      [(eq? code 'escape) (exit 0)])))

(define (draw)
  (define t (/ (- (current-inexact-milliseconds) start-ms) 1000.0))
  (define-values (w h) (send canvas get-gl-client-size))
  (define aspect (/ (exact->inexact w) (exact->inexact h)))
  (define P (mat4-perspective 45.0 aspect 0.1 100.0))
  (define V (mat4-look-at 0.0 0.0 5.0  0.0 0.0 0.0  0.0 1.0 0.0))
  (define M (mat4-rot-y (* t 30.0)))
  (define la (* (/ PI 180.0) (* t 70.0)))

  (glClearColor 0.06 0.07 0.12 1.0)
  (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
  (glUseProgram prog)
  (glUniformMatrix4fv loc-model 1 #f M)
  (glUniformMatrix4fv loc-mvp   1 #f (mat4-mult (mat4-mult P V) M))
  (glUniform3f loc-albedo 0.82 0.84 0.90)
  (glUniform3f loc-lcol 1.0 0.96 0.85)
  (glUniform3f loc-lpos (* 4.5 (cos la)) 3.2 (* 4.5 (sin la)))
  (glUniform3f loc-view 0.0 0.0 5.0)
  (glUniform1f loc-blinn (if (unbox blinn?) 1.0 0.0))
  (glBindVertexArray vao)
  (glDrawElements GL_TRIANGLES 36 GL_UNSIGNED_SHORT 0))

(define-values (frame canvas)
  (make-window #:title "10-04 光照模型谱系（P/B 切换）"
               #:width 600 #:height 600 #:draw draw #:on-char on-char))

(define prog (send canvas with-gl-context (lambda () (build-program vert-src frag-src))))
(define loc-model  (send canvas with-gl-context (lambda () (glGetUniformLocation prog "uModel"))))
(define loc-mvp    (send canvas with-gl-context (lambda () (glGetUniformLocation prog "uMVP"))))
(define loc-albedo (send canvas with-gl-context (lambda () (glGetUniformLocation prog "uAlbedo"))))
(define loc-lpos   (send canvas with-gl-context (lambda () (glGetUniformLocation prog "uLightPos"))))
(define loc-lcol   (send canvas with-gl-context (lambda () (glGetUniformLocation prog "uLightColor"))))
(define loc-view   (send canvas with-gl-context (lambda () (glGetUniformLocation prog "uViewPos"))))
(define loc-blinn  (send canvas with-gl-context (lambda () (glGetUniformLocation prog "uBlinn"))))
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
