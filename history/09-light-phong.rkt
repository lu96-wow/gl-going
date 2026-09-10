#lang racket/base
;; =========================================================
;; 09-light-phong.rkt —— 光照：逐片元 Phong（环境+漫反射+高光）
;; 运行：racket 09-light-phong.rkt   L = 光开/关   ESC = 退出
;; =========================================================
;; 前面的颜色都"写死"，物体才没有立体感。本课加光照，新东西三个：
;;
;;   ① 法线 attribute：每个顶点多一个"朝外方向"（vec3 单位向量）。
;;      面与光线的夹角 → 亮度：这就是"为什么光照能看出形状"。
;;   ② 光照计算挪进片元着色器（逐片元）：片元收到的法线是三角形面内
;;      插值，比"每顶点算完再插值颜色"更平滑（后者叫逐顶点 Gouraud）。
;;   ③ Phong 三项加和（图形学最经典的经验模型）：
;;       环境 ambient   = 常数 * 光色          —— 没有直射时也不全黑
;;       漫反射 diffuse = max(N·L,0) * 光色    —— 面越正对光源越亮
;;       高光 specular  = pow(max(R·V,0), shininess) —— 视线方向的反光斑
;;     最终 = albedo(物体色) × (ambient+diffuse) + specular
;;
;; 新 API：这课没有新 GL 函数——光照全部发生在着色器里。
;; 但 Racket 侧每帧要多上传三个 uniform：光位置、相机位置、物体色。
;;
;; 演示：中央自转立方体 + 环绕的彩色小立方体 + 绕行的小点光源。
;; 光源位置每帧上传到 uLightPos——光源也是"数据"，跟物体一样可以动。
;; =========================================================

(require racket/gui opengl)
(require "lib.rkt")

(define PI (acos -1.0))
(define start-ms (current-inexact-milliseconds))
(define light-on? (box #t))

;; 顶点着色器：aPos/aNormal 是模型空间的位置/法线。
;; 法线只受旋转影响 → 用 mat3(uModel)（本课模型只有旋转+等比缩放，足够）。
;; 同时把世界坐标 vFragW 传给片元，因为光照要在世界空间里算方向。
(define vert-src
  (glsl-pretty
   (glsl
    (version 330 core)
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
      (set! gl_Position (* uMVP (vec4 aPos 1.0)))))))

;; 片元着色器：uniform 含义——uAlbedo=底色、uLightPos=点光源(世界)、
;; uViewPos=相机位置(世界)、uLightOn=开关(0 时直接输出底色)。
;; 公式逐项怎么读，看下面那段"用法小抄"。
(define frag-src
  (glsl-pretty
   (glsl
    (version 330 core)
    (in vec3 vNormalW)
    (in vec3 vFragW)
    (uniform vec3 uAlbedo)
    (uniform vec3 uLightPos)
    (uniform vec3 uLightColor)
    (uniform vec3 uViewPos)
    (uniform float uLightOn)
    (out vec4 FragColor)
    (define (main) void
      (vec3 n (normalize vNormalW))
      (vec3 albedo uAlbedo)
      (when (< uLightOn 0.5)
        (set! FragColor (vec4 albedo 1.0))
        (return))
      (vec3 l (normalize (- uLightPos vFragW)))
      (vec3 v (normalize (- uViewPos vFragW)))
      (vec3 h (normalize (+ l v)))
      (float diff (max (dot n l) 0.0))
      (float spec (pow (max (dot n h) 0.0) 64.0))
      (vec3 ambient (* 0.15 uLightColor))
      (vec3 diffuse (* diff uLightColor))
      (vec3 specular (* (* spec uLightColor) 0.8))
      (set! FragColor (vec4 (+ (* albedo (+ ambient diffuse)) specular) 1.0))))))

;; 用法小抄——照这个顺序读片元着色器里的公式：
;;   normalize(v)：向量变成"长度=1、只留方向"（点积/方向比较才有意义）
;;   dot(a,b) 方向为单位时 = cosθ；max(·, 0) 把"背光面"夹成 0——
;;     不夹的话背光面是负亮度，会越照越黑（反了）
;;   pow(cosθ, 64)：高光"锐度"。64 表示只有几乎正对反射方向才亮 →
;;     小而亮的光斑；改成 8 就变成大而柔的光斑（可自己改 64 对比）

(define cfg (new gl-config%))
(send cfg set-legacy? #f)
(send cfg set-double-buffered #t)
(send cfg set-depth-size 1)
(define frame
  (new (class frame%
         (augment* [on-close (lambda () (exit 0))])
         (super-new))
       (label "09 Phong 光照") (width 800) (height 600)))

(define fw (box 800)) (define fh (box 600))
(define init? (box #f))
(define prog #f) (define vao-cube 0)
(define loc-model 0) (define loc-mvp 0) (define loc-albedo 0)
(define loc-lpos 0) (define loc-lcol 0) (define loc-view 0) (define loc-on 0)

(define canvas
  (new (class canvas%
         (inherit with-gl-context swap-gl-buffers)
         (define/override (on-size w h)
           (with-gl-context
            (lambda ()
              (define-values (gw gh) (send this get-gl-client-size))
              (set-box! fw gw) (set-box! fh gh)
              (glViewport 0 0 gw gh)
              (glEnable GL_DEPTH_TEST)
              (glClearColor 0.06 0.07 0.12 1.0))))
         (define/override (on-char e)
           (define code (send e get-key-code))
           (when (not (eq? code 'release))
             (cond
               [(eq? code 'escape) (exit 0)]
               [(or (eq? code #\l) (eq? code #\L))
                (set-box! light-on? (not (unbox light-on?)))
                (printf (if (unbox light-on?) "光照 开~%" "光照 关（看纯 albedo）~%"))])))
         (define/override (on-paint)
           (with-gl-context
            (lambda ()
              (unless (unbox init?)
                (set-box! init? #t)
                (set! prog (build-program vert-src frag-src))
                (set! loc-model (glGetUniformLocation prog "uModel"))
                (set! loc-mvp   (glGetUniformLocation prog "uMVP"))
                (set! loc-albedo (glGetUniformLocation prog "uAlbedo"))
                (set! loc-lpos  (glGetUniformLocation prog "uLightPos"))
                (set! loc-lcol  (glGetUniformLocation prog "uLightColor"))
                (set! loc-view  (glGetUniformLocation prog "uViewPos"))
                (set! loc-on    (glGetUniformLocation prog "uLightOn"))
                ;; 立方体：每面 4 个独立顶点（法线要逐面朝外）
                (define pos8
                  '((-1.0 -1.0  1.0) ( 1.0 -1.0  1.0) ( 1.0  1.0  1.0) (-1.0  1.0  1.0)
                    (-1.0 -1.0 -1.0) ( 1.0 -1.0 -1.0) ( 1.0  1.0 -1.0) (-1.0  1.0 -1.0)))
                (define faces
                  (list (cons '(0.0 0.0 1.0)  '(0 1 2 3))
                        (cons '(0.0 0.0 -1.0) '(5 4 7 6))
                        (cons '(1.0 0.0 0.0)  '(1 5 6 2))
                        (cons '(-1.0 0.0 0.0) '(4 0 3 7))
                        (cons '(0.0 1.0 0.0)  '(3 2 6 7))
                        (cons '(0.0 -1.0 0.0) '(4 5 1 0))))
                (define verts
                  (apply f32vector
                         (apply append
                                (for/list ([f faces])
                                  (apply append
                                         (for/list ([j (in-range 4)])
                                           (define p (list-ref pos8 (list-ref (cdr f) j)))
                                           (define n (car f))
                                           (list (car p) (cadr p) (caddr p)
                                                 (car n) (cadr n) (caddr n))))))))
                (define idx
                  (apply u16vector
                         (apply append
                                (for/list ([i (in-range 6)])
                                  (define b (* i 4))
                                  (list b (+ b 1) (+ b 2) b (+ b 2) (+ b 3))))))
                (define v (u32vector-ref (glGenVertexArrays 1) 0))
                (glBindVertexArray v)
                (define vb (u32vector-ref (glGenBuffers 1) 0))
                (glBindBuffer GL_ARRAY_BUFFER vb)
                (glBufferData GL_ARRAY_BUFFER (gl-vector-sizeof verts) verts GL_STATIC_DRAW)
                (define s6 (* 6 4))
                (glVertexAttribPointer 0 3 GL_FLOAT #f s6 0)
                (glEnableVertexAttribArray 0)
                (glVertexAttribPointer 1 3 GL_FLOAT #f s6 (* 3 4))
                (glEnableVertexAttribArray 1)
                (define eb (u32vector-ref (glGenBuffers 1) 0))
                (glBindBuffer GL_ELEMENT_ARRAY_BUFFER eb)
                (glBufferData GL_ELEMENT_ARRAY_BUFFER (gl-vector-sizeof idx) idx GL_STATIC_DRAW)
                (glBindVertexArray 0)
                (set! vao-cube v))

              (define t (/ (- (current-inexact-milliseconds) start-ms) 1000.0))
              (define gw (unbox fw)) (define gh (unbox fh))
              (define aspect (/ (exact->inexact gw) (exact->inexact gh)))
              (define P (m4-perspective 45.0 aspect 0.1 100.0))
              ;; 相机绕场缓转，光/物体的位置都是"世界坐标"
              (define cam-rad (* (/ PI 180.0) (* t 18.0)))
              (define V (m4-look-at (* 8.0 (sin cam-rad)) 3.0
                                    (* 8.0 (cos cam-rad))
                                    0.0 0.0 0.0  0.0 1.0 0.0))

              (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
              (glUseProgram prog)
              ;; 每帧都上传的光照参数
              (glUniform3f loc-view (f64vector-ref V 12) (f64vector-ref V 13) (f64vector-ref V 14))
              (glUniform1f loc-on (if (unbox light-on?) 1.0 0.0))
              (glUniform3f loc-lcol 1.0 0.96 0.85)
              ;; 点光源绕圈
              (define la (* t 70.0))
              (define lrad (* (/ PI 180.0) la))
              (glUniform3f loc-lpos (* 4.5 (cos lrad)) 3.2 (* 4.5 (sin lrad)))

              (define (draw-cube m color)
                (glUniformMatrix4fv loc-model 1 #f (mat4 m))
                (glUniformMatrix4fv loc-mvp 1 #f (mat4 (m4-mult (m4-mult P V) m)))
                (glUniform3f loc-albedo (car color) (cadr color) (caddr color))
                (glBindVertexArray vao-cube)
                (glDrawElements GL_TRIANGLES 36 GL_UNSIGNED_SHORT 0))

              ;; 中央：翻滚的灰白立方体（旋转占√3体积，缩到 0.7）
              (define M1 (m4-mult (m4-mult (m4-rot-y (* t 50.0)) (m4-rot-x (* t 40.0)))
                                  (m4-scale 0.7 0.7 0.7)))
              (draw-cube M1 '(0.82 0.84 0.90))
              ;; 三颗彩色小立方体绕行（缩到 0.45，与中央保持间距不交叠）
              (for ([k (in-range 3)])
                (define a (+ (* k 120.0) (* t 90.0)))
                (define r (* (/ PI 180.0) a))
                (define M2 (m4-mult (m4-translate (* 2.7 (cos r)) 0.6 (* 2.7 (sin r)))
                                    (m4-mult (m4-rot-y (* t -90.0))
                                             (m4-scale 0.45 0.45 0.45))))
                (draw-cube M2 (list-ref '((0.95 0.3 0.3) (0.3 0.9 0.4) (0.3 0.6 0.95)) k)))

              (send this swap-gl-buffers))))
         (super-new))
       (style '(gl no-autoclear))
       (gl-config cfg)
       (parent frame)))

(define ticker
  (new timer% (interval 16)
       (notify-callback
        (lambda () (send canvas refresh)))))
(send frame show #t)
(send canvas focus)
