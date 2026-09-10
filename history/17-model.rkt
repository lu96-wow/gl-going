#lang racket/base
;; =========================================================
;; 17-model.rkt —— 模型导入：把 OBJ 文件变成能画的顶点数组
;; 运行：racket 17-model.rkt
;;   1/2 = 切换模型（cube / suzanne）   Space = 暂停旋转   ESC = 退出
;; =========================================================
;; 前面的课里，立方体/四边形都是"手写 f32vector"。真实场景里模型在
;; 文件里（OBJ/glTF…），而 GL 根本不知道"文件"是什么——它只认
;; VAO/VBO/EBO 里的数组。所以模型导入 = 一个三步流水线，raylib 的
;; LoadModel/DrawModel 就是这条流水线的现成实现：
;;
;;   raylib                         本课（Racket + lib.rkt）
;;   -----------------------------  ------------------------------------
;;   LoadModel 解析文件 → Mesh      (obj-load-file) 读文本 → obj-mesh
;;   （Mesh = CPU 上的纯数据：        （纯 Racket 数据，还不碰 GL）
;;     顶点/法线/uv/索引）
;;   rlLoadMesh 上传 GPU →          mesh->vao：verts→VBO，idx→EBO，
;;   VBO/EBO/VAO                     配好 attribute 指针
;;   rlDrawMesh → glDrawElements    glDrawElements（索引绘制）
;;
;; 所以"导入模型"没有新 GL 函数（VBO/EBO 是 01/03 课的旧知识），
;; 新东西是两步 CPU 侧的活：① 把文本文件解析成数组（lib.rkt 的
;; obj-load-file，见其注释）；② 认识这个数组的排布，照 03 课的方式
;; 灌进 VBO/EBO。
;;
;; obj-load-file 返回的 obj-mesh 有两个数组（8 个 float 一个顶点）：
;;   verts = f32vector [x y z | nx ny nz | u v]   （模型已居中、等比缩放到 ~1.6）
;;   idx   = u32vector 索引
;; 关键设计两点（本课正文各讲一次）：
;;   * 索引化去重：OBJ 的面共享角点；按"角点=(v,vt,vn) 三编号"去重，
;;     suzanne 的 968 个三角形只存 590 个顶点（省 80%）。
;;   * flat / 平滑法线：文件带 vn（建模软件烘焙好的平滑法线）就直接
;;     用；缺 vn 的面现场叉积算面法线（flat，不与邻面共享 → 硬边）。
;;     本课 1/2 切换即可看到两种：cube 的 vn 是逐面法线 → 硬边棱角；
;;     suzanne 的 vn 是平滑法线 → 圆润表面。
;; =========================================================

(require racket/gui opengl)
(require racket/runtime-path)
(require "lib.rkt")            ; obj-load-file / obj-mesh-* / m4-* / build-program

;; 模型文件：相对"本脚本所在目录"解析（而非运行时的当前目录），
;; 这样在任意目录下 racket 17-model.rkt 都能找到 assets/。
(define-runtime-path cube-obj-path "assets/models/cube.obj")
(define-runtime-path suz-obj-path  "assets/models/suzanne.obj")

(define PI (acos -1.0))
(define start-ms (current-inexact-milliseconds))
(define (now-sec) (/ (- (current-inexact-milliseconds) start-ms) 1000.0))

;; ---- 内联 GLSL（用 (glsl ...) S 表达式写，讲解放 S 表达式外）----
;; 只画"打光的几何"，不贴图：uv 已经在 verts 里，但本课聚焦模型导入，
;; 贴纹理是 08 课内容，"模型 + 纹理"合起来留到下一步自然接上。
;; 顶点着色器：aPos/aNormal 来自模型文件(v/vn，缺 vn 时 lib.rkt 叉积补)。
;; 法线只受旋转影响 → mat3(uModel) 跟着模型一起转。
(define vert-src
  (glsl-pretty
   (glsl
    (version 330 core)
    (layout (location 0) in vec3 aPos)
    (layout (location 1) in vec3 aNormal)
    (uniform mat4 uModel)
    (uniform mat4 uMVP)
    (out vec3 vNormalW)
    (define (main) void
      (set! vNormalW (* (mat3 uModel) aNormal))
      (set! gl_Position (* uMVP (vec4 aPos 1.0)))))))

;; 片元着色器：固定方向光 + 漫反射(N·L，09 课)，让模型有立体感即可。
;; 本课不贴图，uAlbedo 是纯底色。
(define frag-src
  (glsl-pretty
   (glsl
    (version 330 core)
    (in vec3 vNormalW)
    (uniform vec3 uAlbedo)
    (out vec4 FragColor)
    (define (main) void
      (vec3 n (normalize vNormalW))
      (vec3 l (normalize (vec3 0.4 0.9 0.6)))
      (float diff (max (dot n l) 0.0))
      (vec3 ambient (* 0.22 (vec3 1.0)))
      (vec3 diffuse (* diff (vec3 0.85)))
      (set! FragColor (vec4 (* uAlbedo (+ ambient diffuse)) 1.0))))))

(define cfg (new gl-config%))
(send cfg set-legacy? #f)
(send cfg set-double-buffered #t)
(send cfg set-depth-size 1)
(define frame
  (new (class frame%
         (augment* [on-close (lambda () (exit 0))])
         (super-new))
       (label "17 模型导入") (width 800) (height 600)))

(define fw (box 800)) (define fh (box 600))
(define init? (box #f))
(define prog #f)
(define loc-model 0) (define loc-mvp 0) (define loc-albedo 0)
(define vao-cube 0) (define cnt-cube 0)       ; cube.obj 的 VAO + 索引数
(define vao-suz 0)  (define cnt-suz 0)        ; suzanne.obj 的 VAO + 索引数
(define cur-model (box 1))                    ; 1 = cube，2 = suzanne
(define paused? (box #f))
(define paused-at (box 0.0))                  ; 暂停那一刻的 now
(define paused-acc (box 0.0))                 ; 累计暂停掉的秒数

;; =========================================================
;; ② 上传 GPU：obj-mesh → VAO（raylib 的 rlLoadMesh 这一步）
;; =========================================================
;; verts 的排布（8 float/顶点，stride = 32 字节）：
;;   offset 0   -> aPos    (3 float)
;;   offset 12  -> aNormal (3 float)
;;   offset 24  -> uv      (2 float，本课不贴图所以不绑定 location 2；
;;                           数据照旧躺在 VBO 里，纹理课随时可开)
(define (mesh->vao verts idx)
  (define vao (u32vector-ref (glGenVertexArrays 1) 0))
  (glBindVertexArray vao)
  (define vbo (u32vector-ref (glGenBuffers 1) 0))
  (glBindBuffer GL_ARRAY_BUFFER vbo)
  (glBufferData GL_ARRAY_BUFFER (gl-vector-sizeof verts) verts GL_STATIC_DRAW)
  (define stride (* 8 4))
  (glVertexAttribPointer 0 3 GL_FLOAT #f stride 0)
  (glEnableVertexAttribArray 0)
  (glVertexAttribPointer 1 3 GL_FLOAT #f stride (* 3 4))
  (glEnableVertexAttribArray 1)
  (define ebo (u32vector-ref (glGenBuffers 1) 0))
  (glBindBuffer GL_ELEMENT_ARRAY_BUFFER ebo)
  (glBufferData GL_ELEMENT_ARRAY_BUFFER (gl-vector-sizeof idx) idx GL_STATIC_DRAW)
  (glBindVertexArray 0)
  (values vao (u32vector-length idx)))

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
              (glClearColor 0.07 0.08 0.14 1.0))))
         (define/override (on-char e)
           (define code (send e get-key-code))
           (when (not (eq? code 'release))
             (cond
               [(eq? code #\1)
                (set-box! cur-model 1)
                (printf "模型：cube.obj（vn = 逐面法线 → 硬边）~%")]
               [(eq? code #\2)
                (set-box! cur-model 2)
                (printf "模型：suzanne.obj（vn = 平滑法线 → 圆润）~%")]
               [(eq? code #\space)
                (define now (now-sec))
                (if (unbox paused?)
                    (begin
                      (set-box! paused-acc (+ (unbox paused-acc)
                                              (- now (unbox paused-at))))
                      (set-box! paused? #f)
                      (printf "继续旋转~%"))
                    (begin
                      (set-box! paused-at now)
                      (set-box! paused? #t)
                      (printf "暂停~%")))]
               [(eq? code 'escape) (exit 0)])))
         (define/override (on-paint)
           (with-gl-context
            (lambda ()
              (unless (unbox init?)
                (set-box! init? #t)
                (printf "按键：1 = cube  2 = suzanne  Space = 暂停  ESC = 退出~%")
                (set! prog (build-program vert-src frag-src))
                (set! loc-model  (glGetUniformLocation prog "uModel"))
                (set! loc-mvp    (glGetUniformLocation prog "uMVP"))
                (set! loc-albedo (glGetUniformLocation prog "uAlbedo"))
                ;; ---- ① 解析（CPU）：文件 → 纯数据（raylib LoadModel 的解析阶段）----
                (define cube (obj-load-file cube-obj-path))
                (define suz  (obj-load-file suz-obj-path))
                (printf "~a~%" (obj-mesh-summary cube))
                (printf "~a~%" (obj-mesh-summary suz))
                ;; ---- ② 上传（GPU）：数据 → VAO/VBO/EBO（raylib 的 rlLoadMesh）----
                (define-values (vc cc) (mesh->vao (obj-mesh-verts cube) (obj-mesh-idx cube)))
                (define-values (vs cs) (mesh->vao (obj-mesh-verts suz)  (obj-mesh-idx suz)))
                (set! vao-cube vc) (set! cnt-cube cc)
                (set! vao-suz vs)  (set! cnt-suz cs))

              ;; ---- ③ 绘制（raylib 的 rlDrawMesh）：和画立方体一模一样 ----
              (define t (- (now-sec) (unbox paused-acc)))
              (define gw (unbox fw)) (define gh (unbox fh))
              (define P (m4-perspective 45.0 (/ (exact->inexact gw) (exact->inexact gh))
                                        0.1 100.0))
              ;; 相机绕场缓转，模型在原点自转（和 09 课画立方体同款）
              (define cam-rad (* (/ PI 180.0) (* t 18.0)))
              (define V (m4-look-at (* 4.5 (sin cam-rad)) 1.1
                                    (* 4.5 (cos cam-rad))
                                    0.0 0.0 0.0  0.0 1.0 0.0))
              (define M (m4-rot-y (* t 40.0)))

              (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
              (glUseProgram prog)
              (glUniformMatrix4fv loc-model 1 #f (mat4 M))
              (glUniformMatrix4fv loc-mvp 1 #f (mat4 (m4-mult (m4-mult P V) M)))
              (glUniform3f loc-albedo 0.82 0.84 0.88)

              ;; 切模型只换"绑哪个 VAO、画几个索引"——模型数据从文件来，
              ;; 但到了 GPU 上它和手写的立方体没有任何区别。
              (define-values (vao cnt) (if (= (unbox cur-model) 1)
                                           (values vao-cube cnt-cube)
                                           (values vao-suz cnt-suz)))
              (glBindVertexArray vao)
              (glDrawElements GL_TRIANGLES cnt GL_UNSIGNED_INT 0)

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
