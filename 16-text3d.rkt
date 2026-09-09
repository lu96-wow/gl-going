#lang racket/base
;; =========================================================
;; 16-text3d.rkt —— 把文字放进 3D 空间（raylib DrawText3D 同款思路）
;; 运行：racket 16-text3d.rkt
;;   B = 标签 billboard（面向相机）↔ 固定世界朝向    D = 深度测试 开/关
;;   ESC = 退出（相机自动环绕，看文字在不同角度下的表现）
;; =========================================================
;; 15 课回答"字符从哪来"：glyph atlas + 逐字四边形。本课回答"四边形
;; 能放在哪"：15 课把它摆在屏幕像素上（叠加层），其实文字和立方体一样
;; 只是"顶点数据"。把它从像素坐标系搬进世界坐标系、吃同一套 MVP 和
;; 深度测试，文字就活在 3D 里了 —— 这正是 raylib 的 DrawText3D 原理：
;;
;;   * raylib 的 Font 就是一张字符位图贴图 + 每个字的格子/步进；
;;   * DrawText3D 逐字符在世界坐标放四边形，字号 = 世界单位
;;     （fontSize/baseSize 换算），走与几何相同的相机/深度管线；
;;   * 它默认把四边形转成"面向相机"（billboard，名字牌效果）；
;;     把朝向矩阵换成世界固定朝向，字就贴到墙上/地上了。
;;
;; 本课一个场景里放三种文字，对比它们和普通几何的关系：
;;   ① 地面文字：世界固定朝向（平贴在 XZ 地面）→ 环绕时被透视压缩，
;;      从背面看是倒的 —— 它是"长在地面上的一张贴图"。
;;   ② 平面招牌：世界固定朝向（竖着朝 +Z）→ 绕到侧面就变一条线。
;;   ③ 漂浮标签：B 键在 billboard / 固定朝向间切换 —— billboard 每帧
;;      用相机朝向重算朝向矩阵，永远正对镜头（名字牌）；
;;      切回固定朝向立刻露馅：绕到背后字就镜像/侧躺。
;;
;; "3D 文字"还有第三种：把字形轮廓挤出成网格的真立体字 —— 那需要
;; 矢量字体三角化，raylib 的位图字帖同样做不到，不是本课范围。
;; 本课示范的是 (a) 贴平面 (b) billboard —— 覆盖 raylib 的用法。
;;
;; 深度测试对文字同样生效：招牌 ② 放在立方体正后方，D 键关掉深度时
;; 它无视遮挡画在最上层；打开后，被立方体挡住的笔画就被切掉 ——
;; 文字真的"在空间里"，而不是画在屏幕上的贴纸。
;; =========================================================

(require racket/gui opengl)
(require "lib.rkt")            ; m4-* / build-program

(define PI (acos -1.0))
(define start-ms (current-inexact-milliseconds))

;; ---- 内联 GLSL（用 (glsl ...) S 表达式写）----
(define cube-vert
  (glsl-pretty
   (glsl
    (version 330 core)
    (layout (location 0) in vec3 aPos)
    (layout (location 1) in vec3 aColor)
    (uniform mat4 uMVP)
    (out vec3 vColor)
    (define (main) void
      (set! vColor aColor)
      (set! gl_Position (* uMVP (vec4 aPos 1.0)))))))
(define cube-frag
  (glsl-pretty
   (glsl
    (version 330 core)
    (in vec3 vColor)
    (out vec4 FragColor)
    (define (main) void
      (set! FragColor (vec4 vColor 1.0))))))
;; 与 15 课唯一的区别：顶点从 vec2(像素) 变 vec2(文字局部坐标)，
;; uProj 换成完整的 uMVP —— 文字局部坐标先过模型矩阵进世界，再走相机。
;; 文字顶点着色器：aPos 是字形四边形角点(文字局部单位、基线 y=0)，
;; aUV 是字帖格子。uMVP = P·V·M：文字局部 → 世界 → 裁剪。
(define text-vert
  (glsl-pretty
   (glsl
    (version 330 core)
    (layout (location 0) in vec2 aPos)
    (layout (location 1) in vec2 aUV)
    (uniform mat4 uMVP)
    (out vec2 vUV)
    (define (main) void
      (set! vUV aUV)
      (set! gl_Position (* uMVP (vec4 aPos 0.0 1.0)))))))
;; 文字片元着色器：R8 字帖取覆盖率当 alpha，笔画处不透明、留白全透明。
(define text-frag
  (glsl-pretty
   (glsl
    (version 330 core)
    (in vec2 vUV)
    (uniform sampler2D uFont)
    (uniform vec4 uColor)
    (out vec4 FragColor)
    (define (main) void
      (float cov (r (texture uFont vUV)))
      (set! FragColor (vec4 (rgb uColor) (* (a uColor) cov)))))))

;; ---- 字帖参数（同 15，本课只收 ASCII 字符，字帖与 3D 无关）----
(define FNT-SIZE 48)
(define ATLAS-W 1024)
(define GAP 4)

(define cfg (new gl-config%))
(send cfg set-legacy? #f)
(send cfg set-double-buffered #t)
(send cfg set-depth-size 1)
(define frame
  (new (class frame%
         (augment* [on-close (lambda () (exit 0))])
         (super-new))
       (label "16 3D 文字") (width 800) (height 600)))

(define billboard-on? (box #t))   ; B 键切换
(define depth-on? (box #t))       ; D 键切换
(define init? (box #f))
(define fw (box 800)) (define fh (box 600))

(define prog-cube #f) (define prog-text #f)
(define loc-cube-mvp 0) (define loc-text-mvp 0)
(define loc-color 0) (define loc-font 0)
(define tex-atlas 0) (define vao-text 0) (define vbo-text 0) (define vao-cube 0)
(define glyphs (make-hash))       ; char -> (u0 v0 u1 v1 advance-px)
(define font-h0 88) (define font-asc 69) (define font-des 19)

;; =========================================================
;; ① 字帖生成（ASCII 简化版，逻辑同 15：量宽→排版→画位图→上传 R8）
;; =========================================================
(define (build-glyph-atlas)
  (define chars (for/list ([i (in-range 32 127)]) (integer->char i)))
  (define fnt (make-object font% FNT-SIZE 'default))
  (define probe (make-bitmap 2 2))
  (define pdc (new bitmap-dc% (bitmap probe)))
  (send pdc set-font fnt)
  (define (adv-of ch)
    (exact-round (car (call-with-values
                       (lambda () (send pdc get-text-extent (string ch) fnt)) list))))
  (define h0 (exact-round (list-ref (call-with-values
                                     (lambda () (send pdc get-text-extent "A" fnt)) list) 1)))
  (define des (exact-round (list-ref (call-with-values
                                      (lambda () (send pdc get-text-extent "A" fnt)) list) 2)))
  (set! font-h0 h0) (set! font-asc (- h0 des)) (set! font-des des)
  ;; 排版（一行放不下换行）
  (define placed '())
  (define x 0) (define y 0)
  (for ([ch chars])
    (define w (adv-of ch))
    (when (> (+ x w) ATLAS-W) (set! y (+ y h0 GAP)) (set! x 0))
    (set! placed (cons (list ch x y w) placed))
    (set! x (+ x w GAP)))
  (define ah (+ y h0 GAP))
  ;; 光栅化白字
  (define bm (make-bitmap ATLAS-W ah))
  (send bm set-argb-pixels 0 0 ATLAS-W ah (make-bytes (* ATLAS-W ah 4) 0))
  (define dc (new bitmap-dc% (bitmap bm)))
  (send dc set-text-foreground (make-object color% 255 255 255))
  (send dc set-font fnt)
  (for ([p (reverse placed)])
    (send dc draw-text (string (list-ref p 0)) (list-ref p 1) (list-ref p 2)))
  ;; alpha 通道 → 单通道 R8（覆盖率）
  (define argb (make-bytes (* ATLAS-W ah 4)))
  (send bm get-argb-pixels 0 0 ATLAS-W ah argb)
  (define alpha (make-bytes (* ATLAS-W ah)))
  (for ([i (in-range (* ATLAS-W ah))])
    (bytes-set! alpha i (bytes-ref argb (* i 4))))
  (define tex (u32vector-ref (glGenTextures 1) 0))
  (glBindTexture GL_TEXTURE_2D tex)
  (glPixelStorei GL_UNPACK_ALIGNMENT 1)
  (glTexImage2D GL_TEXTURE_2D 0 GL_R8 ATLAS-W ah 0 GL_RED GL_UNSIGNED_BYTE alpha)
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP_TO_EDGE)
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP_TO_EDGE)
  (for ([p placed])
    (define ch (list-ref p 0)) (define cx (list-ref p 1))
    (define cy (list-ref p 2)) (define w (list-ref p 3))
    (hash-set! glyphs ch
               (list (exact->inexact (/ cx ATLAS-W)) (exact->inexact (/ cy ah))
                     (exact->inexact (/ (+ cx w) ATLAS-W)) (exact->inexact (/ (+ cy h0) ah))
                     (exact->inexact w))))
  (printf "字帖 ~a x ~a px：~a 个 ASCII 字形（R8）~%" ATLAS-W ah (length placed))
  tex)

;; =========================================================
;; ② 文字局部几何（单位 = "em"，一个字符格高 = 1.0；基线在 y=0）
;; =========================================================
(define h0f (exact->inexact font-h0))
(define ascE (exact->inexact (/ font-asc h0f)))      ; 格顶（基线以上）
(define desE (exact->inexact (/ font-des h0f)))      ; 格底（基线以下）
(define cyE (/ (- ascE desE) 2.0))                   ; 格子在基线附近的中心

;; 一个字符的 advance（em）—— 缺字时退回空格宽度
(define (ch-adv-em ch)
  (define g (hash-ref glyphs ch #f))
  (if g (/ (list-ref g 4) h0f)
      (/ (list-ref (hash-ref glyphs #\space) 4) h0f)))

;; 字符串总宽（em，居中要减半）
(define (string-ems s) (for/sum ([ch (in-string s)]) (ch-adv-em ch)))

;; 逐字排四边形 → 6 顶点/字（x, y, u, v），y 正方向向上、uv v0=字格顶部。
;; 顶点顺序为 CCW（正面 = +Z，绕到背后字就镜像 —— 世界固定朝向的代价）
(define (string-quads-local s)
  (define parts '())
  (define pen 0.0)
  (for ([ch (in-string s)])
    (define g (or (hash-ref glyphs ch #f) (hash-ref glyphs #\space)))
    (define u0 (list-ref g 0)) (define v0 (list-ref g 1))
    (define u1 (list-ref g 2)) (define v1 (list-ref g 3))
    (define a (/ (list-ref g 4) h0f))
    (define x0 pen) (define x1 (+ pen a))
    (define yt ascE) (define yb (- desE))
    ;; tl, bl, br | tl, br, tr
    (set! parts (cons (list x0 yt u0 v0   x0 yb u0 v1   x1 yb u1 v1
                            x0 yt u0 v0   x1 yb u1 v1   x1 yt u1 v0)
                      parts))
    (set! pen (+ pen a)))
  (define flat (apply append (reverse parts)))
  (values (apply f32vector flat) (quotient (length flat) 4)))

;; =========================================================
;; ③ 朝向矩阵：文字四边形"面向相机"的 billboard 变换
;; =========================================================
;; 视图矩阵 V 的上 3×3（列主序存放）的"行"是相机在世界里的三个轴：
;;   行0 = 相机右 s，行1 = 相机上 u，行2 = 相机后向 -f
;; 把它转置成"列"，正好得到旋转矩阵：把文字局部坐标 (X=右, Y=上, Z=正对)
;; 旋转到"以相机为朝向"的世界姿态 —— 四边形于是永远正对镜头，
;; 但仍保留平移 → 仍吃透视 → 距离远了照样变小、该被挡还是被挡。
(define (billboard-rot V)
  (f64vector (f64vector-ref V 0) (f64vector-ref V 4) (f64vector-ref V 8) 0.0
             (f64vector-ref V 1) (f64vector-ref V 5) (f64vector-ref V 9) 0.0
             (f64vector-ref V 2) (f64vector-ref V 6) (f64vector-ref V 10) 0.0
             0.0 0.0 0.0 1.0))

;; =========================================================
;; ④ 画一段 3D 文字：M = T(位置)·R(朝向)·S(字号)·T(-宽/2, -格中心, 0)
;;    内层平移把字符串居中到 (x,y,z)，再按 R 朝向放进世界。
;; =========================================================
(define (text-model R x y z s wEm)
  (define T1 (m4-translate (- (/ wEm 2.0)) (- cyE) 0.0))
  (define S (m4-scale s s s))
  (m4-mult (m4-translate x y z) (m4-mult R (m4-mult S T1))))

(define (draw-3d-string s model color mvp)
  (glUniform4f loc-color (list-ref color 0) (list-ref color 1)
               (list-ref color 2) (list-ref color 3))
  (glUniformMatrix4fv loc-text-mvp 1 #f (mat4 (m4-mult mvp model)))
  (define-values (data n) (string-quads-local s))
  (glBindVertexArray vao-text)
  (glBindBuffer GL_ARRAY_BUFFER vbo-text)
  (glBufferData GL_ARRAY_BUFFER (gl-vector-sizeof data) data GL_DYNAMIC_DRAW)
  (glDrawArrays GL_TRIANGLES 0 n))

;; ---- 场景几何 ----
(define (build-cube-vao)          ; 同 15：6 面 6 色立方体（±1，36 顶点索引）
  (define pos8
    '((-1.0 -1.0  1.0) ( 1.0 -1.0  1.0) ( 1.0  1.0  1.0) (-1.0  1.0  1.0)
      (-1.0 -1.0 -1.0) ( 1.0 -1.0 -1.0) ( 1.0  1.0 -1.0) (-1.0  1.0 -1.0)))
  (define faces
    (list (list 0.85 0.20 0.20 '(0 1 2 3))
          (list 0.20 0.80 0.25 '(5 4 7 6))
          (list 0.95 0.60 0.10 '(1 5 6 2))
          (list 0.95 0.85 0.15 '(4 0 3 7))
          (list 0.20 0.60 0.95 '(3 2 6 7))
          (list 0.75 0.30 0.90 '(4 5 1 0))))
  (define verts
    (apply f32vector
           (apply append
                  (for/list ([f faces])
                    (apply append
                           (for/list ([j (in-range 4)])
                             (define p (list-ref pos8 (list-ref (list-ref f 3) j)))
                             (list (car p) (cadr p) (caddr p)
                                   (car f) (cadr f) (caddr f))))))))
  (define idx
    (apply u16vector
           (apply append
                  (for/list ([i (in-range 6)])
                    (define b (* i 4))
                    (list b (+ b 1) (+ b 2) b (+ b 2) (+ b 3))))))
  (define vao (u32vector-ref (glGenVertexArrays 1) 0))
  (glBindVertexArray vao)
  (define vbo (u32vector-ref (glGenBuffers 1) 0))
  (glBindBuffer GL_ARRAY_BUFFER vbo)
  (glBufferData GL_ARRAY_BUFFER (gl-vector-sizeof verts) verts GL_STATIC_DRAW)
  (define s6 (* 6 4))
  (glVertexAttribPointer 0 3 GL_FLOAT #f s6 0)
  (glEnableVertexAttribArray 0)
  (glVertexAttribPointer 1 3 GL_FLOAT #f s6 (* 3 4))
  (glEnableVertexAttribArray 1)
  (define ebo (u32vector-ref (glGenBuffers 1) 0))
  (glBindBuffer GL_ELEMENT_ARRAY_BUFFER ebo)
  (glBufferData GL_ELEMENT_ARRAY_BUFFER (gl-vector-sizeof idx) idx GL_STATIC_DRAW)
  (glBindVertexArray 0)
  vao)

;; =========================================================
;; 窗口：渲染循环
;; =========================================================
(define canvas
  (new (class canvas%
         (inherit with-gl-context swap-gl-buffers)
         (define/override (on-size w h)
           (with-gl-context
            (lambda ()
              (define-values (gw gh) (send this get-gl-client-size))
              (set-box! fw gw) (set-box! fh gh)
              (glViewport 0 0 gw gh)
              (glClearColor 0.08 0.09 0.14 1.0))))
         (define/override (on-char e)
           (define code (send e get-key-code))
           (when (not (eq? code 'release))
             (cond
               [(or (eq? code #\b) (eq? code #\B))
                (set-box! billboard-on? (not (unbox billboard-on?)))
                (printf (if (unbox billboard-on?)
                            "标签 billboard（面向相机）~%"
                            "标签固定朝 +Z（绕到背后会镜像/变线）~%"))]
               [(or (eq? code #\d) (eq? code #\D))
                (set-box! depth-on? (not (unbox depth-on?)))
                (printf (if (unbox depth-on?)
                            "深度测试 开（文字会被立方体遮挡）~%"
                            "深度测试 关（文字无视深度盖在最上层）~%"))]
               [(eq? code 'escape) (exit 0)])))
         (define/override (on-paint)
           (with-gl-context
            (lambda ()
              (unless (unbox init?)
                (set-box! init? #t)
                (printf "按键：B = billboard/固定朝向  D = 深度 开/关  ESC = 退出~%")
                (set! tex-atlas (build-glyph-atlas))
                (set! prog-cube (build-program cube-vert cube-frag))
                (set! loc-cube-mvp (glGetUniformLocation prog-cube "uMVP"))
                (set! prog-text (build-program text-vert text-frag))
                (set! loc-text-mvp (glGetUniformLocation prog-text "uMVP"))
                (set! loc-color (glGetUniformLocation prog-text "uColor"))
                (set! loc-font  (glGetUniformLocation prog-text "uFont"))
                (define tv (u32vector-ref (glGenVertexArrays 1) 0))
                (define tb (u32vector-ref (glGenBuffers 1) 0))
                (glBindVertexArray tv)
                (glBindBuffer GL_ARRAY_BUFFER tb)
                (glBufferData GL_ARRAY_BUFFER (* 4096 4) #f GL_DYNAMIC_DRAW)
                (define s4 (* 4 4))
                (glVertexAttribPointer 0 2 GL_FLOAT #f s4 0)
                (glEnableVertexAttribArray 0)
                (glVertexAttribPointer 1 2 GL_FLOAT #f s4 (* 2 4))
                (glEnableVertexAttribArray 1)
                (glBindVertexArray 0)
                (set! vao-text tv) (set! vbo-text tb)
                (set! vao-cube (build-cube-vao)))

              (define gw (unbox fw)) (define gh (unbox fh))
              (define t (/ (- (current-inexact-milliseconds) start-ms) 1000.0))

              ;; ---- 相机：绕 Y 轴自动环绕（观察 billboard 与贴地文字的区别）----
              (define P (m4-perspective 45.0 (/ (exact->inexact gw) (exact->inexact gh))
                                         0.1 100.0))
              (define yaw (* 16.0 t))
              (define yr (* (/ PI 180.0) yaw))
              (define V (m4-look-at (* 7.0 (sin yr)) 3.0 (* 7.0 (cos yr))
                                    0.0 1.1 0.0   0.0 1.0 0.0))
              (define mvp (m4-mult P V))

              ;; ---- 第 1 遍：立方体（不透明几何）----
              (glEnable GL_DEPTH_TEST)
              (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
              (glUseProgram prog-cube)
              (define (draw-cube M)
                (glUniformMatrix4fv loc-cube-mvp 1 #f (mat4 (m4-mult mvp M)))
                (glBindVertexArray vao-cube)
                (glDrawElements GL_TRIANGLES 36 GL_UNSIGNED_SHORT 0))
              (define C (m4-mult (m4-mult (m4-translate 0.0 1.1 0.0)
                                          (m4-scale 0.55 0.55 0.55))
                                 (m4-mult (m4-rot-y (* t 80.0)) (m4-rot-x (* t 45.0)))))
              (draw-cube C)

              ;; ---- 第 2 遍：3D 文字（半透明字形片 → 开混合）----
              (glEnable GL_BLEND)
              (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
              (if (unbox depth-on?)
                  (glEnable GL_DEPTH_TEST)
                  (glDisable GL_DEPTH_TEST))
              (glUseProgram prog-text)
              (glActiveTexture GL_TEXTURE0)
              (glBindTexture GL_TEXTURE_2D tex-atlas)
              (glUniform1i loc-font 0)

              ;; ① 地面文字：世界固定朝向，平贴在 XZ 地面上（R = 绕 X 转 -90°
              ;;    把文字平面从 XY 放倒到 XZ，字头朝 -Z 远离正面镜头）
              (define s1 "GROUND-LEVEL TEXT")
              (draw-3d-string s1
                              (text-model (m4-rot-x -90.0) 0.0 0.35 0.0 0.30
                                          (string-ems s1))
                              '(0.40 0.90 0.60 1.0) mvp)

              ;; ② 平面招牌：世界固定朝向竖在立方体正后方（吃深度 → D 演示）
              (define s2 "PLANE + DEPTH")
              (draw-3d-string s2
                              (text-model (m4-identity) 0.0 1.35 -1.7 0.35
                                          (string-ems s2))
                              '(0.45 0.95 0.95 1.0) mvp)

              ;; ③ 漂浮标签：B 键切换 billboard（面向相机）/ 固定朝 +Z
              (define s3 "BILLBOARD TAG")
              (define R3 (if (unbox billboard-on?)
                             (billboard-rot V)
                             (m4-identity)))
              (draw-3d-string s3
                              (text-model R3 2.4 1.9 -1.2 0.40
                                          (string-ems s3))
                              '(0.95 0.85 0.30 1.0) mvp)

              (glDisable GL_BLEND)
              (glDisable GL_DEPTH_TEST)

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
