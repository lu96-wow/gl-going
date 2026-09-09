#lang racket/base
;; =========================================================
;; 15-text.rkt —— 文本渲染：glyph atlas（字形表）
;; 运行：racket 15-text.rkt    T = 文字开/关   ESC = 退出
;; =========================================================
;; 前面 14 课，GPU 只画"几何"。屏幕上的字符（标题、FPS、中文标签）从
;; 哪来？本课把文字也变成几何。新思路分两步，全部在 CPU 上：
;;
;;   ① 烤字（一次）：字符是"形状"，不是 GPU 原生数据。用 Racket 字体
;;      引擎把每个要用的字符画进位图 → 拼成一张"字帖"纹理（glyph
;;      atlas），每个字符记下自己在字帖里的格子(uv)与步进(advance)。
;;      字帖的 alpha 通道拷成单通道 R8 纹理 = 每个像素的"覆盖率"。
;;   ② 画字（每帧）：渲染一个字符 = 画一个贴了该字格子的四边形。
;;      CPU 按 advance 推光标把整串字符的四边形拼好，一次 draw 画完。
;;      —— 这正是 raylib / Dear ImGui 文本渲染的原理。
;;
;; 本课复用前面学到的每一块：纹理(08)、正交投影+像素世界(05)、
;; 混合+alpha(10，文字=用 alpha 雕刻字形的片)、每帧 uniform(04)、
;; 第二遍叠加绘制（先画 3D 场景、再叠一层文字）。
;;
;; 字体覆盖：中文需要系统里存在含中文字形的字体，且拉丁/中文两套字形
;; 的"格高"一致，逐字排版才不会错位 → 文件里 cjk-ok?/pick-font 逐个试
;; 候选字体。字符集只是数据：把想显示的字符加进 display-strings 即可。
;; （几千常用汉字会超过单张纹理 → 那是"字形缓存"优化课的事，这里只
;;   收录实际显示的几十个字符。）
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
;; 文字顶点着色器：aPos 是四边形角点(像素坐标、y 向下)，aUV 是字帖里的格子。
;; uProj 把像素坐标直接映射成 NDC（正交，与 05 课像素世界同款）。
(define text-vert
  (glsl-pretty
   (glsl
    (version 330 core)
    (layout (location 0) in vec2 aPos)
    (layout (location 1) in vec2 aUV)
    (uniform mat4 uProj)
    (out vec2 vUV)
    (define (main) void
      (set! vUV aUV)
      (set! gl_Position (* uProj (vec4 aPos 0.0 1.0)))))))
;; 文字片元着色器：uFont 是 R8 字帖(单通道=字形覆盖率)。
;; 取 .r 作为覆盖率，用它当 alpha —— 字形笔画处不透明、留白处全透明。
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

;; ---- 字帖参数 ----
(define FNT-SIZE 48)           ; 光栅化字号（大一点 → 缩小显示时清晰）
(define ATLAS-W 1024)          ; 字帖宽度（一行放不下就换行）
(define GAP 4)                 ; 字形格子之间的空隙（避免线性过滤串色）

;; 本课要显示的所有字符串（含中文）：字帖收录 = ASCII 全量 + 这些字符串里
;; 出现的字符。注意 FPS 是动态拼的，但用到的数字/字母/标点都在 ASCII 里。
(define display-strings
  (list "GLYPH ATLAS TEXT"
        "one texture, latin + CJK 中英文混排"
        "你好 字帖支持中文"
        "立方体 #1"
        "T 文字开/关  ESC 退出"))

;; 候选字体：要同时含拉丁与中文字形，且两者的度量一致。字体也决定格子
;; 行高/基线 —— 若两套字形格高不同（如 default 回退时 A=88px、中=65px），
;; 逐字格排版就会上下错位。下面按常见程度列出，运行时逐个试（见 cjk-ok?）。
(define font-candidates
  (list "WenQuanYi Micro Hei" "WenQuanYi Zen Hei"
        "Noto Sans CJK SC" "Noto Sans CJK SC Regular"
        "Droid Sans Fallback" "Microsoft YaHei"
        "PingFang SC" "Source Han Sans SC"))

(define cfg (new gl-config%))
(send cfg set-legacy? #f)
(send cfg set-double-buffered #t)
(send cfg set-depth-size 1)
(define frame
  (new (class frame%
         (augment* [on-close (lambda () (exit 0))])
         (super-new))
       (label "15 文本渲染") (width 800) (height 600)))

(define text-on? (box #t))
(define init? (box #f))
(define fw (box 800)) (define fh (box 600))

;; GL 对象与着色器 uniform
(define prog-scene #f) (define prog-text #f)
(define loc-mvp 0) (define loc-proj 0) (define loc-color 0) (define loc-font 0)
(define tex-atlas 0) (define vao-text 0) (define vbo-text 0)
(define vao-cube 0)
;; 字帖元数据（build 时填）：glyph 表 + 字体行高
(define glyphs (make-hash))    ; char -> (u0 v0 u1 v1 advance-px)
(define font-h0 88)            ; 字格高度(px) = 上行+下行，同一字体所有字一致
(define font-asc 69)           ; 字格顶 → 基线 的距离(px)

;; FPS 显示
(define fps-str (box "FPS --"))

;; =========================================================
;; ① 字帖生成器：选字体 → 量宽 → 排版 → 逐字画进位图 → 上传 R8 纹理
;;    纯 Racket 部分（画位图）不依赖 GL；只有最后上传要在 GL 上下文里。
;; =========================================================

;; 一个字体可不可用：能画出"中"的墨迹（字形存在，不是豆腐块），且
;; 拉丁字母 A 与中文 中 的格子高/下行一致（度量统一，逐字格不会错位）
(define (cjk-ok? face)
  (with-handlers ([exn:fail? (lambda (e) #f)])
    (define f (if (string? face)
                  (make-object font% FNT-SIZE face 'default)
                  (make-object font% FNT-SIZE face)))
    (define (ext s)
      (define bm (make-bitmap 2 2))
      (define dc (new bitmap-dc% (bitmap bm)))
      (call-with-values (lambda () (send dc get-text-extent s f)) list))
    (define eA (ext "A")) (define eZ (ext "中"))
    (define same-metric?
      (and (= (exact-round (list-ref eA 1)) (exact-round (list-ref eZ 1)))
           (= (exact-round (list-ref eA 2)) (exact-round (list-ref eZ 2)))))
    ;; 墨迹测试：把"中"画进位图，数一数 alpha>120 的像素
    (define bm2 (make-bitmap 128 96))
    (define dc2 (new bitmap-dc% (bitmap bm2)))
    (send bm2 set-argb-pixels 0 0 128 96 (make-bytes (* 128 96 4) 0))
    (send dc2 set-text-foreground (make-object color% 255 255 255))
    (send dc2 set-font f)
    (send dc2 draw-text "中" 10 10)
    (define px (make-bytes (* 128 96 4)))
    (send bm2 get-argb-pixels 0 0 128 96 px)
    (define ink 0)
    (for ([i (in-range (* 128 96))])
      (when (> (bytes-ref px (* i 4)) 120) (set! ink (+ ink 1))))
    (and same-metric? (> ink 400))))

;; 从候选字体里挑第一个可用的；全不行就退回 default（中文可能变豆腐/错位）
(define (pick-font)
  (for/or ([face font-candidates])
    (and (cjk-ok? face) face)))

(define (build-glyph-atlas)
  ;; 字帖收录的字符 = ASCII 全量 + 显示字符串里出现过的字（含中文）
  (define chars
    (remove-duplicates
     (append (for/list ([i (in-range 32 127)]) (integer->char i))
             (apply append (map string->list display-strings)))))
  ;; 挑字体：default 只在没有可用中文字体时才退回去
  (define picked (pick-font))
  (define fnt (if picked
                  (make-object font% FNT-SIZE picked 'default)
                  (make-object font% FNT-SIZE 'default)))
  (printf "字帖字体：~a~%" (or picked "default(未找到带中文的字体！)"))
  (define probe (make-bitmap 2 2))
  (define pdc (new bitmap-dc% (bitmap probe)))
  (send pdc set-font fnt)
  ;; get-text-extent 返回 (宽 高 下行 偏移)：宽 = 该字符的 advance（含左右侧隙）
  (define (adv-of ch)
    (exact-round (car (call-with-values (lambda () (send pdc get-text-extent (string ch) fnt)) list))))
  (define h0 (exact-round (list-ref (call-with-values (lambda () (send pdc get-text-extent "A" fnt)) list) 1)))
  (define des (exact-round (list-ref (call-with-values (lambda () (send pdc get-text-extent "A" fnt)) list) 2)))
  (set! font-h0 h0) (set! font-asc (- h0 des))

  ;; 排版：逐字放进"自己的 advance 盒子"，放不下就换行
  (define placed '())          ; (ch x y w)
  (define x 0) (define y 0)
  (for ([ch chars])
    (define w (adv-of ch))
    (when (> (+ x w) ATLAS-W) (set! y (+ y h0 GAP)) (set! x 0))
    (set! placed (cons (list ch x y w) placed))
    (set! x (+ x w GAP)))
  (define ah (+ y h0 GAP))

  ;; 光栅化：白字画进透明位图（Racket 字体引擎负责抗锯齿）
  (define bm (make-bitmap ATLAS-W ah))
  (send bm set-argb-pixels 0 0 ATLAS-W ah (make-bytes (* ATLAS-W ah 4) 0))
  (define dc (new bitmap-dc% (bitmap bm)))
  (send dc set-text-foreground (make-object color% 255 255 255))
  (send dc set-font fnt)
  (for ([p (reverse placed)])
    (send dc draw-text (string (list-ref p 0)) (list-ref p 1) (list-ref p 2)))

  ;; 上传：把 alpha 通道（byte0）拷成单通道，当覆盖率用
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

  ;; 每个字符：uv 归一化 + advance（后续画字只用这三样）
  (for ([p placed])
    (define ch (list-ref p 0)) (define cx (list-ref p 1))
    (define cy (list-ref p 2)) (define w (list-ref p 3))
    (hash-set! glyphs ch
               (list (exact->inexact (/ cx ATLAS-W)) (exact->inexact (/ cy ah))
                     (exact->inexact (/ (+ cx w) ATLAS-W)) (exact->inexact (/ (+ cy h0) ah))
                     (exact->inexact w))))
  (printf "字帖 ~a x ~a px：~a 个字形（ASCII+中文）已上传为 R8 纹理~%"
          ATLAS-W ah (length placed))
  tex)

;; =========================================================
;; ② 字符串 → 四边形顶点：CPU 端排版（这是"画文字"的本质）
;; =========================================================

;; 一个字符串在给定缩放下的总宽（居中要用）
(define (string-width s scale)
  (for/sum ([ch (in-string s)])
    (* scale (if (hash-has-key? glyphs ch)
                 (list-ref (hash-ref glyphs ch) 4)
                 (list-ref (hash-ref glyphs #\space) 4)))))

;; 排版整个字符串：光标 px 从 x 开始，每字符步进 advance*scale，
;; 字符画成 (advance×h0) 的四边形，贴上它在字帖里那一格
(define (string-quads s x by scale)
  (define asc font-asc) (define h0 font-h0)
  (define y0 (- by (* asc scale)))          ; 字格顶 = 基线 - 上行高
  (define y1 (+ y0 (* h0 scale)))
  (define parts '())
  (define px x)
  (for ([ch (in-string s)])
    (define g (hash-ref glyphs ch #f))
    (cond
      [g
       (define u0 (list-ref g 0)) (define v0 (list-ref g 1))
       (define u1 (list-ref g 2)) (define v1 (list-ref g 3))
       (define wpx (* (list-ref g 4) scale))
       (define x1 (+ px wpx))
       ;; 两个三角形（位置, uv）×6 顶点
       (set! parts (append parts
                           (list px y0 u0 v0   x1 y0 u1 v0   x1 y1 u1 v1
                                 px y0 u0 v0   x1 y1 u1 v1   px y1 u0 v1)))
       (set! px x1)]
      [else (set! px (+ px (* scale (list-ref (hash-ref glyphs #\space) 4))))]))
  (values (apply f32vector parts) (quotient (length parts) 4)))

;; 画一行文字：by = 基线 y（文字坐在这条线上，向上长）
(define (draw-text s x by scale color)
  (glUniform4f loc-color (list-ref color 0) (list-ref color 1)
               (list-ref color 2) (list-ref color 3))
  (define-values (data n) (string-quads s x by scale))
  (glBindVertexArray vao-text)
  (glBindBuffer GL_ARRAY_BUFFER vbo-text)
  (glBufferData GL_ARRAY_BUFFER (gl-vector-sizeof data) data GL_DYNAMIC_DRAW)
  (glDrawArrays GL_TRIANGLES 0 n))

;; 3D 点 (x,y,z) 经 MVP 投影到屏幕像素坐标（文本标签贴 3D 物体用）
(define (project-to-screen m px py pz gw gh)
  (define (el c r) (f64vector-ref m (+ (* 4 c) r)))
  (define w (+ (* (el 3 0) px) (* (el 3 1) py) (* (el 3 2) pz) (el 3 3)))
  (if (<= w 0.0) #f
      (let ([cx (/ (+ (* (el 0 0) px) (* (el 0 1) py) (* (el 0 2) pz) (el 0 3)) w)]
            [cy (/ (+ (* (el 1 0) px) (* (el 1 1) py) (* (el 1 2) pz) (el 1 3)) w)])
        (values (* (+ (* cx 0.5) 0.5) gw)     ; NDC → 像素，x 向右
                (* (- 0.5 (* cy 0.5)) gh))))) ; NDC y 向上 → 像素 y 向下

;; 立方体 VAO（05 同款：6 面 6 色，pos+color）
(define (build-cube-vao)
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
              (glClearColor 0.08 0.09 0.14 1.0))))
         (define/override (on-char e)
           (define code (send e get-key-code))
           (when (not (eq? code 'release))
             (cond
               [(or (eq? code #\t) (eq? code #\T))
                (set-box! text-on? (not (unbox text-on?)))
                (printf (if (unbox text-on?) "文字 开~%" "文字 关（只剩 3D 场景）~%"))]
               [(eq? code 'escape) (exit 0)])))
         (define/override (on-paint)
           (with-gl-context
            (lambda ()
              (unless (unbox init?)
                (set-box! init? #t)
                ;; 字帖（内部：量字→排版→画位图→上传 R8 纹理）
                (set! tex-atlas (build-glyph-atlas))
                ;; 场景着色器（立方体）与文字着色器
                (set! prog-scene
                      (build-program cube-vert cube-frag))
                (set! loc-mvp (glGetUniformLocation prog-scene "uMVP"))
                (set! prog-text
                      (build-program text-vert text-frag))
                (set! loc-proj  (glGetUniformLocation prog-text "uProj"))
                (set! loc-color (glGetUniformLocation prog-text "uColor"))
                (set! loc-font  (glGetUniformLocation prog-text "uFont"))
                ;; 文字 VAO：位置(2) + uv(2)，每帧重灌顶点数据
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

              ;; ========== 第 1 遍：3D 场景（深度测试开）==========
              (glViewport 0 0 gw gh)
              (glEnable GL_DEPTH_TEST)
              (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
              (define aspect (/ (exact->inexact gw) (exact->inexact gh)))
              (define P (m4-perspective 45.0 aspect 0.1 100.0))
              (define V (m4-mult (m4-translate 0.0 0.0 -6.0) (m4-rot-y 20.0)))
              (glUseProgram prog-scene)
              (define (draw-cube-at M)
                (glUniformMatrix4fv loc-mvp 1 #f (mat4 (m4-mult (m4-mult P V) M)))
                (glBindVertexArray vao-cube)
                (glDrawElements GL_TRIANGLES 36 GL_UNSIGNED_SHORT 0))
              (define M1 (m4-mult (m4-rot-y (* t 70.0)) (m4-rot-x (* t 45.0))))
              (draw-cube-at M1)                     ; 中心大立方体
              (for ([k (in-range 3)])               ; 三颗公转小立方体
                (define a (+ (* k 120.0) (* t 90.0)))
                (define r (* (/ PI 180.0) a))
                (define x (* 2.6 (cos r)))
                (define z (* 2.6 (sin r)))
                (draw-cube-at (m4-mult (m4-translate x 0.0 (- z)) (m4-rot-y (* t -60.0)))))

              ;; ========== 第 2 遍：屏幕文字（关深度、开混合）==========
              (glDisable GL_DEPTH_TEST)
              (glEnable GL_BLEND)
              (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
              (glUseProgram prog-text)
              (glActiveTexture GL_TEXTURE0)
              (glBindTexture GL_TEXTURE_2D tex-atlas)
              (glUniform1i loc-font 0)
              (glUniformMatrix4fv loc-proj 1 #f
                                  (mat4 (m4-ortho 0.0 (exact->inexact gw)
                                                     (exact->inexact gh) 0.0 -1.0 1.0)))
              (when (unbox text-on?)
                ;; 左上：标题 / 拉丁+CJK 混排说明（基线定位：by 是字底）
                (draw-text "GLYPH ATLAS TEXT" 16.0 62.0 0.62 '(0.95 0.85 0.30 1.0))
                (draw-text "one texture, latin + CJK 中英文混排" 18.0 92.0 0.40
                           '(0.75 0.80 0.95 1.0))
                ;; 右上：整行中文 → UTF-8 不需要任何额外处理
                (define zh "你好 字帖支持中文")
                (draw-text zh (- (exact->inexact gw) 16.0 (string-width zh 0.40))
                           92.0 0.40 '(0.45 0.95 0.90 1.0))
                ;; 立方体上方标签：把 3D 模型点投影到屏幕再写文字
                (define mvp1 (m4-mult (m4-mult P V) M1))
                (define-values (sx sy) (project-to-screen mvp1 0.0 0.0 0.0 gw gh))
                (define label "立方体 #1")
                (define ls 0.55)
                (draw-text label (- sx (/ (string-width label ls) 2.0))
                           (- sy 160.0) ls '(0.95 0.95 0.95 1.0))
                ;; 左下 FPS（动态内容：每帧变 → 字帖方案的价值）
                (draw-text (unbox fps-str) 16.0 (- (exact->inexact gh) 16.0)
                           0.50 '(0.45 0.90 0.55 1.0))
                ;; 右下操作提示（也含中文）
                (define hint "T 文字开/关  ESC 退出")
                (draw-text hint (- (exact->inexact gw) 16.0 (string-width hint 0.40))
                           (- (exact->inexact gh) 20.0) 0.40 '(0.60 0.62 0.68 1.0)))
              (glDisable GL_BLEND)

              (send this swap-gl-buffers))))
         (super-new))
       (style '(gl no-autoclear))
       (gl-config cfg)
       (parent frame)))

;; FPS：每 0.5s 更新一次文字内容
(define frame-count 0)
(define fps-t0 (current-inexact-milliseconds))
(define ticker
  (new timer% (interval 16)
       (notify-callback
        (lambda ()
          (set! frame-count (+ frame-count 1))
          (define now (current-inexact-milliseconds))
          (when (>= (- now fps-t0) 500.0)
            (set! fps-str (box (format "FPS ~a   |   ~a ms/frame"
                                       (/ frame-count 0.5)
                                       (exact->inexact (/ (- now fps-t0) frame-count)))))
            (set! frame-count 0)
            (set! fps-t0 now))
          (send canvas refresh)))))
(send frame show #t)
(send canvas focus)
