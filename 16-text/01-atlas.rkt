#lang racket/base
;; =========================================================
;; 16-text/01-atlas.rkt —— 第一步：烤字帖 + 画一行文字
;; 运行：racket 16-text/01-atlas.rkt    点 X = 退出
;; =========================================================
;; 前面 15 课 GPU 只画"几何"。屏幕上的字符从哪来？本步把文字也变成几何。
;;
;; 本步新增（2 组）：
;;   ① 烤字帖（glyph atlas）—— 用 Racket 字体引擎把字符画进位图，拼成
;;      一张纹理，每个字符记下自己的格子(uv)与步进(advance)
;;   ② 画字 —— 渲染一个字符 = 画一个贴了该字格子的四边形，CPU 按 advance
;;      推光标把整串拼好，一次 draw 画完
;;
;; ★核心思路（分两步，CPU 各管一半）：
;;   烤字（一次）：字符是"形状"，不是 GPU 原生数据。用 Racket 字体引擎把
;;     每个要用的字符画进位图 → 拼成一张"字帖"；字帖的 alpha 通道（字形
;;     覆盖率）拷成单通道 R8 纹理。每个字符记下自己在字帖里的 uv 格子。
;;   画字（每帧）：一个字符 = 一个贴了该字格子的四边形。CPU 按 advance
;;     （步进宽度）推光标，把整串字符的四边形拼成顶点数组，一次画完。
;;     文字 = 用 alpha 雕刻字形的片（所以 11 课的混合在这里派上用场）。
;;
;; 本步视觉：深色背景上一行 "HELLO, TEXT!"。字帖只收录了 ASCII 32-126。
;; =========================================================

(require "lib-gui.rkt")
(require "lib.rkt")

(define FNT-SIZE 48)
(define ATLAS-W 1024)
(define GAP 4)

;; 文字顶点着色器：aPos 是四边形角点（像素坐标、y 向下），aUV 是字帖格子。
;; uProj 把像素坐标直接映射成 NDC（06 课的正交投影）。
(define text-vert
  (glsl (version 330 core)
        (layout (location 0) in vec2 aPos)
        (layout (location 1) in vec2 aUV)
        (uniform mat4 uProj)
        (out vec2 vUV)
        (define (main) void
          (set! vUV aUV)
          (set! gl_Position (* uProj (vec4 aPos 0.0 1.0))))))

;; 文字片元着色器：uFont 是 R8 字帖（单通道 = 字形覆盖率）。
;; 取 .r 当 alpha —— 笔画处不透明、留白处全透明。
(define text-frag
  (glsl (version 330 core)
        (in vec2 vUV)
        (uniform sampler2D uFont)
        (uniform vec4 uColor)
        (out vec4 FragColor)
        (define (main) void
          (float cov (r (texture uFont vUV)))
          (set! FragColor (vec4 (rgb uColor) (* (a uColor) cov))))))

;; ---- 烤字帖：选字体 → 排版 → 逐字画进位图 → 上传 R8 纹理 ----
(define (build-glyph-atlas)
  ;; 字帖收录 = ASCII 32-126
  (define chars (for/list ([i (in-range 32 127)]) (integer->char i)))
  (define fnt (make-object font% FNT-SIZE 'default))
  ;; 量字宽（advance = get-text-extent 的宽度，含左右侧隙）
  (define probe (make-bitmap 2 2))
  (define pdc (new bitmap-dc% (bitmap probe)))
  (send pdc set-font fnt)
  (define (ext s) (call-with-values (lambda () (send pdc get-text-extent s fnt)) list))
  (define h0 (exact-round (list-ref (ext "A") 1)))     ; 字格高
  (define des (exact-round (list-ref (ext "A") 2)))    ; 下行
  (define asc (- h0 des))                               ; 基线→字格顶
  ;; 排版：逐字放进自己的 advance 盒子，放不下就换行
  (define placed '())
  (define x 0) (define y 0)
  (for ([ch chars])
    (define w (exact-round (car (ext (string ch)))))
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
  ;; 上传：alpha 通道拷成单通道 R8（覆盖率）
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
  ;; 每字符：uv 归一化 + advance
  (define glyphs (make-hash))
  (for ([p placed])
    (define ch (list-ref p 0)) (define cx (list-ref p 1))
    (define cy (list-ref p 2)) (define w (list-ref p 3))
    (hash-set! glyphs ch
               (list (exact->inexact (/ cx ATLAS-W)) (exact->inexact (/ cy ah))
                     (exact->inexact (/ (+ cx w) ATLAS-W)) (exact->inexact (/ (+ cy h0) ah))
                     (exact->inexact w))))
  (values tex glyphs h0 asc))

;; ---- 字符串 → 四边形顶点（CPU 排版）----
;; 光标从 x 开始，每字符步进 advance*scale；字符画成 (advance×h0) 的四边形
(define (string-quads s glyphs h0 asc x by scale)
  (define y0 (- by (* asc scale)))          ; 字格顶 = 基线 - 上行
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
       ;; 一个字符 = 2 个三角形（6 顶点），每顶点 = 位置(vec2) + uv(vec2)
       (set! parts (append parts
                           (list (concat-vecs (vec2 px y0) (vec2 u0 v0)
                                              (vec2 x1 y0) (vec2 u1 v0)
                                              (vec2 x1 y1) (vec2 u1 v1)
                                              (vec2 px y0) (vec2 u0 v0)
                                              (vec2 x1 y1) (vec2 u1 v1)
                                              (vec2 px y1) (vec2 u0 v1)))))
       (set! px x1)]
      [else (set! px (+ px (* scale (list-ref (hash-ref glyphs #\space) 4))))]))
  (define data (apply concat-vecs parts))
  (values data (quotient (f32vector-length data) 4)))

;; 画一行文字：by = 基线 y
(define (draw-text s x by scale color glyphs h0 asc)
  (glUniform4f loc-color (list-ref color 0) (list-ref color 1)
               (list-ref color 2) (list-ref color 3))
  (define-values (data n) (string-quads s glyphs h0 asc x by scale))
  (glBindVertexArray vao-text)
  (glBindBuffer GL_ARRAY_BUFFER vbo-text)
  (glBufferData GL_ARRAY_BUFFER (gl-vector-sizeof data) data GL_DYNAMIC_DRAW)
  (glDrawArrays GL_TRIANGLES 0 n))

(define (draw)
  (define-values (w h) (send canvas get-gl-client-size))
  (glViewport 0 0 w h)
  (glClearColor 0.10 0.11 0.16 1.0)
  (glClear GL_COLOR_BUFFER_BIT)
  ;; 文字：关深度、开混合（alpha 雕刻字形）
  (glDisable GL_DEPTH_TEST)
  (glEnable GL_BLEND)
  (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
  (glUseProgram prog-text)
  (glActiveTexture GL_TEXTURE0)
  (glBindTexture GL_TEXTURE_2D tex-atlas)
  (glUniform1i loc-font 0)
  (glUniformMatrix4fv loc-proj 1 #f
                       (mat4-ortho 0.0 (exact->inexact w) (exact->inexact h) 0.0 -1.0 1.0))
  (draw-text "HELLO, TEXT!" 40.0 120.0 0.8 '(0.95 0.85 0.30 1.0) glyphs font-h0 font-asc)
  (glDisable GL_BLEND))

(define-values (frame canvas)
  (make-window #:title "16-01 烤字帖 + 画文字" #:width 800 #:height 400 #:draw draw))

(define prog-text (send canvas with-gl-context (lambda () (build-program text-vert text-frag))))
(define loc-proj  (send canvas with-gl-context (lambda () (glGetUniformLocation prog-text "uProj"))))
(define loc-color (send canvas with-gl-context (lambda () (glGetUniformLocation prog-text "uColor"))))
(define loc-font  (send canvas with-gl-context (lambda () (glGetUniformLocation prog-text "uFont"))))
;; 烤字帖（在 GL 上下文里，因为要上传纹理）
(define-values (tex-atlas glyphs font-h0 font-asc)
  (send canvas with-gl-context (lambda () (build-glyph-atlas))))
;; 文字 VAO：位置(2)+uv(2)，每帧重灌顶点
(define-values (vao-text vbo-text)
  (send canvas with-gl-context
        (lambda ()
          (define v (u32vector-ref (glGenVertexArrays 1) 0))
          (define b (u32vector-ref (glGenBuffers 1) 0))
          (glBindVertexArray v)
          (glBindBuffer GL_ARRAY_BUFFER b)
          (glBufferData GL_ARRAY_BUFFER (* 4096 4) #f GL_DYNAMIC_DRAW)
          (glVertexAttribPointer 0 (glsl-size 'vec2) GL_FLOAT #f (glsl-stride-bytes 'vec2 'vec2) 0)
          (glEnableVertexAttribArray 0)
          (glVertexAttribPointer 1 (glsl-size 'vec2) GL_FLOAT #f (glsl-stride-bytes 'vec2 'vec2) (glsl-stride-bytes 'vec2))
          (glEnableVertexAttribArray 1)
          (glBindVertexArray 0)
          (values v b))))

(send frame show #t)
