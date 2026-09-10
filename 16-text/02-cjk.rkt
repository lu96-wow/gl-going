#lang racket/base
;; =========================================================
;; 16-text/02-cjk.rkt —— 第二步：中文支持 + 字体度量
;; 运行：racket 16-text/02-cjk.rkt    点 X = 退出
;; =========================================================
;; 上一步：ASCII 能画了。本步加中文——难点不在字符编码（UTF-8 直接可用），
;; 而在**字体**：系统里要有含中文字形的字体，且拉丁/中文两套字形的"字格
;; 高、下行"一致，逐字排版才不会上下错位。
;;
;; 本步新增（1 组，同属"中文字体"这一件事）：
;;   cjk-ok? —— 逐个候选字体测试：能否画出"中"的墨迹（不是豆腐块）+ 度量一致
;;
;; ★为什么中文要挑字体：Racket 默认字体可能不含中文字形（中文显示成方块
;;   "豆腐"），或者拉丁字母和中文字形来自两个 fallback、字格高度不同 →
;;   混排时中文那几格会上下错位。所以先测再选。
;;
;; 本步视觉：两行文字——拉丁+CJK 混排、纯中文，都在同一张字帖上正确显示。
;; =========================================================

(require "lib-gui.rkt")
(require "lib.rkt")

(define FNT-SIZE 48)
(define ATLAS-W 1024)
(define GAP 4)
(define display-strings (list "GLYPH ATLAS TEXT" "中英文混排 latin + CJK" "你好 字帖支持中文"))

;; 候选字体：要同时含拉丁与中文字形，且度量一致（按常见程度排）
(define font-candidates
  (list "WenQuanYi Micro Hei" "WenQuanYi Zen Hei"
        "Noto Sans CJK SC" "Noto Sans CJK SC Regular"
        "Droid Sans Fallback" "Microsoft YaHei"
        "PingFang SC" "Source Han Sans SC"))

;; 字体可用性：能画出"中"的墨迹（不是豆腐块），且 A 与 中 的格高/下行一致
(define (cjk-ok? face)
  (with-handlers ([exn:fail? (lambda (e) #f)])
    (define f (make-object font% FNT-SIZE face))
    (define (ext s)
      (define bm (make-bitmap 2 2))
      (define dc (new bitmap-dc% (bitmap bm)))
      (call-with-values (lambda () (send dc get-text-extent s f)) list))
    (define eA (ext "A")) (define eZ (ext "中"))
    (define same-metric?
      (and (= (exact-round (list-ref eA 1)) (exact-round (list-ref eZ 1)))
           (= (exact-round (list-ref eA 2)) (exact-round (list-ref eZ 2)))))
    ;; 墨迹测试：把"中"画进 128×96 位图，数 alpha>120 的像素
    (define bm2 (make-bitmap 128 96))
    (send bm2 set-argb-pixels 0 0 128 96 (make-bytes (* 128 96 4) 0))
    (define dc2 (new bitmap-dc% (bitmap bm2)))
    (send dc2 set-text-foreground (make-object color% 255 255 255))
    (send dc2 set-font f)
    (send dc2 draw-text "中" 10 10)
    (define px (make-bytes (* 128 96 4)))
    (send bm2 get-argb-pixels 0 0 128 96 px)
    (define ink 0)
    (for ([i (in-range (* 128 96))])
      (when (> (bytes-ref px (* i 4)) 120) (set! ink (+ ink 1))))
    (and same-metric? (> ink 400))))

(define (pick-font)
  (for/or ([face font-candidates]) (and (cjk-ok? face) face)))

(define text-vert
  (glsl (version 330 core)
        (layout (location 0) in vec2 aPos)
        (layout (location 1) in vec2 aUV)
        (uniform mat4 uProj)
        (out vec2 vUV)
        (define (main) void
          (set! vUV aUV)
          (set! gl_Position (* uProj (vec4 aPos 0.0 1.0))))))
(define text-frag
  (glsl (version 330 core)
        (in vec2 vUV)
        (uniform sampler2D uFont)
        (uniform vec4 uColor)
        (out vec4 FragColor)
        (define (main) void
          (float cov (r (texture uFont vUV)))
          (set! FragColor (vec4 (rgb uColor) (* (a uColor) cov))))))

(define (build-glyph-atlas)
  (define chars
    (remove-duplicates
     (append (for/list ([i (in-range 32 127)]) (integer->char i))
             (apply append (map string->list display-strings)))))
  (define picked (pick-font))
  (define fnt (if picked
                  (make-object font% FNT-SIZE picked 'default)
                  (make-object font% FNT-SIZE 'default)))
  (printf "字帖字体：~a~%" (or picked "default（未找到带中文的字体！）"))
  (define probe (make-bitmap 2 2))
  (define pdc (new bitmap-dc% (bitmap probe)))
  (send pdc set-font fnt)
  (define (ext s) (call-with-values (lambda () (send pdc get-text-extent s fnt)) list))
  (define h0 (exact-round (list-ref (ext "A") 1)))
  (define des (exact-round (list-ref (ext "A") 2)))
  (define asc (- h0 des))
  (define placed '())
  (define x 0) (define y 0)
  (for ([ch chars])
    (define w (exact-round (car (ext (string ch)))))
    (when (> (+ x w) ATLAS-W) (set! y (+ y h0 GAP)) (set! x 0))
    (set! placed (cons (list ch x y w) placed))
    (set! x (+ x w GAP)))
  (define ah (+ y h0 GAP))
  (define bm (make-bitmap ATLAS-W ah))
  (send bm set-argb-pixels 0 0 ATLAS-W ah (make-bytes (* ATLAS-W ah 4) 0))
  (define dc (new bitmap-dc% (bitmap bm)))
  (send dc set-text-foreground (make-object color% 255 255 255))
  (send dc set-font fnt)
  (for ([p (reverse placed)])
    (send dc draw-text (string (list-ref p 0)) (list-ref p 1) (list-ref p 2)))
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
  (define glyphs (make-hash))
  (for ([p placed])
    (define ch (list-ref p 0)) (define cx (list-ref p 1))
    (define cy (list-ref p 2)) (define w (list-ref p 3))
    (hash-set! glyphs ch
               (list (exact->inexact (/ cx ATLAS-W)) (exact->inexact (/ cy ah))
                     (exact->inexact (/ (+ cx w) ATLAS-W)) (exact->inexact (/ (+ cy h0) ah))
                     (exact->inexact w))))
  (values tex glyphs h0 asc))

(define (string-quads s glyphs h0 asc x by scale)
  (define y0 (- by (* asc scale)))
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
       (set! parts (append parts
                           (list px y0 u0 v0   x1 y0 u1 v0   x1 y1 u1 v1
                                 px y0 u0 v0   x1 y1 u1 v1   px y1 u0 v1)))
       (set! px x1)]
      [else (set! px (+ px (* scale (list-ref (hash-ref glyphs #\space) 4))))]))
  (values (apply f32vector parts) (quotient (length parts) 4)))

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
  (glDisable GL_DEPTH_TEST)
  (glEnable GL_BLEND)
  (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
  (glUseProgram prog-text)
  (glActiveTexture GL_TEXTURE0)
  (glBindTexture GL_TEXTURE_2D tex-atlas)
  (glUniform1i loc-font 0)
  (glUniformMatrix4fv loc-proj 1 #f
                      (mat4 (m4-ortho 0.0 (exact->inexact w) (exact->inexact h) 0.0 -1.0 1.0)))
  (draw-text "GLYPH ATLAS TEXT" 24.0 80.0 0.6 '(0.95 0.85 0.30 1.0) glyphs font-h0 font-asc)
  (draw-text "中英文混排 latin + CJK" 26.0 150.0 0.5 '(0.75 0.80 0.95 1.0) glyphs font-h0 font-asc)
  (draw-text "你好 字帖支持中文" 26.0 220.0 0.5 '(0.45 0.95 0.90 1.0) glyphs font-h0 font-asc)
  (glDisable GL_BLEND))

(define-values (frame canvas)
  (make-window #:title "16-02 中文文本" #:width 800 #:height 400 #:draw draw))

(define prog-text (send canvas with-gl-context (lambda () (build-program text-vert text-frag))))
(define loc-proj  (send canvas with-gl-context (lambda () (glGetUniformLocation prog-text "uProj"))))
(define loc-color (send canvas with-gl-context (lambda () (glGetUniformLocation prog-text "uColor"))))
(define loc-font  (send canvas with-gl-context (lambda () (glGetUniformLocation prog-text "uFont"))))
(define-values (tex-atlas glyphs font-h0 font-asc)
  (send canvas with-gl-context (lambda () (build-glyph-atlas))))
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
