#lang racket/base
;; =========================================================
;; 17-text/03-demo.rkt —— 第三步：综合，3D 场景 + 屏幕文字
;; 运行：racket glsl/17-text/03-demo.rkt     T = 文字开/关   点X = 退出
;; =========================================================
;; 本课前两步：烤字帖画文字(01)、中文(02)。本步**不引入新语法**，把老教程
;; 15-text 的成品拼出来：3D 场景 + 屏幕文字 + FPS + 3D 标签投影。
;; 文字是"第二遍叠加"（先画 3D，再叠一层文字），和 14 课 FBO 两遍同思路。
;; =========================================================

(require "lib-gui.rkt")
(require "lib.rkt")

(define PI (acos -1.0))
(define start-ms (current-inexact-milliseconds))
(define text-on? (box #t))
(define FNT-SIZE 48) (define ATLAS-W 1024) (define GAP 4)
(define display-strings (list "GLYPH ATLAS TEXT" "one texture, latin + CJK 中英文混排"
                              "你好 字帖支持中文" "立方体 #1" "T 文字开/关  ESC 退出"))
(define font-candidates
  (list "WenQuanYi Micro Hei" "WenQuanYi Zen Hei"
        "Noto Sans CJK SC" "Noto Sans CJK SC Regular"
        "Droid Sans Fallback" "Microsoft YaHei" "PingFang SC" "Source Han Sans SC"))

(define (cjk-ok? face)
  (with-handlers ([exn:fail? (lambda (e) #f)])
    (define f (make-object font% FNT-SIZE face))
    (define (ext s)
      (define bm (make-bitmap 2 2))
      (define dc (new bitmap-dc% (bitmap bm)))
      (call-with-values (lambda () (send dc get-text-extent s f)) list))
    (define eA (ext "A")) (define eZ (ext "中"))
    (define same?
      (and (= (exact-round (list-ref eA 1)) (exact-round (list-ref eZ 1)))
           (= (exact-round (list-ref eA 2)) (exact-round (list-ref eZ 2)))))
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
    (and same? (> ink 400))))

(define (pick-font) (for/or ([f font-candidates]) (and (cjk-ok? f) f)))

;; ---- shader ----
(define cube-vert
  (glsl (version 330 core)
        (layout (location 0) in vec3 aPos)
        (layout (location 1) in vec3 aColor)
        (uniform mat4 uMVP)
        (out vec3 vColor)
        (define (main) void
          (set! vColor aColor)
          (set! gl_Position (* uMVP (vec4 aPos 1.0))))))
(define cube-frag
  (glsl (version 330 core)
        (in vec3 vColor)
        (out vec4 FragColor)
        (define (main) void
          (set! FragColor (vec4 vColor 1.0)))))
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

;; ---- 字帖（同 02 步）----
(define (build-glyph-atlas)
  (define chars
    (remove-duplicates
     (append (for/list ([i (in-range 32 127)]) (integer->char i))
             (apply append (map string->list display-strings)))))
  (define picked (pick-font))
  (define fnt (if picked (make-object font% FNT-SIZE picked 'default)
                  (make-object font% FNT-SIZE 'default)))
  (define probe (make-bitmap 2 2))
  (define pdc (new bitmap-dc% (bitmap probe)))
  (send pdc set-font fnt)
  (define (ext s) (call-with-values (lambda () (send pdc get-text-extent s fnt)) list))
  (define h0 (exact-round (list-ref (ext "A") 1)))
  (define des (exact-round (list-ref (ext "A") 2)))
  (define asc (- h0 des))
  (define placed '()) (define x 0) (define y 0)
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
  (for ([p (reverse placed)]) (send dc draw-text (string (list-ref p 0)) (list-ref p 1) (list-ref p 2)))
  (define argb (make-bytes (* ATLAS-W ah 4)))
  (send bm get-argb-pixels 0 0 ATLAS-W ah argb)
  (define alpha (make-bytes (* ATLAS-W ah)))
  (for ([i (in-range (* ATLAS-W ah))]) (bytes-set! alpha i (bytes-ref argb (* i 4))))
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
    (hash-set! glyphs ch (list (exact->inexact (/ cx ATLAS-W)) (exact->inexact (/ cy ah))
                               (exact->inexact (/ (+ cx w) ATLAS-W)) (exact->inexact (/ (+ cy h0) ah))
                               (exact->inexact w))))
  (values tex glyphs h0 asc))

(define (string-width s glyphs scale)
  (for/sum ([ch (in-string s)])
    (* scale (list-ref (hash-ref glyphs ch (hash-ref glyphs #\space)) 4))))

(define (string-quads s glyphs h0 asc x by scale)
  (define y0 (- by (* asc scale))) (define y1 (+ y0 (* h0 scale)))
  (define parts '()) (define px x)
  (for ([ch (in-string s)])
    (define g (hash-ref glyphs ch #f))
    (cond
      [g
       (define u0 (list-ref g 0)) (define v0 (list-ref g 1))
       (define u1 (list-ref g 2)) (define v1 (list-ref g 3))
       (define wpx (* (list-ref g 4) scale)) (define x1 (+ px wpx))
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

(define (draw-text s x by scale color)
  (glUniform4f loc-color (list-ref color 0) (list-ref color 1)
               (list-ref color 2) (list-ref color 3))
  (define-values (data n) (string-quads s glyphs font-h0 font-asc x by scale))
  (glBindVertexArray vao-text)
  (glBindBuffer GL_ARRAY_BUFFER vbo-text)
  (glBufferData GL_ARRAY_BUFFER (gl-vector-sizeof data) data GL_DYNAMIC_DRAW)
  (glDrawArrays GL_TRIANGLES 0 n))

;; 3D 点 (x,y,z) 经 MVP 投影到屏幕像素（GLSL 里就是 (mvp * vec4(x y z 1.0)) 的透视除法）
(define (project-to-screen m px py pz gw gh)
  (define clip (mat4-mult-vec4 m (vec4 px py pz 1.0)))
  (define w (f32vector-ref clip 3))
  (if (<= w 0.0) #f
      (let ([cx (/ (f32vector-ref clip 0) w)]
            [cy (/ (f32vector-ref clip 1) w)])
        (values (* (+ (* cx 0.5) 0.5) gw) (* (- 0.5 (* cy 0.5)) gh)))))

;; FPS
(define fps-str (box "FPS --"))
(define frame-count 0) (define fps-t0 (current-inexact-milliseconds))

(define (on-char e)
  (define code (send e get-key-code))
  (when (not (eq? code 'release))
    (cond
      [(or (eq? code #\t) (eq? code #\T))
       (set-box! text-on? (not (unbox text-on?)))
       (printf (if (unbox text-on?) "文字 开~%" "文字 关~%"))]
      [(eq? code 'escape) (exit 0)])))

(define (draw)
  (define t (/ (- (current-inexact-milliseconds) start-ms) 1000.0))
  (define-values (w h) (send canvas get-gl-client-size))
  (define aspect (/ (exact->inexact w) (exact->inexact h)))
  (define P (mat4-perspective 45.0 aspect 0.1 100.0))
  (define V (mat4-mult (mat4-translate 0.0 0.0 -6.0) (mat4-rot-y 20.0)))
  (define M1 (mat4-mult (mat4-rot-y (* t 70.0)) (mat4-rot-x (* t 45.0))))

  ;; ---- 第 1 遍：3D 场景 ----
  (glViewport 0 0 w h)
  (glEnable GL_DEPTH_TEST)
  (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
  (glClearColor 0.08 0.09 0.14 1.0)
  (glUseProgram prog-scene)
  (define (draw-cube-at M)
    (glUniformMatrix4fv loc-mvp 1 #f (mat4-mult (mat4-mult P V) M))
    (glBindVertexArray vao-cube)
    (glDrawElements GL_TRIANGLES 36 GL_UNSIGNED_SHORT 0))
  (draw-cube-at M1)
  (for ([k (in-range 3)])
    (define a (* (/ PI 180.0) (+ (* k 120.0) (* t 90.0))))
    (draw-cube-at (mat4-mult (mat4-translate (* 2.6 (cos a)) 0.0 (- (* 2.6 (sin a))))
                           (mat4-rot-y (* t -60.0)))))

  ;; ---- 第 2 遍：屏幕文字 ----
  (glDisable GL_DEPTH_TEST)
  (glEnable GL_BLEND)
  (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
  (glUseProgram prog-text)
  (glActiveTexture GL_TEXTURE0)
  (glBindTexture GL_TEXTURE_2D tex-atlas)
  (glUniform1i loc-font 0)
  (glUniformMatrix4fv loc-proj 1 #f
                       (mat4-ortho 0.0 (exact->inexact w) (exact->inexact h) 0.0 -1.0 1.0))
  (when (unbox text-on?)
    (draw-text "GLYPH ATLAS TEXT" 16.0 62.0 0.62 '(0.95 0.85 0.30 1.0))
    (draw-text "one texture, latin + CJK 中英文混排" 18.0 92.0 0.40 '(0.75 0.80 0.95 1.0))
    (define zh "你好 字帖支持中文")
    (draw-text zh (- (exact->inexact w) 16.0 (string-width zh glyphs 0.40))
               92.0 0.40 '(0.45 0.95 0.90 1.0))
    ;; 3D 标签：把立方体中心投影到屏幕
    (define mvp1 (mat4-mult (mat4-mult P V) M1))
    (define-values (sx sy) (project-to-screen mvp1 0.0 0.0 0.0 w h))
    (define label "立方体 #1")
    (define ls 0.55)
    (draw-text label (- sx (/ (string-width label glyphs ls) 2.0)) (- sy 160.0) ls '(0.95 0.95 0.95 1.0))
    ;; FPS（左下）
    (draw-text (unbox fps-str) 16.0 (- (exact->inexact h) 16.0) 0.50 '(0.45 0.90 0.55 1.0))
    ;; 提示（右下）
    (define hint "T 文字开/关  ESC 退出")
    (draw-text hint (- (exact->inexact w) 16.0 (string-width hint glyphs 0.40))
               (- (exact->inexact h) 20.0) 0.40 '(0.60 0.62 0.68 1.0)))
  (glDisable GL_BLEND))

(define-values (frame canvas)
  (make-window #:title "17-03 3D + 文字（综合）"
               #:width 800 #:height 600 #:draw draw #:on-char on-char))

(define prog-scene (send canvas with-gl-context (lambda () (build-program (GL_VERTEX_SHADER cube-vert) (GL_FRAGMENT_SHADER cube-frag)))))
(define prog-text  (send canvas with-gl-context (lambda () (build-program (GL_VERTEX_SHADER text-vert) (GL_FRAGMENT_SHADER text-frag)))))
(define loc-mvp   (send canvas with-gl-context (lambda () (glGetUniformLocation prog-scene "uMVP"))))
(define loc-proj  (send canvas with-gl-context (lambda () (glGetUniformLocation prog-text "uProj"))))
(define loc-color (send canvas with-gl-context (lambda () (glGetUniformLocation prog-text "uColor"))))
(define loc-font  (send canvas with-gl-context (lambda () (glGetUniformLocation prog-text "uFont"))))
(define-values (tex-atlas glyphs font-h0 font-asc)
  (send canvas with-gl-context (lambda () (build-glyph-atlas))))
(define vao-cube
  (send canvas with-gl-context
        (lambda ()
          (define vbo (u32vector-ref (glGenBuffers 1) 0))
          (glBindBuffer GL_ARRAY_BUFFER vbo)
          (glBufferData GL_ARRAY_BUFFER (gl-vector-sizeof cube-verts) cube-verts GL_STATIC_DRAW)
          (define v (u32vector-ref (glGenVertexArrays 1) 0))
          (glBindVertexArray v)
          (glVertexAttribPointer 0 (glsl-size 'vec3) GL_FLOAT #f (glsl-stride-bytes 'vec3 'vec3) 0)
          (glEnableVertexAttribArray 0)
          (glVertexAttribPointer 1 (glsl-size 'vec3) GL_FLOAT #f (glsl-stride-bytes 'vec3 'vec3) (glsl-stride-bytes 'vec3))
          (glEnableVertexAttribArray 1)
          (define ebo (u32vector-ref (glGenBuffers 1) 0))
          (glBindBuffer GL_ELEMENT_ARRAY_BUFFER ebo)
          (glBufferData GL_ELEMENT_ARRAY_BUFFER (gl-vector-sizeof cube-idx) cube-idx GL_STATIC_DRAW)
          (glBindVertexArray 0)
          v)))
(define-values (vao-text vbo-text)
  (send canvas with-gl-context
        (lambda ()
          (define v (u32vector-ref (glGenVertexArrays 1) 0))
          (define b (u32vector-ref (glGenBuffers 1) 0))
          (glBindVertexArray v)
          (glBindBuffer GL_ARRAY_BUFFER b)
          (glBufferData GL_ARRAY_BUFFER (* 8192 4) #f GL_DYNAMIC_DRAW)
          (glVertexAttribPointer 0 (glsl-size 'vec2) GL_FLOAT #f (glsl-stride-bytes 'vec2 'vec2) 0)
          (glEnableVertexAttribArray 0)
          (glVertexAttribPointer 1 (glsl-size 'vec2) GL_FLOAT #f (glsl-stride-bytes 'vec2 'vec2) (glsl-stride-bytes 'vec2))
          (glEnableVertexAttribArray 1)
          (glBindVertexArray 0)
          (values v b))))

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
            (set! frame-count 0) (set! fps-t0 now))
          (send canvas refresh)))))

(send frame show #t)
(send canvas focus)
