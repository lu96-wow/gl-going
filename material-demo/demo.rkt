#lang racket/base
;; ============================================================
;; demo.rkt —— 真窗口 demo：左半 = 裸 GL，右半 = material
;;
;; 运行：racket material-demo/demo.rkt     （点 X 退出）
;;
;; 同一个思路画两次，只是为了并排看两种“代码形状”：
;;   左边：手动写 uniform 名字 + 手动 glGetUniformLocation + 手动挑 glUniform*
;;   右边：material 全部自动（名字/类型从 shader 读，loc 自动查）
;; ============================================================

(require racket/list
         "window.rkt"
         "material.rkt"
         "../racket-glsl/tool.rkt"           ; build-program
         "../racket-glsl/rewrite.rkt"        ; (glsl ...)
         "../racket-glsl/rename-vector.rkt") ; vec / vec2

;; ---------- 着色器（注意：uniform 就声明在这里，material 之后自己读）----------

;; 顶点：位置 + uOffset（把方块推开）
(define vs
  (glsl (version 330 core)
        (layout (location 0) in vec2 aPos)
        (uniform vec2 uOffset)
        (define (main) void
          (set! gl_Position (vec4 (+ aPos uOffset) 0.0 1.0)))))

;; 片元 A：纯色
(define fs-solid
  (glsl (version 330 core)
        (uniform vec4 uColor)
        (out vec4 FragColor)
        (define (main) void (set! FragColor uColor))))

;; 片元 B：颜色 × uPulse（随时间脉动）
(define fs-pulse
  (glsl (version 330 core)
        (uniform vec4 uColor)
        (uniform float uPulse)
        (out vec4 FragColor)
        (define (main) void (set! FragColor (* uColor uPulse)))))

;; ---------- 几何：一个 0.5 大小的方块（两个三角形，6 顶点）----------
(define verts
  (vec (vec2 -0.5 -0.5) (vec2 0.5 -0.5) (vec2 0.5 0.5)
       (vec2 -0.5 -0.5) (vec2 0.5 0.5) (vec2 -0.5 0.5)))

(define start-ms (current-inexact-milliseconds))

;; ---------- 每帧 ----------
(define (draw)
  (define t (/ (- (current-inexact-milliseconds) start-ms) 1000.0))
  (glClearColor 0.08 0.09 0.14 1.0)
  (glClear GL_COLOR_BUFFER_BIT)
  (glBindVertexArray vao)

  ;; ===== 左：裸 GL =====
  ;; program 手动绑；用整数 loc；手动挑 glUniform2f / glUniform4f
  (glUseProgram prog-solid)
  (glUniform2f loc-solid-offset -0.5 (exact->inexact (* 0.12 (sin t))))
  (glUniform4f loc-solid-color 0.95 0.35 0.35 1.0)
  (glDrawArrays GL_TRIANGLES 0 6)

  ;; ===== 右：material =====
  ;; 只写名字；loc 由对象内部拿；setter 由 shader 声明的类型自动挑
  (with-material mat-pulse
    (material-set! mat-pulse 'uOffset '(0.5 0.0))
    (material-set! mat-pulse 'uColor '(0.35 0.75 1.0 1.0))
    (material-set! mat-pulse 'uPulse (+ 0.55 (* 0.45 (sin (* 2.0 t)))))
    (glDrawArrays GL_TRIANGLES 0 6)))

;; ---------- 窗口 + 初始化（初始化都要在 GL 上下文里）----------
(define-values (frame canvas)
  (make-window #:title "material demo：左=裸 GL  右=material"
               #:width 640 #:height 480 #:draw draw))

;; —— 裸 GL 这边需要的东西：program + 逐个手动查的 loc ——
(define prog-solid
  (send canvas with-gl-context
        (lambda () (build-program (GL_VERTEX_SHADER vs) (GL_FRAGMENT_SHADER fs-solid)))))
(define loc-solid-offset
  (send canvas with-gl-context (lambda () (glGetUniformLocation prog-solid "uOffset"))))
(define loc-solid-color
  (send canvas with-gl-context (lambda () (glGetUniformLocation prog-solid "uColor"))))

;; —— material 这边：编译链接 + 从 shader 自动读 uniform 名/类型 + 自动查 loc ——
(define mat-pulse
  (send canvas with-gl-context
        (lambda ()
          (make-material 'pulse (list (list GL_VERTEX_SHADER vs)
                                      (list GL_FRAGMENT_SHADER fs-pulse))))))
(printf "material `pulse' 自动读到的 uniform：~a\n"
        (sort (hash-keys (material-uniforms mat-pulse)) string<?))

;; —— 顶点缓冲 + VAO ——
(define vao
  (send canvas with-gl-context
        (lambda ()
          (define vbo (u32vector-ref (glGenBuffers 1) 0))
          (glBindBuffer GL_ARRAY_BUFFER vbo)
          (define data (vec->f32vector verts))
          (glBufferData GL_ARRAY_BUFFER (* 4 (f32vector-length data)) data GL_STATIC_DRAW)
          (define v (u32vector-ref (glGenVertexArrays 1) 0))
          (glBindVertexArray v)
          (glVertexAttribPointer 0 2 GL_FLOAT #f 8 0)
          (glEnableVertexAttribArray 0)
          (glBindVertexArray 0)
          v)))

;; 16ms 定时刷新 → 动画
(define ticker (new timer% (interval 16)
                       (notify-callback (lambda () (send canvas refresh)))))
(send frame show #t)
