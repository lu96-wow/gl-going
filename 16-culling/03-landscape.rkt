#lang racket/base
;; =========================================================
;; 16-culling/03-landscape.rkt —— 第三步：可见性处理的方法谱系
;; 运行：racket 16-culling/03-landscape.rkt     R = 自转开/关   点X = 退出
;; =========================================================
;; 前两步学会了背面剔除。本步退一步看全景：**面剔除只是可见性处理的其中一种**。
;;
;; ★可见性处理的方法谱系——"谁该被画"在不同粒度上各有方法：
;;
;;   像素级   深度测试 z-buffer（08 课）：每个片元画之前比 z，远的画不过近的。
;;            管"前后遮挡"，最通用。
;;   三角形级 背面剔除（本课）：按绕序扔掉背对相机的三角形。对封闭实体是
;;            纯优化（省片元着色器），对单面纸片则直接决定可见性。
;;   物体级   视锥剔除 frustum culling：相机视野外的物体整块不提交。
;;            CPU 侧算包围盒 vs 视锥，管"视野外的东西"。
;;   物体级   遮挡剔除 occlusion culling：被前面物体完全挡住的物体不画。
;;            最复杂（要判断"完全被挡"），但能省最多。
;;   老方法   画家算法（12 课透明）：按距离排序、从远到近画。不需要深度缓冲，
;;            但排序贵、且互相穿插时会出错。
;;
;;   还有 GPU 硬件层的 Early-Z / hierarchical-Z：片元着色器之前先粗测深度，
;;   跳过被挡的片元。它们在不同粒度上互补，没有"唯一正确"的方法。
;;
;; 本步视觉：一张单面纸片（CCW）。按 R 让它自转——转到背面朝相机时，纸片
;;   "消失"（背面被剔除）。这就是背面剔除对**开放表面**的意义：它不是隐藏
;;   的性能优化，而是可见性规则本身。（对封闭立方体则相反：视觉不变、纯省事。）
;; =========================================================

(require "lib-gui.rkt")
(require "lib.rkt")

(define start-ms (current-inexact-milliseconds))
(define spin? (box #f))

(define vert-src
  (glsl (version 330 core)
        (layout (location 0) in vec3 aPos)
        (layout (location 1) in vec3 aColor)
        (uniform mat4 uMVP)
        (out vec3 vColor)
        (define (main) void
          (set! vColor aColor)
          (set! gl_Position (* uMVP (vec4 aPos 1.0))))))
(define frag-src
  (glsl (version 330 core)
        (in vec3 vColor)
        (out vec4 FragColor)
        (define (main) void
          (set! FragColor (vec4 vColor 1.0)))))

(define (quad-verts corners color)
  (apply concat-vecs
         (apply append
                (for/list ([c corners])
                  (list (vec3 (car c) (cadr c) 0.0) color)))))
(define idx (u16vector 0 1 2  0 2 3))
(define ccw-corners (list (list -1.1 -0.75) (list 1.1 -0.75)
                          (list 1.1 0.75) (list -1.1 0.75)))

(define (on-char e)
  (define code (send e get-key-code))
  (when (not (eq? code 'release))
    (cond
      [(or (eq? code #\r) (eq? code #\R))
       (set-box! spin? (not (unbox spin?)))
       (printf (if (unbox spin?) "自转 开（转到背面就消失）~%" "自转 关~%"))]
      [(eq? code 'escape) (exit 0)])))

(define (draw)
  (define t (/ (- (current-inexact-milliseconds) start-ms) 1000.0))
  (define-values (w h) (send canvas get-gl-client-size))
  (define aspect (/ (exact->inexact w) (exact->inexact h)))
  (define P (mat4-perspective 45.0 aspect 0.1 100.0))
  (define V (mat4-translate 0.0 0.0 -5.0))

  (glEnable GL_CULL_FACE)   ; 剔除始终开
  (glClearColor 0.08 0.09 0.14 1.0)
  (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
  (glUseProgram prog)
  (define yaw (if (unbox spin?) (* t 45.0) -15.0))
  (glUniformMatrix4fv loc-mvp 1 #f
                       (mat4-mult (mat4-mult P V) (mat4-rot-y yaw)))
  (glBindVertexArray vao)
  (glDrawElements GL_TRIANGLES 6 GL_UNSIGNED_SHORT 0))

(define-values (frame canvas)
  (make-window #:title "16-03 可见性谱系（R 自转）"
               #:width 800 #:height 600 #:draw draw #:on-char on-char))

(define prog (send canvas with-gl-context (lambda () (build-program (GL_VERTEX_SHADER vert-src) (GL_FRAGMENT_SHADER frag-src)))))
(define loc-mvp (send canvas with-gl-context (lambda () (glGetUniformLocation prog "uMVP"))))
(define vao
  (send canvas with-gl-context
        (lambda ()
          (glEnable GL_DEPTH_TEST)
          (define verts (quad-verts ccw-corners (vec3 0.20 0.55 0.95)))
          (define vbo (u32vector-ref (glGenBuffers 1) 0))
          (glBindBuffer GL_ARRAY_BUFFER vbo)
          (glBufferData GL_ARRAY_BUFFER (gl-vector-sizeof verts) verts GL_STATIC_DRAW)
          (define v (u32vector-ref (glGenVertexArrays 1) 0))
          (glBindVertexArray v)
          (glVertexAttribPointer 0 (glsl-size 'vec3) GL_FLOAT #f (glsl-stride-bytes 'vec3 'vec3) 0)
          (glEnableVertexAttribArray 0)
          (glVertexAttribPointer 1 (glsl-size 'vec3) GL_FLOAT #f (glsl-stride-bytes 'vec3 'vec3) (glsl-stride-bytes 'vec3))
          (glEnableVertexAttribArray 1)
          (define ebo (u32vector-ref (glGenBuffers 1) 0))
          (glBindBuffer GL_ELEMENT_ARRAY_BUFFER ebo)
          (glBufferData GL_ELEMENT_ARRAY_BUFFER (gl-vector-sizeof idx) idx GL_STATIC_DRAW)
          (glBindVertexArray 0)
          v)))

(define ticker
  (new timer% (interval 16)
       (notify-callback (lambda () (send canvas refresh)))))

(send frame show #t)
(send canvas focus)
