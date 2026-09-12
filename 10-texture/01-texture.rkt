#lang racket/base
;; =========================================================
;; 10-texture/01-texture.rkt —— 第一步：上传纹理 + sampler2D
;; 运行：racket 10-texture/01-texture.rkt    点 X = 退出
;; =========================================================
;; 前几课每个顶点亲手填颜色。本步换成"贴图"：把一张图片变成 GPU 能读的纹理，
;; 片元着色器按 uv 去图上取色。照片级细节不可能逐顶点手填，这就是纹理的意义。
;;
;; 本步新增（2 组）：
;;   ① 纹理上传 —— read-bitmap 读图 → glTexImage2D 传上 GPU
;;   ② sampler2D + texture() —— 片元着色器里"采样"取色
;;
;; ★纹理 = 显存里的一张图。上传流程（本步主角，裸写）：
;;   ① read-bitmap 读 PNG → 位图
;;   ② 位图是 ARGB 字节序，重排成 RGBA（GL 要的顺序）
;;   ③ glGenTextures 生成纹理对象 → glBindTexture 绑定
;;   ④ glTexImage2D 把字节真正传上 GPU
;;   （glTexParameteri 的过滤/环绕参数本步先照抄，下一步讲它们的意义）
;;
;; ★采样：片元着色器里 uniform sampler2D + (texture uTex vUV) 在 uv 处取色。
;;   uv 是 04 课学过的 0..1 坐标，GPU 在三角形内插值，每个像素拿到自己的 uv
;;   → 去图上对应位置取颜色 → 图片就"贴"在了面上。
;;
;; 本步视觉：一个四边形（面朝相机）贴上 cube.png——深蓝底 + 橙圈 + 黄叉。
;; =========================================================

(require "lib-gui.rkt")
(require "lib.rkt")
(require racket/runtime-path)

;; 纹理文件：相对"本脚本所在目录"解析，任意目录下运行都能找到 assets/
(define-runtime-path cube-png "assets/cube.png")

(define vert-src
  (glsl (version 330 core)
        (layout (location 0) in vec3 aPos)
        (layout (location 1) in vec2 aUV)
        (uniform mat4 uMVP)
        (out vec2 vUV)
        (define (main) void
          (set! vUV aUV)
          (set! gl_Position (* uMVP (vec4 aPos 1.0))))))

;; 片元着色器：texture(uTex, vUV) 在 uv 处采样，得到该像素的颜色。
(define frag-src
  (glsl (version 330 core)
        (in vec2 vUV)
        (uniform sampler2D uTex)
        (out vec4 FragColor)
        (define (main) void
          (set! FragColor (texture uTex vUV)))))

;; 纹理加载器（裸写，本步主角）。wrap-mode='repeat/'clamp；mipmap?=是否生成多级图。
(define (load-tex path wrap-mode mipmap?)
  ;; ① 读图，拿到 RGBA 字节
  (define bm (read-bitmap path))
  (define w (send bm get-width)) (define h (send bm get-height))
  (define argb (make-bytes (* w h 4)))
  (send bm get-argb-pixels 0 0 w h argb)      ; 位图是 A,R,G,B 顺序
  (define rgba (make-bytes (* w h 4)))
  (for ([i (in-range (* w h))])               ; 重排成 R,G,B,A
    (bytes-set! rgba (* i 4)      (bytes-ref argb (+ (* i 4) 1)))
    (bytes-set! rgba (+ (* i 4) 1) (bytes-ref argb (+ (* i 4) 2)))
    (bytes-set! rgba (+ (* i 4) 2) (bytes-ref argb (+ (* i 4) 3)))
    (bytes-set! rgba (+ (* i 4) 3) (bytes-ref argb (* i 4))))
  ;; ② 生成纹理对象并上传
  (define tex (u32vector-ref (glGenTextures 1) 0))
  (glBindTexture GL_TEXTURE_2D tex)
  (glPixelStorei GL_UNPACK_ALIGNMENT 1)       ; 每行 4 字节对齐，别让 GL 猜错
  (glTexImage2D GL_TEXTURE_2D 0 GL_RGBA w h 0 GL_RGBA GL_UNSIGNED_BYTE rgba)
  ;; ③ 采样参数（本步先照抄，下一步讲）
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S
                   (if (eq? wrap-mode 'repeat) GL_REPEAT GL_CLAMP_TO_EDGE))
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T
                   (if (eq? wrap-mode 'repeat) GL_REPEAT GL_CLAMP_TO_EDGE))
  (if mipmap?
      (begin (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR_MIPMAP_LINEAR)
             (glGenerateMipmap GL_TEXTURE_2D))
      (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR))
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
  tex)

;; 四边形（面朝 z+）：位置 vec3 + uv vec2，交错 5 float
(define verts
  (concat-vecs (vec3 -1.0 -1.0 0.0) (vec2 0.0 0.0)
               (vec3  1.0 -1.0 0.0) (vec2 1.0 0.0)
               (vec3  1.0  1.0 0.0) (vec2 1.0 1.0)
               (vec3 -1.0  1.0 0.0) (vec2 0.0 1.0)))
(define idx (u16vector 0 1 2  0 2 3))

(define (draw)
  (define-values (w h) (send canvas get-gl-client-size))
  (define aspect (/ (exact->inexact w) (exact->inexact h)))
  (define P (mat4-perspective 45.0 aspect 0.1 100.0))
  (define V (mat4-look-at 0.0 0.0 3.0  0.0 0.0 0.0  0.0 1.0 0.0))  ; 相机正对四边形

  (glClearColor 0.10 0.11 0.17 1.0)
  (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
  (glUseProgram prog)
  ;; ★纹理接上采样器：采样器 = 单元 0，单元 0 上绑我们的纹理
  (glUniform1i loc-tex 0)
  (glActiveTexture GL_TEXTURE0)
  (glBindTexture GL_TEXTURE_2D tex)
  (glUniformMatrix4fv loc-mvp 1 #f (mat4-mult P V))
  (glBindVertexArray vao)
  (glDrawElements GL_TRIANGLES 6 GL_UNSIGNED_SHORT 0))

(define-values (frame canvas)
  (make-window #:title "10-01 纹理贴图" #:width 600 #:height 600 #:draw draw))

(define prog (send canvas with-gl-context (lambda () (build-program (GL_VERTEX_SHADER vert-src) (GL_FRAGMENT_SHADER frag-src)))))
(define loc-mvp (send canvas with-gl-context (lambda () (glGetUniformLocation prog "uMVP"))))
(define loc-tex (send canvas with-gl-context (lambda () (glGetUniformLocation prog "uTex"))))
(define tex (send canvas with-gl-context (lambda () (load-tex cube-png 'clamp #f))))
(define vao
  (send canvas with-gl-context
        (lambda ()
          (glEnable GL_DEPTH_TEST)
          (define vbo (u32vector-ref (glGenBuffers 1) 0))
          (glBindBuffer GL_ARRAY_BUFFER vbo)
          (glBufferData GL_ARRAY_BUFFER (gl-vector-sizeof verts) verts GL_STATIC_DRAW)
          (define v (u32vector-ref (glGenVertexArrays 1) 0))
          (glBindVertexArray v)
          (glVertexAttribPointer 0 (glsl-size 'vec3) GL_FLOAT #f (glsl-stride-bytes 'vec3 'vec2) 0)
          (glEnableVertexAttribArray 0)
          (glVertexAttribPointer 1 (glsl-size 'vec2) GL_FLOAT #f (glsl-stride-bytes 'vec3 'vec2) (glsl-stride-bytes 'vec3))
          (glEnableVertexAttribArray 1)
          (define ebo (u32vector-ref (glGenBuffers 1) 0))
          (glBindBuffer GL_ELEMENT_ARRAY_BUFFER ebo)
          (glBufferData GL_ELEMENT_ARRAY_BUFFER (gl-vector-sizeof idx) idx GL_STATIC_DRAW)
          (glBindVertexArray 0)
          v)))

(send frame show #t)
