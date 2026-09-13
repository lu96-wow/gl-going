#lang racket/base

;; ============================================================
;; opengl-rename.rkt —— OpenGL 命名统一层（kebab-case）
;;
;; opengl 包沿用的是 C 的原始命名，两种风格混在一起：
;;   函数：glGenBuffers、glBufferData、glDrawArrays（驼峰式）
;;   常量：GL_ARRAY_BUFFER、GL_TRIANGLES（全大写 + 下划线）
;;
;; 教程统一改成 Racket 习惯的 kebab-case（全小写 + 连字符）：
;;   函数：glGenBuffers    →  gl-gen-buffers
;;         glDrawArrays    →  gl-draw-arrays
;;   常量：GL_ARRAY_BUFFER →  gl-array-buffer
;;         GL_TRIANGLES    →  gl-triangles
;;
;; 换算规则（三条）：
;;   1. 常量：去掉 GL_ 前缀 → 全小写 → 下划线换成连字符
;;        GL_ARRAY_BUFFER → array-buffer → gl-array-buffer
;;   2. 函数：驼峰拆成单词 → 全小写 → 连字符连接
;;        glGenBuffers → gen buffers → gl-gen-buffers
;;   3. 类型后缀（数字 + 字母，如 1f / 3f / iv / fv）算一个单词
;;        glUniform1f   → gl-uniform-1f
;;        glGetShaderiv → gl-get-shader-iv
;;
;; 用法：require 本模块后，代码里只写 kebab-case 的 gl-* 名字；
;;       原始驼峰名不再直接出现在教程代码里。
;;       需要某个还没收录的函数/常量时，按上面三条规则把它补进下面两段。
;; double 精度（dvec/dmat/double）的专属函数也已收录。教程主线用 float，这些先备着。
;; ============================================================

(require opengl)

(provide
 ;; ---------- 函数 ----------
 gl-attach-shader
 gl-bind-buffer
 gl-bind-vertex-array
 gl-buffer-data
 gl-clear
 gl-clear-color
 gl-compile-shader
 gl-create-program
 gl-create-shader
 gl-delete-program
 gl-delete-shader
 gl-draw-arrays
 gl-enable-vertex-attrib-array
 gl-gen-buffers
 gl-gen-vertex-arrays
 gl-get-program-info-log
 gl-get-program-iv
 gl-get-shader-info-log
 gl-get-shader-iv
 gl-get-uniform-location
 gl-link-program
 gl-shader-source
 gl-uniform-1f
 gl-uniform-1d
 gl-uniform-2d
 gl-uniform-3d
 gl-uniform-4d
 gl-uniform-matrix-2dv
 gl-uniform-matrix-3dv
 gl-uniform-matrix-4dv
 gl-use-program
 gl-vertex-attrib-pointer
 gl-vertex-attrib-l-pointer
 gl-vertex-attrib-l-1d
 gl-vertex-attrib-l-2d
 gl-vertex-attrib-l-3d
 gl-vertex-attrib-l-4d
 gl-viewport
 ;; ---------- 常量 ----------
 gl-array-buffer
 gl-color-buffer-bit
 gl-compile-status
 gl-double
 gl-dynamic-draw
 gl-float
 gl-fragment-shader
 gl-geometry-shader
 gl-info-log-length
 gl-link-status
 gl-static-draw
 gl-stream-draw
 gl-tess-control-shader
 gl-tess-evaluation-shader
 gl-triangles
 gl-vertex-shader
 ;; ---------- 辅助函数（opengl 包本就 kebab-case，直接转出） ----------
 gl-vector-sizeof)

;; ---------- 函数：驼峰 → kebab-case ----------

(define gl-attach-shader            glAttachShader)
(define gl-bind-buffer              glBindBuffer)
(define gl-bind-vertex-array        glBindVertexArray)
(define gl-buffer-data              glBufferData)
(define gl-clear                    glClear)
(define gl-clear-color              glClearColor)
(define gl-compile-shader           glCompileShader)
(define gl-create-program           glCreateProgram)
(define gl-create-shader            glCreateShader)
(define gl-delete-program           glDeleteProgram)
(define gl-delete-shader            glDeleteShader)
(define gl-draw-arrays              glDrawArrays)
(define gl-enable-vertex-attrib-array glEnableVertexAttribArray)
(define gl-gen-buffers              glGenBuffers)
(define gl-gen-vertex-arrays        glGenVertexArrays)
(define gl-get-program-info-log     glGetProgramInfoLog)
(define gl-get-program-iv           glGetProgramiv)
(define gl-get-shader-info-log      glGetShaderInfoLog)
(define gl-get-shader-iv            glGetShaderiv)
(define gl-get-uniform-location     glGetUniformLocation)
(define gl-link-program             glLinkProgram)
(define gl-shader-source            glShaderSource)
(define gl-uniform-1f               glUniform1f)
(define gl-uniform-1d               glUniform1d)
(define gl-uniform-2d               glUniform2d)
(define gl-uniform-3d               glUniform3d)
(define gl-uniform-4d               glUniform4d)
(define gl-uniform-matrix-2dv       glUniformMatrix2dv)
(define gl-uniform-matrix-3dv       glUniformMatrix3dv)
(define gl-uniform-matrix-4dv       glUniformMatrix4dv)
(define gl-use-program              glUseProgram)
(define gl-vertex-attrib-pointer    glVertexAttribPointer)
(define gl-vertex-attrib-l-pointer  glVertexAttribLPointer)
(define gl-vertex-attrib-l-1d       glVertexAttribL1d)
(define gl-vertex-attrib-l-2d       glVertexAttribL2d)
(define gl-vertex-attrib-l-3d       glVertexAttribL3d)
(define gl-vertex-attrib-l-4d       glVertexAttribL4d)
(define gl-viewport                 glViewport)

;; ---------- 常量：全大写下划线 → kebab-case ----------

(define gl-array-buffer            GL_ARRAY_BUFFER)
(define gl-color-buffer-bit        GL_COLOR_BUFFER_BIT)
(define gl-compile-status          GL_COMPILE_STATUS)
(define gl-double                  GL_DOUBLE)
(define gl-dynamic-draw            GL_DYNAMIC_DRAW)
(define gl-float                   GL_FLOAT)
(define gl-fragment-shader         GL_FRAGMENT_SHADER)
(define gl-geometry-shader         GL_GEOMETRY_SHADER)
(define gl-info-log-length         GL_INFO_LOG_LENGTH)
(define gl-link-status             GL_LINK_STATUS)
(define gl-static-draw             GL_STATIC_DRAW)
(define gl-stream-draw             GL_STREAM_DRAW)
(define gl-tess-control-shader     GL_TESS_CONTROL_SHADER)
(define gl-tess-evaluation-shader  GL_TESS_EVALUATION_SHADER)
(define gl-triangles               GL_TRIANGLES)
(define gl-vertex-shader           GL_VERTEX_SHADER)

;; gl-vector-sizeof（见 provide 列表"辅助函数"段）是 opengl 包自带的 Racket 工具
;; 函数，本来就 kebab-case，不需要改名——所以这里没有对应 define，直接转出。
