#lang racket/base

;; ============================================================
;; tool.rkt —— GLSL 文本 → 程序 的工具层
;;
;; 隐藏 OpenGL 的 ffi 细节（glShaderSource 的 s32vector/vector、
;; 编译链接状态检查、info log 拼接），把"编译 → 链接 → 组织成程序"
;; 这一整套收成几个函数，覆盖一个程序的完整生命周期：
;;   编译 compile-shader → 链接 link-program → 组织 build-program
;;   → 启用 use-program → 查 uniform uniform-location → 删除 delete-*
;;
;; 约定：
;;   - 编译 / 链接失败自动抛 Racket 异常，消息里带 GLSL / 链接报错日志。
;;   - 每段管线可自定义：任何 GL 着色器阶段类型都可作为编译 type。
;;   - 本模块不重新导出 opengl（GL_VERTEX_SHADER 等常量由调用方自己 require）。
;; ============================================================

(require opengl ffi/vector)

(provide compile-shader link-program build-program build-program/list
         use-program uniform-location delete-shader delete-program)

;; ---------- 内部：报错日志 ----------

(define (shader-info-log shader)
  (define len (glGetShaderiv shader GL_INFO_LOG_LENGTH))
  (define-values (actual log) (glGetShaderInfoLog shader len))
  (bytes->string/utf-8 log #\? 0 actual))

(define (program-info-log prog)
  (define len (glGetProgramiv prog GL_INFO_LOG_LENGTH))
  (define-values (actual log) (glGetProgramInfoLog prog len))
  (bytes->string/utf-8 log #\? 0 actual))

;; ---------- ① 编译：一段 GLSL 文本 → 一个着色器对象 ----------

;; type 是任意着色器阶段：GL_VERTEX_SHADER / GL_TESS_CONTROL_SHADER /
;; GL_TESS_EVALUATION_SHADER / GL_GEOMETRY_SHADER / GL_FRAGMENT_SHADER。
;; 编译失败自动抛错，错误消息带 GLSL 报错日志。
(define (compile-shader type src)
  (define shader (glCreateShader type))
  ;; glShaderSource 的 C 签名要"字符串数组 + 每段长度数组"：
  ;; 长度数组必须是 s32vector（32 位有符号整数向量）。纯 ffi 细节，藏在这里。
  (glShaderSource shader 1 (vector src) (s32vector (string-length src)))
  (glCompileShader shader)
  (when (zero? (glGetShaderiv shader GL_COMPILE_STATUS))
    (error 'compile-shader "着色器编译失败：\n~a" (shader-info-log shader)))
  shader)

;; ---------- ② 链接：若干着色器对象 → 一个程序对象 ----------

;; 链接失败自动抛错，带链接日志。
(define (link-program . shaders)
  (define prog (glCreateProgram))
  (for ([s shaders])
    (glAttachShader prog s))
  (glLinkProgram prog)
  (when (zero? (glGetProgramiv prog GL_LINK_STATUS))
    (error 'link-program "程序链接失败：\n~a" (program-info-log prog)))
  prog)

;; ---------- ③ 组织：把 (阶段类型 源码) 编译 + 链接成一个程序 ----------

;; 宏（每段写成 (类型 源码)，不提前求值）——每段管线可自定义、任意组合：
;;   (build-program (GL_VERTEX_SHADER vs) (GL_FRAGMENT_SHADER fs))
;;   (build-program (GL_VERTEX_SHADER vs) (GL_GEOMETRY_SHADER gs) (GL_FRAGMENT_SHADER fs))
(define-syntax-rule (build-program (type src) ...)
  (build-program/list (list (list type src) ...)))

;; 函数版（给需要动态构造阶段列表的代码用）：
;;   (build-program/list (list (list GL_VERTEX_SHADER vs) (list GL_FRAGMENT_SHADER fs)))
(define (build-program/list stages)
  (apply link-program
         (for/list ([s stages])
           (compile-shader (car s) (cadr s)))))

;; ---------- ④ 启用：把程序设为"当前要用的程序" ----------

;; OpenGL 是状态机，同一时刻只有一个"当前程序"，之后所有绘制都用它。
(define (use-program prog)
  (glUseProgram prog))

;; ---------- ⑤ 查 uniform：按名字拿一个 uniform 的位置 ----------

;; uniform = CPU 每帧传给 shader 的全局常量（教程后面会讲）。
;; 找不到时返回 -1（OpenGL 约定），调用方自行判断。
(define (uniform-location prog name)
  (glGetUniformLocation prog name))

;; ---------- ⑥ 删除：释放 GPU 资源 ----------

;; OpenGL 的删除是"标记待删"：真正释放等它不再被使用。删完别再引用。
(define (delete-shader shader)
  (glDeleteShader shader))

(define (delete-program prog)
  (glDeleteProgram prog))
