#lang racket/base
;; ============================================================
;; silent-bug.rkt —— 为什么需要 material？看裸 GL 的“静默忽略”
;; 运行：racket material-demo/silent-bug.rkt   （不需要 GPU）
;;
;; 用 mock-gl 复现真 GL 行为：glGetUniformLocation 给的是“program 内的整数”，
;; 把 A program 的 loc 用在 B program 上 → 什么都不设、也不报错。
;; ============================================================

(require racket/string "mock-gl.rkt")

(define g (make-gl))
(define scene  (glCreateProgram g 'scene  '("uMVP")))
(define screen (glCreateProgram g 'screen '("uScreen" "uMode")))

;; 两个 program 各自的 loc —— 靠人肉命名约定配对
(define loc-mvp  (glGetUniformLocation g scene  "uMVP"))
(define loc-tex  (glGetUniformLocation g screen "uScreen"))

;; 正确：scene 配它自己的 loc
(glUseProgram g scene)
(glUniform g loc-mvp '(mat4 P*V*M))

;; 写错：想在 scene 上传纹理，却拿了 screen 的 loc
(glUseProgram g scene)
(glUniform g loc-tex 0)

(displayln "—— 调用轨迹 ——")
(for-each displayln (gl-trace g))
(displayln "\n—— 最终生效的值 ——")
(gl-dump g (list scene screen))
(displayln "\n全程没有任何报错：uMVP 没设上、loc=1 被丢掉。")
(displayln "真 GL 里这就是“黑屏 / 参数不生效”却查不到原因的经典来源。")
