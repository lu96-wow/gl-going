# 10-camera 小结

做了什么: 把 09 课写死的"往后退 6 格"换成真相机: 视图矩阵 V 把世界搬到相机面前; 再用球坐标(yaw/pitch/dist)做轨道相机, 鼠标拖拽环绕/滚轮拉近拉远.

API:
- mat4-look-at eye center up  由相机位置+看向目标+上方向生成视图矩阵 V(01 裸写, 04 收进 lib)
- grid-verts span step        XZ 平面网格地面(gl-lines), 让朝向一眼可见
- 球坐标 -> 眼睛位置           yaw/pitch/dist 三个数反推 eye.x/y/z
- on-event(e)                 鼠标回调: button-down?/button-up?/motion + get-x/get-y 拖拽环绕
- on-char(e)                  键盘回调: get-key-code = 'wheel-up/'wheel-down(滚轮!)/#\r/'escape
- box / unbox / set-box!      可变状态(相机参数), 回调改数字, draw 每帧读

注意: 相机不动世界动, CPU 每帧只改 V 上传; 滚轮是键盘事件(在 on-char 处理); pitch 夹 -85..85 防翻顶; (send canvas focus) 让键盘事件落到画布.
