;;;; rogue-like.lisp

(in-package #:rogue-like)

;;- What this file exports -----------------------------------------------------
(export '(basic-window))
 
;;- Basic Key Movement ---------------------------------------------------------

(defvar *square-x* 0 "X-Origin of the Square")
(defvar *square-y* 0 "Y-Origin of the Square")
(defvar *square-size* 100 "Width of the Square")
(defvar *step-size* 10 "Move by units")

(defun set-viewport (width height)
  "Create an orthographic project matrix."
  (let* ((bottom (/ height 2.0))
	 (right  (/ width 2.0))
	 (top    (- bottom))
	 (left   (- right)))
    (gl:viewport 0 0 width height)
    (gl:matrix-mode :projection)
    (gl:load-identity)
    (gl:ortho left right bottom top -1 1)
    (gl:matrix-mode :modelview)
    (gl:load-identity)))

; Callback to resize viewport when window is resized.
(def-window-size-callback update-viewport (window w h)
  (declare (ignore window))
  (set-viewport w h))

; Callback to close window when 'ESC' is pressed
(def-key-callback on-key-press (window key scancode action mod-keys)
  (declare (ignore window scancode mod-keys))
  (when (eq action :press)
    (case key
      ((:escape) (set-window-should-close))
      ((:w :up) (setq *square-y* (- *square-y* *step-size*)))
      ((:s :down) (setq *square-y* (+ *square-y* *step-size*)))
      ((:a :left) (setq *square-x* (- *square-x* *step-size*)))
      ((:d :right) (setq *square-x* (+ *square-x* *step-size*))))))

(defun render ()
  "Render a white rectangle on black background."
  (gl:clear :color-buffer)
  (gl:with-pushed-matrix
    (gl:color 1 1 1)
    (gl:rect (- *square-x* (/ *square-size* 2.0))
	     (- *square-y* (/ *square-size* 2.0))
	     (+ *square-x* (/ *square-size* 2.0))
	     (+ *square-y* (/ *square-size* 2.0)))))

(defun basic-window (width height)
  "Create basic window and show it."
  (with-body-in-main-thread ()
    (with-init-window (:title "Basic Window"
		       :width width
		       :height height)
      (setf %gl:*gl-get-proc-address* #'get-proc-address)
      (set-key-callback 'on-key-press)
      (set-window-size-callback 'update-viewport)
      (gl:clear-color 0 0 0 0)
      (set-viewport width height)
      (loop until (window-should-close-p)
	    do (render)
	    do (swap-buffers)
	    do (poll-events)))))
