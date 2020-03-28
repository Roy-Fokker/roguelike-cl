;;;; rogue-like.lisp

(in-package #:rogue-like)

;;- What this file exports -----------------------------------------------------
(export '(hello-world
	  basic-window))

;;- Test Package Setup ---------------------------------------------------------
(defun hello-world ()
  "Print 'Hello World'"
  (print "Hello World")
  nil)

;;- Basic Window Example -------------------------------------------------------
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
(def-key-callback quit-on-escape (window key scancode action mod-keys)
  (declare (ignore window scancode mod-keys))
  (when (and (eq key :escape) (eq action :press))
    (set-window-should-close)))

(defun render ()
  "Render a white rectangle on black background."
  (gl:clear :color-buffer)
  (gl:with-pushed-matrix
    (gl:color 1 1 1)
    (gl:rect -25 -25 25 25)))

(defun basic-window (width height)
  "Create basic window and show it."
  (with-body-in-main-thread ()
    (with-init-window (:title "Basic Window"
		       :width width
		       :height height)
      (setf %gl:*gl-get-proc-address* #'get-proc-address)
      (set-key-callback 'quit-on-escape)
      (set-window-size-callback 'update-viewport)
      (gl:clear-color 0 0 0 0)
      (set-viewport width height)
      (loop until (window-should-close-p)
	    do (render)
	    do (swap-buffers)
	    do (poll-events)))))
