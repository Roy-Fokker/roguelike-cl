;;;; rogue-like.lisp

(in-package #:rogue-like)

;;- What this file exports -----------------------------------------------------
(export '(main))
 
;;- Movement -------------------------------------------------------------------
(defvar *step-size* 10 "Move by units")

;;- Mesh variables -------------------------------------------------------------
(defvar *square-x* 0 "X-Origin of mesh")
(defvar *square-y* 0 "Y-Origin of mesh")
(defvar *square-size* 50 "Size of mesh")
(defvar *vao* nil "Vertex Array Object")
(defvar *vbo* nil "Vertex Buffer Object")

;;- Create and Bind Vertex Array and Buffer Objects ----------------------------
(defun create-gl-array (type lisp-array)
  (let ((gl-array (gl:alloc-gl-array type (length lisp-array))))
    (dotimes (i (length lisp-array))
      (setf (gl:glaref gl-array i) (aref lisp-array i)))
    gl-array))

(defun make-vertex-objects ()
  "Create a Square mesh"

  (setq *vao* (gl:gen-vertex-array))
  (gl:bind-vertex-array *vao*)

  (setq *vbo* (car (gl:gen-buffers 1)))
  (gl:bind-buffer :array-buffer *vbo*)
  
  (let ((vertex-array (create-gl-array :float (vector
					       -0.5 -0.5
					       -0.5  0.5 
					       0.5  0.5
					       0.5  0.5
					       0.5 -0.5
					       -0.5 -0.5
					       ;; (- *square-x* (/ *square-size* 2.0)) (- *square-y* (/ *square-size* 2.0))
					       ;; (- *square-x* (/ *square-size* 2.0)) (+ *square-y* (/ *square-size* 2.0))
					       ;; (+ *square-x* (/ *square-size* 2.0)) (+ *square-y* (/ *square-size* 2.0))
					       ;; (+ *square-x* (/ *square-size* 2.0)) (+ *square-y* (/ *square-size* 2.0))
					       ;; (+ *square-x* (/ *square-size* 2.0)) (- *square-y* (/ *square-size* 2.0))
					       ;; (- *square-x* (/ *square-size* 2.0)) (- *square-y* (/ *square-size* 2.0))
					       )
				       )))
    (gl:buffer-data :array-buffer :static-draw vertex-array))
  )

;;- Shader variables -----------------------------------------------------------
(defvar *vertex-shader* nil "GLSL Vertex Shader")
(defvar *fragment-shader* nil "GLSL Fragment Shader")
(defvar *shader-program* nil "GLSL Shader Program")

;;- GLSL Shader Source ---------------------------------------------------------
(defparameter +vs-source+ "
#version 330

in vec2 position;
void main(void)
{
    gl_Position = vec4(position, 0.0, 1.0);
}
")

(defparameter +fs-source+ "
#version 330

void main(void) 
{
    gl_FragColor = vec4(0.0, 0.0, 1.0, 1.0);   
}
")

;;- Compile and Link Shader Program --------------------------------------------
(defun make-shader-program ()
  "Create vertex and fragment shader and link it to shader program."
  (setf *vertex-shader* (gl:create-shader :vertex-shader))
  (gl:shader-source *vertex-shader* +vs-source+)
  (gl:compile-shader *vertex-shader*)
  (if (gl:get-shader *vertex-shader* :compile-status)
	*vertex-shader*
	(error "Vertex Shader compliation failed!"))

  (setf *fragment-shader* (gl:create-shader :fragment-shader))
  (gl:shader-source *fragment-shader* +fs-source+)
  (gl:compile-shader *fragment-shader*)
  (if (gl:get-shader *fragment-shader* :compile-status)
	*fragment-shader*
	(error "Fragment Shader compliation failed!"))
  
  (setf *shader-program* (gl:create-program))
  (gl:attach-shader *shader-program* *vertex-shader*)
  (gl:attach-shader *shader-program* *fragment-shader*)
  (gl:link-program *shader-program*)

  (let 
      ((pos-attr (gl:get-attrib-location *shader-program* "position")))
    (gl:enable-vertex-attrib-array pos-attr)
    (gl:vertex-attrib-pointer pos-attr
			      2
			      :float
			      :false
			      0 ;(* 4 (cffi:foreign-type-size :float))
			      (cffi:null-pointer))
    )
  )

;;- Create Viewport ------------------------------------------------------------
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

;;- GLFW callbacks -------------------------------------------------------------
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

;;- Render function ------------------------------------------------------------
(defun render ()
  "Render a white rectangle on black background."
  (gl:clear :color-buffer)

  (gl:bind-vertex-array *vao*)
  (gl:use-program *shader-program*)
  (gl:draw-arrays :triangles 0 6)
  
  ;; (gl:with-pushed-matrix
  ;;   ;; (gl:color 1 1 1)
  ;;   ;; (gl:rect (- *square-x* (/ *square-size* 2.0))
  ;;   ;; 	     (- *square-y* (/ *square-size* 2.0))
  ;;   ;; 	     (+ *square-x* (/ *square-size* 2.0))
  ;;   ;; 	     (+ *square-y* (/ *square-size* 2.0))))
    )

;;- Window with Loop -----------------------------------------------------------
(defun start-window-and-loop (width height)
  "Create basic window and show it."
  (with-body-in-main-thread ()
    (with-init-window (:title "Basic Window"
		       :width width
		       :height height)
      (setf %gl:*gl-get-proc-address* #'get-proc-address)
      (set-key-callback 'on-key-press)
      (set-window-size-callback 'update-viewport)

      (make-shader-program)
      (make-vertex-objects)

      ;; (gl:enable :cull-face)
      ;; (gl:cull-face :front)
      ;; (gl:front-face :cw)
      
      (gl:clear-color 0 0 0 0)
      (set-viewport width height)
      (loop until (window-should-close-p)
	    do (render)
	    do (swap-buffers)
	    do (poll-events)))))


(defun main ()
  "Get the whole thing started."
  (start-window-and-loop 800 600))
