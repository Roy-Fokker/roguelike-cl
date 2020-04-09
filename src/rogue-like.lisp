;;;; rogue-like.lisp

(in-package #:rogue-like)

(export 'main)

;; -----------------------------------------------------------------------------
;; Package Entry point
(defun main ()
  (rl/glfw-window)
  )

  )

  )

;; -----------------------------------------------------------------------------
;; Create GLFW window
(defvar *is-already-running* nil)  ;; If window already exists

(defun rl/glfw-window (&key
			 (width 800)
			 (height 600))
  (when *is-already-running*
    (error "Running two instances has led to window system crashes."))

  (setf *is-already-running* t)

  ;; So we can continue to interact with REPL
  (trivial-main-thread:with-body-in-main-thread ()
    (rl/glfw-empty-window :width width :height height))

  (setf *is-already-running* nil)
  )

;; -----------------------------------------------------------------------------
;; GLFW callbacks

;; On Window Resize
(glfw:def-window-size-callback rl/update-viewport (window w h)
  (declare (ignore window))
  
  (gl:viewport 0 0 w h)
  )

;; On Key Pressed
(glfw:def-key-callback rl/key-pressed (window key scancode action mod-keys)
  (declare (ignore window
		   scancode
		   mod-keys))

  (when (eq action :press)
    (case key
      ((:escape) (glfw:set-window-should-close))
      ))
  )

;; -----------------------------------------------------------------------------
;; Empty Window
;; Based on example by https://bitbucket.org/pdonnelly/modern-opengl-notes.git
;; Mixed with cl-glfw3's example code.
(defun rl/glfw-empty-window (&key width height)
  "Create an empty glfw window"
  (glfw:with-init-window (:title "Empty GLFW window"
			  :width width
			  :height height)
    ;; Set the callbacks.
    (glfw:set-window-size-callback 'rl/update-viewport)
    (glfw:set-key-callback 'rl/key-pressed)
    
    (glfw:swap-interval 1) ;; Request VSync

    (loop until (window-should-close-p)
	  do (glfw:poll-events)
	  do (glfw:swap-buffers)
	  )
    )
  )
