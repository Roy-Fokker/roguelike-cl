;;;; rogue-like.asd

(asdf:defsystem #:rogue-like
  :description "Learning Common Lisp via roguelike"
  :author "Neel Raiyani [Roy-Fokker]"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-opengl
	       #:cl-glfw3
	       #:trivial-main-thread)
  :pathname "src/"
  :components ((:file "package")
               (:file "rogue-like")))
