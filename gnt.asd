;;;; gnt.asdf

(asdf:defsystem #:gnt
  :license "MIT"
  :depends-on (#:sdl2
	       #:cl-opengl)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "aux")
   (:file "display")
   (:file "gnt")))
