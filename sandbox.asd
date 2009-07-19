(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :sandbox.system)
    (defpackage :sandbox.system
      (:use :common-lisp :asdf))))

(in-package :sandbox.system)

(defsystem :sandbox
  :description "playground"
  :author "Ryan Davis <ryan@mokeys.org>"
  :licence "LGPL (or talk to me)"
  :version "0.1"
  :depends-on (#:iterate #:alexandria #:lispbuilder-sdl #:cl-geometry
			 #:spatial-trees #:cl-heap #:dice)
  :components ((:module
		:src
		:components
		((:file "packages")
		 (:file "sdl" :depends-on ("packages"))
		 (:file "sim" :depends-on ("sdl"))
		 (:file "actor" :depends-on ("sim"))
		 (:file "movable" :depends-on ("actor"))
		 (:file "effects" :depends-on ("actor"))
		 (:file "ship" :depends-on ("movable" "effects"))
		 (:file "weapons" :depends-on ("ship"))
		 ))))