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
  :depends-on (#:iterate #:alexandria #:lispbuilder-sdl)
  :components ((:module
		:src
		:components ((:file "packages")
			     (:file "sdl" :depends-on ("packages"))))))