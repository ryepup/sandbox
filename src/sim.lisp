(in-package :sandbox)

(defvar *sim-thread*)

(defun %start ()
  (setf *sim-thread*
	(sb-thread:make-thread
	 (lambda ()
	   (sdl :idlefn (lambda ()
			  (simulation-update)))))))

(defvar *actors* nil)

(defun simulation-update ()
  (when (> 10 (length *actors*))
    (push (make-instance 'mover) *actors*)
    )
  (fader)
  (iter (for a in *actors*)
	(act a)
	(draw a)))







(defgeneric draw (actor)
  (:method ((actor T))
    (break "need a drawing function for ~a" actor)))

(defgeneric act (actor))
(defmethod act ((actor T))
  (break "need something to do for ~a" actor))

(defclass mover ()
  ((location :accessor location :initform (random-point)
	     :initarg :location)
   (speed :accessor current-speed :initform 1)))

(defmethod act ((actor mover))
  (setf (location actor)
	(random-close-point (location actor)
			    (current-speed actor)))
  (case (alexandria:random-elt '(:multiply :speed :slow nil nil nil nil nil))
    (:multiply (push (make-instance 'mover :location (location actor))
		     *actors*))
    (:speed (incf (current-speed actor)))
    (:slow (setf (current-speed actor)
		 (max (1- (current-speed actor))
		      0)))
    )

  )
(defmethod draw ((actor mover))
  (sdl:draw-pixel (location actor) :color sdl:*green*))