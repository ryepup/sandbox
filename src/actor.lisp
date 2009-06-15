(in-package :sandbox)

(defclass actor ()
  ((location :accessor location :initform (random-point)
	     :initarg :location)
   (energy :accessor energy :initform 10 :initarg :energy)
   (size :accessor size :initform 1 :initarg :size)))

(defgeneric draw (actor)
  (:method ((actor T))
    (break "need a drawing function for ~a" actor)))

(defgeneric act (actor)
  (:method  ((actor T))
    (break "need something to do for ~a" actor)))

(defgeneric is-alive-p (actor)
  (:method ((a null)) nil)
  (:method ((a actor))
    (plusp (energy a))))

(defgeneric death (actor)
  (:method-combination progn)
  (:method progn ((a actor))
    (remove-from-world a)))

(defmethod distance ((a actor) (b actor))
  (sdl:distance (location a)
		(location b)))

(defmethod rectangle ((obj actor))
  (rectangles:make-rectangle
   :lows (iter (for c in-vector (location obj))
	       (collect (- c (size obj))))
   :highs (iter (for c in-vector (location obj))
		(collect (+ c (size obj))))))

(defmethod draw ((actor actor))
  (sdl:draw-pixel (location actor) :color sdl:*green*))