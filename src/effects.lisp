(in-package :sandbox)

(defclass explosion (actor)
  ((team :accessor team :initarg :team :initform sdl:*green*)
   (num-particles :accessor num-particles :initarg :num-particles)
   (particles :accessor particles)
   (age :accessor age :initform 1)))

(defmethod initialize-instance :after ((x explosion) &key &allow-other-keys)
  (setf (particles x)
	(make-array (num-particles x)
		    :initial-contents (iter (for i from 1 to (num-particles x))
					    (collect (vector (- 5 (random 10))
							     (- 5 (random 10))
							     (1+ (random (num-particles x)))))))))

(defmethod is-alive-p ((x explosion))
  (< (age x) (num-particles x)))

(defmethod act ((x explosion))
  (incf (age x))
  -1) ;;run these at high priority so they get processed/drawn before everything else

(defmethod draw ((exp explosion))
  (iter (for p in-vector (particles exp))
	(for vx = (aref p 0))
	(for vy = (aref p 1))
	(for life = (aref p 2))
	(for percent-black = (/ (age exp)
				life))
	(for x = (* (age exp) vx))
	(for y = (* (age exp) vy))
	(when (< percent-black 1)
	  (sdl:draw-pixel (map 'vector #'+
			       (location exp)
			       (vector x y))
			  :color (fade-to-black (team exp)
						percent-black)))))