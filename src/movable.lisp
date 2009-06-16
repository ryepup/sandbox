(in-package :sandbox)

(defclass movable (actor)
  ((velocity :accessor velocity
	     :initform (vector 0 0)
	     :initarg :velocity)))

(defmethod speed-within ((m movable) speed)
  (distance-within (velocity m) #(0 0) speed))

(defmethod act :after ((actor movable))
  (unless (every #'zerop (velocity actor))
    (setf (aref (location actor) 0)
	  (alexandria:clamp (+ (aref (location actor) 0)
			       (aref (velocity actor) 0))
			    0 *width*)
	  (aref (location actor) 1)
	  (alexandria:clamp (+ (aref (location actor) 1)
			       (aref (velocity actor) 1))
			    0 *height*))))

(defmethod next-location ((ship movable))
  (map 'vector #'+
       (velocity ship)
       (location ship)))