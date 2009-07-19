(in-package :sandbox)
(declaim (optimize (speed 0) (safety 3) (debug 3)))
(defclass ship (movable)
  ((weapons :accessor weapons :initform '(:laser)
	    :initarg :weapons)
   (range :accessor range :initform 50)
   (damage-dice :accessor damage-dice :initarg :damage-dice :initform "1d6")
   (sensor-range :accessor sensor-range :initform 100)
   (health :accessor health :initform 100 :initarg :health)
   (initiative :initform (dice:roll "1d20")
	       :reader initiative)
   (surface :accessor surface :initform nil)
   (team :accessor team :initarg :team :initform sdl:*green*))
  (:default-initargs
      :size 10))

(defmethod initialize-instance :after ((a ship) &key &allow-other-keys)
)

(defun ensure-surface (ship)
  (unless (surface ship)
    (let* ((size (size ship))
	   (double-size (* size 2))
	   (surf (sdl:create-surface double-size double-size :color-key (sdl:color) )))

      (sdl:draw-filled-circle
       (sdl:point :x size
		  :y size)
       size
       :surface surf
       :color (team ship))

	(setf (surface ship) surf))))

(defmethod draw ((actor ship))
  (ensure-surface actor)
  (sdl:set-point (surface actor) (location actor))
  (sdl:blit-surface (surface actor)))

(defmethod print-object ((o ship) s)
  (print-unreadable-object (o s :type t)    
    (format s "~a " (velocity o))))

(defmethod is-alive-p ((a ship))
  (plusp (health a)))


(defmethod act :around ((ship ship))
  (call-next-method)
  (add-to-world ship))

(defmethod act ((ship ship))
  (or (< (energy ship) 20)
      (attack ship)
      (patrol ship))
  (setf (energy ship)
	(alexandria:clamp (1+ (energy ship)) 0 100)))


(defmethod death progn ((ship ship))
  (enqueue (make-instance 'explosion
			  :team (team ship)
			  :num-particles (* 3 (size ship))
			  :location (location ship))))

(defmethod attack ((ship ship))
  (alexandria:if-let
   ((enemy (nearest-enemy-ship ship (sensor-range ship))))    
   (alexandria:if-let
    ((viable-weapons
      (iter (for w in (weapons ship))
	    (for w-range = (weapon-range w))
	    (for w-cost = (weapon-energy-cost w))
	    (when (and (distance-within ship enemy w-range)
		       (< w-cost (energy ship)))
	      (collect w)))))
    (attack-with-weapon ship enemy (alexandria:random-elt viable-weapons)) 
    ;;not in weapons range, give chase
    (iter (for idx in-vector #(0 1))
	  (when (< (aref (location enemy) idx)
		   (aref (location ship) idx))
	    (decf (aref (velocity ship) idx))
	    (decf (energy ship)))
	  (when (> (aref (location enemy) idx)
		   (aref (location ship) idx))
	    (incf (aref (velocity ship) idx))
	    (decf (energy ship)))))))

(defmethod find-ships ((ship actor) range)
  (iter (for s in (search-world 
		   (rectangles:make-rectangle
		    :lows (iter (for c in-vector (location ship))
				(collect (- c range)))
		    :highs (iter (for c in-vector (location ship))
				 (collect (+ c range))))))
	(when (distance-within s ship range)
	  (collect s))))

(defun nearest-enemy-ship (ship range) 
  (iter
   (with q = (make-instance 'cl-heap:priority-queue))
   (for enemy in (find-ships ship range))
   (unless (eq (team enemy)
	       (team ship))
     (cl-heap:enqueue q enemy (distance enemy ship)))
   (finally (return (cl-heap:peep-at-queue q)))))

(defmethod enemy-ships ((ship ship) range)
  (let ((q (make-instance 'cl-heap:priority-queue)))
    (iter (for enemy in (find-ships ship range))
	  (unless (eq (team enemy)
		      (team ship))
	    (cl-heap:enqueue q enemy (distance enemy ship))))
    (iter (for a = (cl-heap:dequeue q))
	  (while a)
	  (collect a))))

(defmethod patrol ((ship ship))
  ;;don't go too fast
  (if (not (speed-within ship 5))
      (iter (for idx in-vector #(0 1))
	    (for c in-vector (velocity ship))
	    (when (plusp c)
	      (decf (aref (velocity ship) idx)))
	    (when (minusp c)
	      (incf (aref (velocity ship) idx)))))
  ;;stay away from other ships
  (let ((n (next-location ship))
	(too-close (* 3 (size ship))))
    (iter (for nearby-ship in (find-ships ship (* 2 too-close)))
	  (for next-ship-location = (next-location nearby-ship))
	  (when (distance-within next-ship-location n too-close)
	    ;;too close, move away from next-ship-location
	    (iter (for idx in-vector #(0 1))
		  (when (< (aref next-ship-location idx)
			   (aref n idx))
		    (incf (aref (velocity ship) idx))
		    (decf (energy ship)))
		  (when (> (aref next-ship-location idx)
			   (aref n idx))
		    (decf (aref (velocity ship) idx))
		    (decf (energy ship)))))))
  ;;stay away from the edges
  (let ((sensor (sensor-range ship)))
    (iter (for idx in-vector #(0 1))
	  (for c in-vector (location ship))
	  (for limit in-vector (vector *width* *height*))
	  (cond 
	    ;;if we can see the left edge, go right
	    ((not (plusp (- c sensor)))
	     (incf (aref (velocity ship) idx))
	     (decf (energy ship) 1))
	    ;;if we can see the right edge, go left
	    ((not (plusp (- limit c sensor)))
	     (decf (aref (velocity ship) idx))
	     (decf (energy ship) 1))
	    ;;if we're stopped, start
	    ((zerop (aref (velocity ship) idx))
	     (setf (aref (velocity ship) idx)
		   (alexandria:random-elt '(1 -1))))))))
