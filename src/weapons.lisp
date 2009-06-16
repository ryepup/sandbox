(in-package :sandbox)

(defvar *weapons* '((:laser "2d6" 5 40)
		    (:torpedo "2d10 + 5" 50 40)))

(defun weapon-damage (weapon)
  (dice:roll (second (assoc weapon *weapons*))))

(defun weapon-energy-cost (weapon)
  (third (assoc weapon *weapons*)))

(defun weapon-range (weapon)
  (fourth (assoc weapon *weapons*)))

(defmethod attack-with-weapon ((attacker ship)
			       (defender ship)
			       (weapon (eql :laser)))
  (sdl:draw-line (next-location attacker)
		 (next-location defender)
		 :color (team attacker))
  (decf (energy attacker) (weapon-energy-cost weapon))
  (decf (health defender) (weapon-damage weapon)))


(defclass torpedo (movable)
  ((team :accessor team :initarg :team :initform sdl:*green*)
   (target :accessor target :initarg :target)
   (age :accessor age :initform 50 :initarg :age))
  (:default-initargs
      :size 5))

(defmethod is-alive-p ((tp torpedo))
 (plusp (age tp)))

(defmethod initiative ((tp torpedo))
  -1)

(defmethod death progn ((tp torpedo))
  (enqueue (make-instance 'explosion
			  :team sdl:*white*
			  :num-particles (dice:roll "2d6")
			  :location (location tp))))

(defmethod act ((tp torpedo))
  (decf (age tp))
  (cond
    ;;detonate
    ((and (target tp)
	  (rectangles:intersectp (rectangle tp)
				 (rectangle (target tp))))     
     (decf (health (target tp))
	   (weapon-damage :torpedo))
     (setf (target tp) nil))
    ;; close in on the target if we're going less than 5 or our current velocity
    ;; takes us further away
    ((is-alive-p (target tp))
	(setf (velocity tp)
	      (direction (location tp)
			 (location (target tp))
			 (min 8 (+ (age tp) 5)))))
    ;;acquire a new target
    (T (setf (target tp) (nearest-enemy-ship tp 100)))))

(defmethod draw ((actor torpedo))
  (sdl:draw-filled-circle (location actor) 2 :color sdl:*white*))

(defmethod attack-with-weapon ((attacker ship)
			       (defender ship)
			       (weapon (eql :torpedo))) 
  (decf (energy attacker) (weapon-energy-cost weapon))
  (enqueue
   (make-instance 'torpedo
		  :location (copy-seq (location attacker))
		  :team (team attacker)
		  :target defender
		  :velocity (direction (location attacker)
				       (location defender)
				       10))))