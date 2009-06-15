(in-package :sandbox)



(defvar *weapons* '((:laser "1d4" 10 20)
		    (:torpedo "2d10 + 5" 15 95)))

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
   (age :accessor age :initform 100 :initarg :age))
  (:default-initargs
      :size 5))

(defmethod is-alive-p ((tp torpedo))
 (and (plusp (age tp))
      (target tp)))

(defmethod death progn ((tp torpedo))
  (cl-heap:enqueue *initiative* 
		   (make-instance 'explosion
				  :team sdl:*white*
				  :num-particles (dice:roll "2d6")
				  :location (location tp))
		   -1))

(defmethod act ((tp torpedo))
  (decf (age tp))
  (cond
    ;;detonate
    ((rectangles:intersectp (rectangle tp)
			    (rectangle (target tp)))     
     (decf (health (target tp))
	   (weapon-damage :torpedo))
     (setf (target tp) nil))
    ;; close in on the target if we're going less than 5 or our current velocity
    ;; takes us further away
    ((is-alive-p (target tp))
	(setf (velocity tp)
	      (direction (location tp)
			 (location (target tp))
			 (min 10 (+ (age tp) 5)))))
    ;;acquire a new target
    (T (setf (target tp) (nearest-enemy-ship tp 100))))
  -1)

(defmethod draw ((actor torpedo))
  (sdl:draw-filled-circle (location actor) 2 :color sdl:*white*))

(defmethod attack-with-weapon ((attacker ship)
			       (defender ship)
			       (weapon (eql :torpedo)))
  (let ((tp (make-instance 'torpedo
			   :location (copy-seq (location attacker))
			   :team (team attacker)
			   :target defender
			   :velocity (direction (location attacker)
						(location defender)
						10))))
    (decf (energy attacker) (weapon-energy-cost weapon))
    (add-to-world tp)
    (cl-heap:enqueue *initiative* tp -1)))