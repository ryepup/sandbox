(in-package :sandbox)

(defvar *sim-thread*)

(defun %start ()
  (setf *sim-thread*
	(sb-thread:make-thread
	 (lambda ()
	   (sdl :idlefn (lambda ()
			  (simulation-update))
		:frame-rate 60)))))


(defgeneric rectangle (actor)
  (:method ((actor T))
    (break "need a rectangle function for ~a" actor)))

(defvar *world* (spatial-trees:make-spatial-tree
		 :r :rectfun #'rectangle))
(defvar *world-rect* (rectangles:make-rectangle :lows '(0 0) :highs (list *width* *height*)))

(defun remove-from-world (object)
  (spatial-trees:delete object *world*))

(defun add-to-world (object)
  (spatial-trees:insert object *world*))

(defvar *initiative* (make-instance 'cl-heap:priority-queue))

(defun reset-sim ()
  (cl-heap:empty-queue *initiative*)
  (setf *world* (spatial-trees:make-spatial-tree
		 :r :rectfun #'rectangle)))

(defun simulation-update ()
  (sdl:clear-display sdl:*black*)
  (iter (for a = (cl-heap:dequeue *initiative*))
	(with next-initiative = (make-instance 'cl-heap:priority-queue))
	(while a)
	(if (is-alive-p a)
	    (let ((new-init (act a))) 
	      (draw a)
	      (if (null new-init)
		  (remove-from-world a)
		  (cl-heap:enqueue next-initiative a new-init)))
	    (death a))
	(finally (setf *initiative* next-initiative))))

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

(defclass movable (actor)
  ((velocity :accessor velocity
	     :initform (vector 0 0)
	     :initarg :velocity)))

(defmethod act :after ((actor movable))
  (unless (every #'zerop (velocity actor))
    (remove-from-world actor)
    (setf (aref (location actor) 0)
	  (alexandria:clamp (+ (aref (location actor) 0)
			       (aref (velocity actor) 0))
			    0 *width*)
	  (aref (location actor) 1)
	  (alexandria:clamp (+ (aref (location actor) 1)
			       (aref (velocity actor) 1))
			    0 *height*))
    (add-to-world actor)))

(defmethod next-location ((ship movable))
  (map 'vector #'+
       (velocity ship)
       (location ship)))

;;;;;;;;;;;;;;
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

(defun fade-to-black (color percentage-black)
  (let ((percentage-black (min 1 percentage-black)))
    (sdl:color :r (* (- 1 percentage-black) (sdl:r color))
	       :g (* (- 1 percentage-black) (sdl:g color))
	       :b (* (- 1 percentage-black) (sdl:b color)))))

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
;;;;;;;;;;;;;;
(defclass ship (movable)
  ((weapons :accessor weapons :initform '(:laser)
	    :initarg :weapons)
   (range :accessor range :initform 50)
   (damage-dice :accessor damage-dice :initarg :damage-dice :initform "1d6")
   (sensor-range :accessor sensor-range :initform 100)
   (health :accessor health :initform 100 :initarg :health)
   (initiative :accessor initiative :initform (dice:roll "1d20"))
   (team :accessor team :initarg :team :initform sdl:*green*))
  (:default-initargs
      :size 10))

(defmethod is-alive-p ((a ship))
  (plusp (health a)))

(defmethod draw ((actor ship))
  (sdl:with-color (c (team actor))
    (sdl:draw-filled-circle (location actor) (size actor))))

(defmethod act ((ship ship))
  (or (< (energy ship) 20)
      (attack ship)
      (patrol ship))
  (setf (energy ship)
	(alexandria:clamp (1+ (energy ship)) 0 100))
  (initiative ship))


(defmethod death progn ((ship ship))
  (cl-heap:enqueue *initiative* 
		   (make-instance 'explosion
				  :team (team ship)
				  :num-particles (* 3 (size ship))
				  :location (location ship))
		   -1))

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


(defmethod attack ((ship ship))
  (alexandria:if-let
   ((enemy (nearest-enemy-ship ship (sensor-range ship))))    
   (alexandria:if-let
    ((viable-weapons (iter (with range = (distance ship enemy))
			   (for w in (weapons ship))
			   (for w-range = (weapon-range w))
			   (for w-cost = (weapon-energy-cost w))
			   (when (and (< w-range range)
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
  (iter (for s in (spatial-trees:search
		   (rectangles:make-rectangle
		    :lows (iter (for c in-vector (location ship))
				(collect (- c range)))
		    :highs (iter (for c in-vector (location ship))
				 (collect (+ c range))))
		   *world*))
	(when (and (typep s 'ship) (< (distance s ship) range))
	  (collect s))))

(defun nearest-enemy-ship (ship range)
  (let ((q (make-instance 'cl-heap:priority-queue)))
    (iter (for enemy in (find-ships ship range))
	  (unless (eq (team enemy)
		      (team ship))
	    (cl-heap:enqueue q enemy (distance enemy ship))))
    (prog1 
	(cl-heap:dequeue q)
      (cl-heap:empty-queue q))))


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
  (if (> (sdl:distance (velocity ship) #(0 0)) 5)
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
	  (when (< (sdl:distance next-ship-location n) too-close)
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
	     (setf (aref (velocity ship) idx) (alexandria:random-elt '(1 -1))))))))

(defmethod print-object ((o ship) s)
  (print-unreadable-object (o s :type t)    
    (format s "~a " (velocity o))))

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

(defun direction (p1 p2 max-length)
  (let* ((v (map 'vector #'- p2 p1))
	 (l (sdl:distance v #(0 0)))
	 (scale (/ max-length l)))
    (map 'vector (lambda (x) (floor (* scale x))) v)))

(defun setup-battle (red green)
  (when green
    (dotimes (n 120)
      (let ((s (make-instance 'ship :team sdl:*green*
			      :energy (dice:roll "1d4")
			      :size (dice:roll "1d4")
			      :health (dice:roll "1d10 + 5")
			      :location (vector (+ (- *width* (/ *width* 4))
						   (random (/ *width* 4)))
						(random *height*)))))
	(cl-heap:enqueue *initiative* s (initiative s))
	(add-to-world s))))
  (when red
    (dotimes (n 60)
      (let ((s (make-instance 'ship :team sdl:*red*
			      :size (dice:roll "1d10 + 10")
			      :energy (dice:roll "1d6")
			      :weapons '(:laser :torpedo)
			      :health (dice:roll "4d10 + 20")
			      :location (vector (random (/ *width* 4))
						(random *height*)))))
	(cl-heap:enqueue *initiative* s (initiative s))
	(add-to-world s)))))