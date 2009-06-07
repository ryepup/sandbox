(in-package :sandbox)

(defvar *sim-thread*)

(defun %start ()
  (setf *sim-thread*
	(sb-thread:make-thread
	 (lambda ()
	   (sdl :idlefn (lambda ()
			  (simulation-update)))))))


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

(defvar *effects* nil)

(defun simulation-update ()
  (sdl:clear-display sdl:*black*)
  (iter (for e in *effects*)
	(act e)
	(draw e))
  (iter (for a in (spatial-trees:search *world-rect* *world*))
	(act a)
	(draw a))
  (setf *effects*
	(remove-if (lambda (ef) (zerop (energy ef)))
		   *effects*))
  )

(defgeneric draw (actor)
  (:method ((actor T))
    (break "need a drawing function for ~a" actor)))

(defgeneric act (actor))
(defmethod act ((actor T))
  (break "need something to do for ~a" actor))


(defclass actor ()
  ((location :accessor location :initform (random-point)
   :initarg :location)
   (energy :accessor energy :initform 10 :initarg :energy)
   (size :accessor size :initform 1 :initarg :size)))

(defmethod distance ((a actor) (b actor))
  (sdl:distance (location a)
		(location b)))

(defmethod act :after ((obj actor))
  (unless (plusp (energy obj))
    (remove-from-world obj)))

(defmethod rectangle ((obj actor))
  (rectangles:make-rectangle
   :lows (iter (for c in-vector (location obj))
	       (collect (- c (size obj))))
   :highs (iter (for c in-vector (location obj))
	       (collect (+ c (size obj))))))

(defmethod draw ((actor actor))
  (sdl:draw-pixel (location actor) :color sdl:*green*))



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
(defmethod act ((x explosion))
  (when (eq (age x) (num-particles x))
    (setf (energy x) 0))
  (incf (age x)))

(defun fade-to-black (color percentage-black)
  (sdl:color :r (* (- 1 percentage-black) (sdl:r color))
	     :g (* (- 1 percentage-black) (sdl:g color))
	     :b (* (- 1 percentage-black) (sdl:b color))))



(defmethod draw ((exp explosion))
  (iter (for p in-vector (particles exp))
	(for vx = (aref p 0))
	(for vy = (aref p 1))
	(for life = (aref p 2))
	(for percent-black = (min (/ (age exp)
				     life)
				  1))
	(for x = (* (age exp) vx))
	(for y = (* (age exp) vy))
	(when (< percent-black 1)
	  (sdl:draw-pixel (map 'vector #'+
			       (location exp)
			       (vector x y))
			  :color (fade-to-black (team exp)
						percent-black)))))


;;;;;;;;;;;;;;
(defclass ship (actor)
  ((velocity :accessor velocity :initform
	     (vector 0 0))
   (range :accessor range :initform 50)
   (sensor-range :accessor sensor-range :initform 100)
   (health :accessor health :initform 100 :initarg :health)
   (team :accessor team :initarg :team :initform sdl:*green*))
  (:default-initargs
      :size 10))

(defmethod draw ((actor ship))
  (sdl:with-color (c (team actor))
;    (sdl:draw-circle (location actor) (range actor) :alpha 50)
;    (sdl:draw-circle (location actor) (sensor-range actor) :alpha 50 )
    (sdl:draw-filled-circle (location actor) (size actor))))

(defmethod act ((ship ship))
  (or (< (energy ship) 20)
      (attack ship)
      (patrol ship))
  (move ship)
  (setf (energy ship)
	(alexandria:clamp (1+ (energy ship)) 0 100)))

(defmethod next-location ((ship ship))
  (map 'vector #'+
       (velocity ship)
       (location ship)))

(defmethod attack ((ship ship))
  (alexandria:when-let ((enemy (first (enemy-ships ship (sensor-range ship)))))
		       (if (< (distance ship enemy) (range ship))
			   (progn
			     (sdl:draw-line (next-location ship)
					    (next-location enemy)
					    :color (team ship))
			     (decf (energy ship) 10)
			     (decf (health enemy) (random 6))
			     (unless (plusp (health enemy))
			       (push (make-instance 'explosion
						    :team (team enemy)
						    :num-particles (* 3 (size enemy))
						    :location (next-location enemy))
				     *effects*)
			       (remove-from-world enemy))))
		       (progn
			 ;;give chase
			 (iter (for idx in-vector #(0 1))
			       (when (< (aref (location enemy) idx)
					(aref (location ship) idx))
				 (decf (aref (velocity ship) idx))
				 (decf (energy ship)))
			       (when (> (aref (location enemy) idx)
					(aref (location ship) idx))
				 (incf (aref (velocity ship) idx))
				 (decf (energy ship)))))))

(defmethod find-ships ((ship ship) range)
  (iter (for s in (spatial-trees:search
		   (rectangles:make-rectangle
		    :lows (iter (for c in-vector (location ship))
				(collect (- c range)))
		    :highs (iter (for c in-vector (location ship))
				 (collect (+ c range))))
		   *world*))
	(when (< (distance s ship) range)
	  (collect s))))

(defmethod enemy-ships ((ship ship) range)
  (iter (for enemy in (find-ships ship range))
	(unless (eq (team enemy)
		    (team ship))	  
	  (collect enemy))))

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

(defmethod move ((actor ship))
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

(defmethod print-object ((o ship) s)
  (print-unreadable-object (o s :type t)    
    (format s "~a " (velocity o))
    ))

(defun setup-battle (red green)
  (when green
  (dotimes (n 120)
    (add-to-world (make-instance 'ship :team sdl:*green*
				 :size (+ 1 (random 4))
				 :health (+ 5 (random 10))
				 :location (vector (+ (- *width* (/ *width* 4))
						      (random (/ *width* 4)))
						   (random *height*))))))
  (when red
  (dotimes (n 50)    
    (add-to-world (make-instance 'ship :team sdl:*red*
				 :size (+ 5 (random 10))
				 :health (+ 20 (random 40))
				 :location (vector (random (/ *width* 4))
						   (random *height*))))))
  )