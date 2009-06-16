(in-package :sandbox)

(defvar *sim-thread*)

(defun %start ()
  (setf *sim-thread*
	(sb-thread:make-thread
	 (lambda ()
	   (sdl :idlefn (lambda ()
			  (simulation-update))
		:frame-rate 25)))))

(defgeneric rectangle (actor)
  (:method ((actor T))
    (break "need a rectangle function for ~a" actor)))

(defvar *world* (spatial-trees:make-spatial-tree
		 :r :rectfun #'rectangle))
(defvar *next-world* nil)

(defun remove-from-world (object)
  (spatial-trees:delete object *world*))

(defun add-to-world (object)
  (spatial-trees:insert object (or *next-world* *world* )))

(defun search-world (rectangle)
  (spatial-trees:search rectangle *world*))

(defvar *initiative* (make-instance 'cl-heap:priority-queue))

(defmethod enqueue (obj &optional (priority 1))  
  (cl-heap:enqueue *initiative* obj priority))




(defun fade-to-black (color percentage-black)
  (let ((percentage-black (min 1 percentage-black)))
    (sdl:color :r (* (- 1 percentage-black) (sdl:r color))
	       :g (* (- 1 percentage-black) (sdl:g color))
	       :b (* (- 1 percentage-black) (sdl:b color)))))

(defun direction (p1 p2 max-length)
  (let* ((v (map 'vector #'- p2 p1))
	 (l (sdl:distance v #(0 0)))
	 (scale (/ max-length l)))
    (map 'vector (lambda (x) (floor (* scale x))) v)))


(defmethod distance-within ((p1 vector) (p2 vector) range)
  (declare (optimize (speed 3) (safety 0))
	   (type (simple-array integer) p1 p2)
	   (type integer range))
  (and (every (lambda (x1 x2)
		(> range (abs (- x2 x1))))
	      p1 p2) 
       (< (reduce #'+ (map 'vector
			   (lambda (x1 x2)
			     (* (- x1 x2) (- x1 x2)))
			   p1 p2))
	  (* range range))))


(defun reset-sim ()
  (cl-heap:empty-queue *initiative*)
  (setf *world* (spatial-trees:make-spatial-tree
		 :r :rectfun #'rectangle)))

(defun simulation-update ()
  (sdl:clear-display sdl:*black*)
  (iter (for a = (cl-heap:dequeue *initiative*))
	(with next-initiative = (make-instance 'cl-heap:priority-queue))
	(with next-world = (spatial-trees:make-spatial-tree
			    :r :rectfun #'rectangle))
	(while a)
	(let ((*initiative* next-initiative)
	      (*next-world* next-world))
	  (if (is-alive-p a)
	      (progn
		(act a)
		(draw a)
		(enqueue a (initiative a)))
	      (death a)))
	(finally (setf *initiative* next-initiative
		       *world* next-world))))


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
	(enqueue s)
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
	(enqueue s)
	(add-to-world s)))))