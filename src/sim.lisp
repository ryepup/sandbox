(in-package :sandbox)

(defvar *sim-thread*)

(defun %start ()
  (setf *sim-thread*
	(sb-thread:make-thread
	 (lambda ()
	   (sdl :idlefn (lambda ()
			  (simulation-update))
		:frame-rate 15)))))

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