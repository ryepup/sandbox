;;;; load our libs
(require 'asdf)
(asdf:oos 'asdf:load-op :lispbuilder-sdl)
(asdf:oos 'asdf:load-op :pcall)

;;;; make a namespace
(defpackage #:brain
  (:use #:cl))

(in-package #:brain)

(defvar *dim-board* '(90 90) "how many squares on the board")

(defun make-board ()
  (make-array *dim-board*))

(defun set-cell (x y state &key (board *board*))
  (setf (aref board x y) state))
(defun get-cell (x y &key (board *board*))
  (declare )
  (aref board x y))

(defmacro do-board ((x-var y-var cell-var) &body body)
  "loop over the current board"
  (let ((x (gensym "x"))
	(y (gensym "y")))
  `(dotimes (,x (first *dim-board*))
     (dotimes (,y (second *dim-board*))
       (let ((,x-var ,x)
	     (,y-var ,y)
	     (,cell-var (get-cell ,x ,y)))
	 ,@body)))))

(defvar *dim-screen* '(540 540) "size of the rendering window, pixels")
(defvar *dim-scale*  
  (map 'vector #'/ *dim-screen* *dim-board*)
  "pixels per grid square")

(defvar *board* nil "the state of the board")
(defvar *stage* nil "staging memory for the next board")

(defun render-cell (x y state)
  (let* ((x-scale (aref *dim-scale* 0))
	 (y-scale (aref *dim-scale* 1))
	 (x (1+ (* x x-scale)))
	 (y (1+ (* y y-scale)))) 
    (sdl:draw-box-* x y
		    (1- x-scale)
		    (1- y-scale)
		    :color (if (eq state :dying)
			       sdl:*red*
			       sdl:*white*))))

(defun render ()
  (sdl:clear-display sdl:*black*)
  (do-board (x y state)
    (unless (eq state :off)
      (render-cell x y state)))
  (sdl:update-display))

(defun torus-window (x y)
  "returns the neighbors of this cell, wrapping around the
screen if needed"
  (loop for x-offset in '(-1 0 1)
	nconc
     (loop for y-offset in (if (zerop x-offset)
			       '(-1 1)
			       '(-1 0 1))		 
	   collect
	;;uses modulo to wrap around the window
	(let* ((x (mod (+ x x-offset)
		       (first *dim-board*)))
	       (y (mod (+ y y-offset)
		       (second *dim-board*))))
	  (get-cell x y)))))

(defun next-state (x y &optional (state (get-cell x y)))
  (cond
    ((eq :on state) :dying)
    ((eq :dying state) :off)
    ((eq 2 (count :on (torus-window x y))) :on)
    (T :off)))

(defun next-board (new-board)
  (do-board (x y cell)
    (set-cell x y (next-state x y cell) :board new-board))
  T)

(defun do-it ()  
  (sdl:with-init ()
    (apply #'sdl:window *dim-screen*)
    (setf *board* (make-board) ;need to setf so threads can see these too
	  *stage* (make-board)) 
    (do-board (x y cell)
      (declare (ignore cell))
      (set-cell x y (if (< 50 (random 100))
			:on :off)))  
    (sdl:with-events ()
      (:quit-event () t)
      (:video-expose-event () (sdl:update-display))
      (:idle ()	       
	     (pcall:plet ((next-board (next-board *stage*)))
	       (render)
	       (unless next-board (error "whoa"))
	       (let ((x *board*))
		 (setf *board* *stage*
		       *stage* x)))))))
 