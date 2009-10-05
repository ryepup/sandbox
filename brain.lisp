;;;; load our libs
(require 'asdf)
(asdf:oos 'asdf:load-op :lispbuilder-sdl)

;;;; make a namespace
(defpackage #:brain
  (:use #:cl))

(in-package #:brain)

;;some helper functions to abstract my data structure choice
(defun x (cell) (aref cell 0))
(defun y (cell) (aref cell 1))
(defun state (cell) (aref cell 2))
(defmethod (setf state) (new-state cell)
  (setf (aref cell 2) new-state))
(defun make-cell (x y &key state)
  (vector x y (or state
		  (if (< 50 (random 100))
		      :on :off ))))

(defvar *dim-board* '(90 90) "how many squares on the board")

(defun make-board ()
  (make-array *dim-board*))
(defun set-cell (board cell)
  (setf (aref board (x cell) (y cell)) cell))
(defmacro do-board ((x-var y-var cell-var)
		    &body body)
  "loop over the current board"
  (let ((x (gensym "x"))
	(y (gensym "y")))
  `(dotimes (,x (first *dim-board*))
     (dotimes (,y (second *dim-board*))
       (let ((,x-var ,x)
	     (,y-var ,y)
	     (,cell-var (aref *board* ,x ,y)))
	 ,@body)))))

(defvar *dim-screen* '(600 600) "size of the rendering window, pixels")
(defvar *dim-scale*  
  (map 'vector #'/ *dim-screen* *dim-board*)
  "pixels per grid square")

(defvar *board* nil "the state of the board")

(defun render-cell (cell)
  (let* ((x-scale (truncate (aref *dim-scale* 0)))
	 (y-scale (truncate (aref *dim-scale* 1)))
	 (x (truncate (1+ (* (x cell) x-scale))))
	 (y (truncate (1+ (* (y cell) y-scale))))
	 (state (state cell)))
    (sdl:with-color (c (if (eq state :dying)
			   sdl:*red*
			   sdl:*white*))
      (sdl:draw-box-* x y
			  (1- x-scale)
			  (1- y-scale)
			  ))))

(defun render ()
  (sdl:clear-display sdl:*black*)
  (do-board (x y cell)
    (declare (ignore x y))
    (unless (eq (state cell) :off)
      (render-cell cell)))
  (sdl:update-display))

(defun torus-window (cell)
  "returns the neighbors of this cell, wrapping around the
screen if needed"
  (loop for x-offset in '(-1 0 1)
	nconc
     (loop for y-offset in (if (zerop x-offset)
			       '(-1 1)
			       '(-1 0 1))		 
	   collect
	;;uses modulo to wrap around the window
	(let* ((x (mod (+ (x cell) x-offset)
		       (first *dim-board*)))
	       (y (mod (+ (y cell) y-offset)
		       (second *dim-board*))))
	  (aref *board* x y)))))

(defun active-neighbors (cells)
  (count :on cells :key #'state))

(defun next-state (cell)
  (let ((state (state cell))) 
    (cond
      ((eq :on state) :dying)
      ((eq :dying state) :off)
      ((eq 2 (active-neighbors (torus-window cell))) :on)
      (T :off))))

(defun next-board ()
  (let ((new-board (make-board)))
    (do-board (x y cell)
      (set-cell new-board
		(make-cell x y :state (next-state cell))))
    new-board))

(defun do-it ()  
  (sdl:with-init ()
    (apply #'sdl:window *dim-screen*)
    (setf *board* (make-board))
    (do-board (x y cell)
      (declare (ignore cell))
      (set-cell *board* (make-cell x y)))   
    (sdl:with-events ()
      (:quit-event () t)
      (:video-expose-event () (sdl:update-display))
      (:idle ()
	     (render)
	     (setf *board* (next-board))))))
 