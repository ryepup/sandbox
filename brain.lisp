;;;; load our libs
(require 'asdf)
(asdf:oos 'asdf:load-op :lispbuilder-sdl)
(asdf:oos 'asdf:load-op :pcall)

;;;; make a namespace
(defpackage #:brain (:use #:cl))
(in-package #:brain)

(defvar *dim-board* '(90 90) "how many squares on the board")
(defvar *board* nil "the state of the board")
(defvar *stage* nil "staging memory for the next board")

;;helpers for dealing with boards
(defun make-board ()
  (make-array *dim-board*))
(defun set-cell (x y state &key (board *board*))
  (setf (aref board x y) state))
(defun get-cell (x y &key (board *board*))
  (aref board x y))

(defmacro do-board ((x-var y-var cell-var) &body body)
  "loop over *board*"
  (let ((x (gensym "x"))
	(y (gensym "y")))
  `(dotimes (,x (first *dim-board*))
     (dotimes (,y (second *dim-board*))
       (let ((,x-var ,x)
	     (,y-var ,y)
	     (,cell-var (get-cell ,x ,y)))
	 ,@body)))))

(defvar *dim-screen* '(540 540) "size of the rendering window, pixels")
(defvar *dim-scale*  (mapcar #'/ *dim-screen* *dim-board*)
  "pixels per grid square")

(defvar *dying* nil "SDL surface for a dying cell")
(defvar *alive* nil "SDL surface for an alive cell")
(defun make-cell-surface (color)
  "makes an SDL surface for a cell of the given color"
  (destructuring-bind (x-scale y-scale) *dim-scale*
    (let ((surface (sdl:create-surface x-scale y-scale)))
      (sdl:draw-box-* 1 1 (1- x-scale) (1- y-scale)
		      :color color
		      :surface surface)
      surface)))

(defun render-cell (x y state)
  (destructuring-bind (x-scale y-scale) *dim-scale*
    (let ((x (1+ (* x x-scale)))
	  (y (1+ (* y y-scale)))
	  (surface (if (eq state :dying)
		       *dying* *alive*)))
      (sdl:set-point-* surface :x x :y y)
      (sdl:blit-surface surface))))

(defun render ()
  (do-board (x y state)
    (unless (eq state :off)
      (render-cell x y state))))

(defun torus-window (x y)
  "returns the neighbors of this cell, wrapping around the
screen if needed"
  (loop for x-offset in '(-1 0 1)
	nconc
     (loop for y-offset in '(-1 0 1)
	   unless (and (zerop x-offset)
		       (zerop y-offset))
	   collect (get-cell
		       (mod (+ x x-offset)
			    (first *dim-board*))
		       (mod (+ y y-offset)
			 (second *dim-board*))))))

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
    (setf (sdl:frame-rate) 0)
    (setf *board* (make-board) ;need to setf so threads can see these too
	  *stage* (make-board)) 
    (do-board (x y cell)
      (declare (ignore cell))
      (set-cell x y (if (< 50 (random 100))
			:on :off)))
    (let ((*dying* (make-cell-surface sdl:*red*))
	  (*alive* (make-cell-surface sdl:*white*)))      
      (sdl:with-events ()
	(:quit-event () t)
	(:video-expose-event () (sdl:update-display))
	(:idle ()	       
	       (pcall:plet ((next-board (next-board *stage*)))
		 (sdl:clear-display sdl:*black*)
		 (render)
		 (sdl:update-display)
		 (unless next-board (error "whoa, joining the thread failed"))
		 (let ((x *board*))
		   (setf *board* *stage*
			 *stage* x)))))
      (sdl:free *dying*)
      (sdl:free *alive*)
      (sdl:average-fps))))
