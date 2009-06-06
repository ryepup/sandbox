(in-package :sandbox)
(defvar *width* 800)
(defvar *height* 600)

(defun sdl (&key
	    (width 800)
	    (height 600)
	    (frame-rate 15)
	    (idlefn #'idle)
	    (background sdl:*black*))
  (let ((*width* width)
	(*height* height))
    (sdl:with-init ()
      (sdl:window *width* *height* )
      (sdl:clear-display background)
      (setf (sdl:frame-rate) frame-rate)
      (sdl:with-events ()
	(:quit-event () t)
	(:video-expose-event () (sdl:update-display))
	(:idle () 
	       (funcall idlefn)
	       (sdl:update-display))))))


(defun random-point ()
  (sdl:point :x (random *width*)
	     :y (random *height*)))

(defun random-pixel (color)
  (sdl:draw-pixel (random-point)
		  :color color))

(defun random-ball ()
  )

(defvar *last* nil)

(defun line-drawer ()
  (let ((p (random-close-point *last* (+ 10 (random 30)))))
    (sdl:draw-circle p 5 :color sdl:*green*)
    (sparkles p)
    (sdl:draw-line *last* p :color sdl:*cyan*)
    (setf *last* p)))

(defun random-close-point (point &optional (max-distance 10))
  (let* ((nd (1+ (* 2 max-distance)))
	(x (+ (sdl:x point) (- (random nd) max-distance)))
	(y (+ (sdl:y point) (- (random nd) max-distance ))))
    (if (and (< 0 x *width*) (< 0 y *height*))
	(sdl:point :x x :y y)
	(random-close-point point max-distance))))

(defun sparkles (center-point)
  (dotimes (n (+ 20 (random 100)))    
    (sdl:draw-pixel (random-close-point center-point (1+ n))
		    :color (sdl:color :g (+ 55 (random 200))
				      :r (random 200)
				      :b (random 200)))))

(defun fader ()
  (sdl:draw-box (sdl:rectangle :w *width* :h *height*)
		:color sdl:*black*
		:alpha (+ 50 (random 10))))

(defun laser ()
  (sdl:draw-line (sdl:point) *last* :color sdl:*red* )

  )

(defun lightning (start end &key (color sdl:*blue*))
  (let ((s start))
    (iterate
      (for p = (random-close-point s (max 10 (random 20))))
      (for d1 = (sdl:distance s end))
      (for d2 = (sdl:distance p end))
      (until (> 10 d1))
	     
      (when (> d1 d2)
	(collect p into pts)
	(setf s p))
      (finally
       (sdl:draw-shape pts :color color)))))
  

(defun idle ()
   (unless *last* (setf *last* (random-point)))
  (fader)
  (laser) 
  (line-drawer)
  (lightning (sdl:point :x *width* :y *height* ) *last*)
  (lightning (sdl:point :x *width* :y *height* ) *last*
	     :color (sdl:color :r (+ 50 (random 50))
			       :g (+ 50 (random 50)) :b 255))
  (lightning (sdl:point :x *width* :y *height* ) *last*
	     :color (sdl:color :r (+ 150 (random 50))
			       :g (+ 150 (random 50))
			       :b 255))
 
  )