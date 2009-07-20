(in-package :sandbox)

(defclass sattelite (ship) ()

  (:default-initargs
      :size 20))

(defmethod draw-surface ((s sattelite))
  (let* ((size (size s))
	 (surf (sdl:create-surface size size :color-key (sdl:color))))

    (sdl:draw-box-* 0 0 size size
		    :surface surf
		    :color (sdl:color :r 200
				      :g 200
				      :b 200)
		    :stroke-color (team s))
    surf))

(defmethod act ((sat sattelite))
  (let ((ships (find-ships sat 300)))
    (iter (for ship in ships)
	  (if (eq (team sat)
		  (team ship))
	      (help-friend sat ship)
	      (when (> (energy sat)
		       (* 2 (weapon-energy-cost :laser)))
		(attack-with-weapon sat ship :laser)
		)
	      )
	  )
    
    )
  
  (setf (energy sat)
	(alexandria:clamp (+ (energy sat) 5)
			  0
			  500)))

(defun help-friend (helper helpee)
  (let ((deficit  (- (max-energy helpee) (energy helpee))))
    (when (plusp deficit)
      (decf (energy helper) deficit)
      (incf (energy helpee) deficit)
      (sdl:draw-line (location helper)
		     (location helpee)
		      :color (sdl:color :b (dice:roll "6d10+195"))))))