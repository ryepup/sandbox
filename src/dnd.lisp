(require 'cl-ppcre)
(require 'alexandria)
(require 'split-sequence)
(require 'iterate)
(use-package 'iterate)
(defvar *rolls* (make-hash-table))

(defun read-rolls ()
  (handler-case 
      (iter (print-rolls T 0 (save-roll (read-line))))
    (end-of-file () (print-rolls))))

(defun save-roll (rolldef)
  (labels
      ((ensure-subhash (key hash)
	 (alexandria:ensure-gethash key hash (make-hash-table)))
       (parse-rolls (hash s)
	 (handler-case
	     (iter (with dice = (ensure-subhash (read s) hash))
		   (for roll = (read s))
		   (until (not (numberp roll)))		  
		   (incf (gethash roll dice 0))
		   (finally (return T)))
	   (end-of-file () nil))))
    (with-input-from-string (s rolldef)
      (let ((player-rolls (ensure-subhash (read s) *rolls* )))
	(iter (while (parse-rolls player-rolls s)))
	player-rolls))))


(defun print-rolls (&optional (stream T) (indent 0) (hash *rolls*))
  (iter (for (k v) in-hashtable hash)
	(with indentation = (make-array indent :element-type 'character :initial-element #\space))	
	(if (hash-table-p v)
	    (progn
	      (format stream "~a~a:~%" indentation k)
	      (print-hash stream (+ 1 indent) v))
	    (format stream "~a~a:~a~%" indentation k v))))
