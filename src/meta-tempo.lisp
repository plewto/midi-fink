;;;; machine-drum meta-tempo
;;;;

(in-package :machine-drum)

(defmethod meta-tempo-p ((this t)) nil)

(defmethod meta-tempo-p ((this integer))
  (= this +tempo+))

(defmethod meta-tempo-p ((this list))
  (and (metap this)
       (meta-tempo-p (second this))))

(labels ((unit-scale (n unit)
		     (* n (cond  ((eq unit :w) 0.25)
				 ((eq unit :h) 0.50)
				 ((eq unit :e) 2.00)
				 ((eq unit :s) 4.00)
				 (t 1.0)))) )

	(defun meta-tempo (bpm &optional (unit :q))
	  (let* ((ubpm (unit-scale bpm unit))
		 (qdur (/ 60.0 ubpm))
		 (udur (truncate (* 1e6 qdur))))
	    (append (list +meta+ +tempo+ 3)
		    (->list (int->n24 udur)))))
	
	(defmethod tempo ((this list) &optional (unit :q))
	  (declare (ignore unit))
	  (when (expect 'machine-drum::tempo this
			#'(lambda (q)
			    (and (>= (length q) 6)
				 (= (car q)  +meta+)
				 (= (second q) +tempo+)))
			"MIDI meta-tempo message")
	    (let* ((udur (take-n24 (->vector this) 3))
		   (dur (/ udur 1e6)))
	      (/ 60.0 dur)))) )

