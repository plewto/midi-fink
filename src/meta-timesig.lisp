;;;; midi-fink meta-timesig.lisp
;;;;

(in-package :midi-fink)

(defmethod meta-timesig-p ((this t)) nil)

(defmethod meta-timesig-p ((this integer))
  (= this +time-signature+))

(defmethod meta-timesig-p ((this list))
  (and (metap this)
       (meta-timesig-p (second this))))

(defun meta-timesig (beats &key (unit :q)(clocks 24)(thirty-seconds 24))
  (let ((u (cond ((eq unit :w) 0)
		 ((eq unit :h) 1)
		 ((eq unit :q) 2)
		 ((eq unit :e) 3)
		 ((eq unit :s) 4)
		 (t 2))))
    (list +meta+ +time-signature+ 4 beats u clocks thirty-seconds)))
			    
(defmethod meta-timesig-beat ((this list))
  (nth 3 this))

(defmethod meta-timesig-unit ((this list))
  (let ((u (nth 4 this)))
    (cond ((= u 0) :w)
	  ((= u 1) :h)
	  ((= u 2) :q)
	  ((= u 3) :e)
	  ((= u 4) :s)
	  (t (midi-warning "Meta timesignture, weird beat unit"
			   "Expected value in interval [0,4]"
			   (sformat "Got ~D, using default 2 (quarter-note)" u))
	     'q))))
			   
