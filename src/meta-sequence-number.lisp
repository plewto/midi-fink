;;;; machine-drum   meta-sequence-number
;;;;

(in-package :machine-drum)

(defmethod meta-sequence-number-p ((this t)) nil)

(defmethod meta-sequence-number-p ((this integer))
  (= this +sequence-number+))

(defmethod meta-sequence-number-p ((this list))
  (and (metap this)
       (meta-sequence-number-p (second this))))

(defun meta-sequence-number (n)
  (append (list +meta+ +sequence-number+ 2)
	  (->list (int->short n))))

