;;;; machine-drum meta-sequencer.lisp
;;;;
;;;;

(in-package :machine-drum)

(defmethod meta-sequencer-p ((this t)) nil)

(defmethod meta-sequencer-p ((this integer))
  (= this +seq-specific+))

(defmethod meta-sequencer-p ((this list))
  (and (metap this)
       (meta-sequencer-p (second this))))

(defun meta-sequencer (values-list)
    (append (list +meta+ +seq-specific+)
	    (int->vlv (length values-list))
	    values-list))

