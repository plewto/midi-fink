;;;; machine-drum meta-keysig.lisp
;;;;

(in-package :machine-drum)

(defmethod meta-keysig-p ((this t)) nil)

(defmethod meta-keysig-p ((this integer))
  (= this +key-signature+))

(defmethod meta-keysig-p ((this list))
  (and (metap this)
       (meta-keysig-p (second this))))

(defun meta-keysig (nsharps minor)
  (list +meta+ +key-signature+ 2 (logand #x7F nsharps) (if minor 1 0)))

(defmethod meta-keysig-accidentals ((this list))
  (let ((n (nth 3 this)))
    (cond ((in-bounds n 0 7) n)
	  ((in-bounds n 121 127)(- n 128))
	  (t (midi-warning "META-KEYSIG ACCIDENT-COUNT OUT OF BOUNDS"
			   "Expected value in intervals [0,7] or [121,127]"
			   (sformat "got ~D, using default 0"))
	     0))))

(defmethod meta-keysig-minor-p ((this list))
  (plusp (nth 4 this)))

