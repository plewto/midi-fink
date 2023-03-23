;;;; machine-drum meta-smpte.lisp
;;;;

(in-package :machine-drum)

(defmethod meta-smpte-p ((this t)) nil)

(defmethod meta-smpte-p ((this integer))
  (= this +smpte+))

(defmethod meta-smpte-p ((this list))
  (and (metap this)
       (meta-smpte-p (second this))))

(defun meta-smpte (&key (hr 0)(min 0)(sec 0)(frame 0)(frame-fraction 0))
  (list +meta+ +smpte+ 5 hr min sec frame frame-fraction))

