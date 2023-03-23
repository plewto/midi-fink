;;;; machine-drum meta.lisp
;;;;
;;;;

(in-package :machine-drum)

(defmethod metap ((this t)) nil)

(defmethod metap ((this integer))
  (= this +meta+))

(defmethod metap ((this list))
  (metap (car this)))

(defmethod meta-end-of-track-p ((this t)) nil)

(defmethod meta-end-of-track-p ((this integer))
  (= this +end-of-track+))

(defmethod meta-end-of-track-p ((this list))
  (and (metap this)
       (meta-end-of-track-p (second this))))

(defun meta-end-of-track ()
  "Creates new meta end-of-track message."
  (list +meta+ +end-of-track+ 0))

