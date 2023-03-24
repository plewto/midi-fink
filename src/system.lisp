;;;; midi-fink system.lisp
;;;;
;;;; MIDI system messages: system-exclusive  end-exclusive
;;;;

(in-package :midi-fink)

(defmethod system-exclusive ((length integer))
  "Creates new SYSTEM-EXCLUSIVE message indicated data length.
The data bytes are initially set to 0.
The resulting message includes both SYSTEM-EXCLUSIVE and END-EXCLUSIVE
status bytes."
  (let ((data (loop for i from 0 below length collect 0)))
    (append (list +system-exclusive+) data (list +end-exclusive+))))

(defmethod system-exclusive ((data list))
  (when (expect 'system-exclusive data
		#'(lambda (q)(every-in-bounds q 0 127))
		"Data bytes in closed interval [0..127]"
		"One or more values are out of bounds.")
    (append (list +system-exclusive+) data (list +end-exclusive+))))


(defmethod system-exclusive-p ((this t)) nil)

(defmethod system-exclusive-p ((this integer))
  (= this +system-exclusive+))

(defmethod system-exclusive-p ((this list))
  (and (system-exclusive-p (car this))
       (end-system-exclusive-p (car (reverse this)))))

(defmethod end-system-exclusive-p ((this t)) nil)

(defmethod end-system-exclusive-p ((this integer))
  (= this +end-exclusive+))

(defmethod end-system-exclusive-p ((this list))
  (end-system-exclusive-p (car this)))
