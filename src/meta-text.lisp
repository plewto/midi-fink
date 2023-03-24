;;;; midi-fink meta-text.lisp
;;;;
;;;;

(in-package :midi-fink)

(defmethod meta-text-p ((this t)) nil)

(defmethod meta-text-p ((this integer))
  (member this (list +text+ +copyright+ +track-name+ +marker+
		     +instrument-name+ +lyric+ +cue+)))

(defmethod meta-text-p ((this list))
  (and (metap this)
       (meta-text-p (second this))))

(defvar +text-type-alist+ (list (cons :text +text+)
				(cons :copyright +copyright+)
				(cons :track-name +track-name+)
				(cons :marker +marker+)
				(cons :instrument-name +instrument-name+)
				(cons :lyric +lyric+)
				(cons :cue +cue+)))

(defvar +reverse-text-type-alist+
  (loop for item in +text-type-alist+ collect
	(cons (cdr item)(car item))))

(defun meta-text (text &optional (mtype :text))
  "Creates new meta text message. 

The optional mtype argument may be one of the following
:text :copyright :track-name :marker :instrument-name :lyric or :cue"
  (let ((mtype (or (cdr (assoc mtype +text-type-alist+)) +text+)))
    (append (list +meta+ mtype)
	    (int->vlv (length text))
	    (loop for i from 0 below (length text) collect
		  (ascii->int (char text i))))))
