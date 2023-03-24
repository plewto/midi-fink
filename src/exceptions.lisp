;;;; midi-fink exceptions.lisp
;;;;
;;;; Error and warning notifications.
;;;;

(in-package :midi-fink)

(defvar *enable-warnings*  t)

(defun midi-warning (&rest args)
  "Prints a warning message and returns NIL.
If *ENABLE-WARNINGS* is nil, this function does nothing."
  (when *enable-warnings*
    (format t "~%;; --------------------------------- MIDI WARNING~%")
    (loop for line in args do
	  (format t ";; --- WARN: ~A~%" line))
    (format t "~%"))
  nil)

(labels ((bar ()
	      (format t "***************************************** MIDI ERROR~%"))

	 (line (text)
	       (format t "*** ~A~%" text)) )
	 
	(defun midi-error (headline more)
	  (bar)
	  (line headline)
	  (line "")
	  (loop for text in more do (line text))
	  (bar)
	  (format t "~%")
	  (finish-output)
	  (error (sformat "MIDI-ERROR ~A" headline))
	  nil)

	(defun expect (headline item predicate expected &rest more)
	  "Produces an error if (predicate item) is nil.
headline - error message headline
item - value under test
predicate - test function
expected - error message indicating what was expected.
more - additional error text."
	  (if (not (funcall predicate item))
	      (midi-error headline
			  (append (list (sformat "Expected : ~A" expected)
					(sformat "Got      : ~A" item))
				  more))
	    t)) )
