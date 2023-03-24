;;;; midi-fink precedence.lisp
;;;;

(in-package :midi-fink)


(defvar +precedence-alist+ (list (cons +meta+ 0)
				 (cons +system-exclusive+ 10)
				 (cons +end-exclusive+ 11)
				 (cons +program-change+ 20)
				 (cons +control-change+ 21)
				 (cons +poly-pressure+  21)
				 (cons +channel-pressure+ 21)
				 (cons +pitch-bend+ 21)
				 (cons +note-on+ 22)
				 (cons +note-off+ 23)
				 (cons +end-of-track+ 100)))

(defmethod precedence ((this list))
  (cond ((metap this)
	 (let ((mtype (second this)))
	   (or (cdr (assoc mtype +precedence-alist+))
	       (cdr (assoc +meta+ +precedence-alist+)))))
	((channel-message-p this)
	 (let ((status (logand #xF0 (car this))))
	   (or (cdr (assoc status +precedence-alist+))
	       (midi-error 'precedence
			   (sformat "channel-message status x~X not handled" status)))))
	(t (let ((status (car this)))
	     (or (cdr (assoc status +precedence-alist+))
		 (midi-error 'precedence
			     (sformat "Status x~X not handled" status)))))))

