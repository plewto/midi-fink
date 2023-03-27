;;;; midi-fink precedence.lisp
;;;;

(in-package :midi-fink)


(defvar +precedence-alist+ (list (cons +meta+ 0)
				 (cons +system-exclusive+ 500)
				 (cons +end-exclusive+    510)
				 (cons +program-change+   520)
				 (cons +control-change+   530)
				 (cons +poly-pressure+    540)
				 (cons +channel-pressure+ 550)
				 (cons +pitch-bend+       560)
				 (cons +note-on+          570)
				 (cons +note-off+         580)
                                 (cons +track-name+       000)
                                 (cons +copyright+        001)
                                 (cons +sequence-number+  002)
                                 (cons +tempo+            003)
                                 (cons +time-signature+   004)
                                 (cons +key-signature+    005)
                                 (cons +channel-prefix+   006)
                                 (cons +smpte+            007)
                                 (cons +text+             100)
                                 (cons +instrument-name+  101)
                                 (cons +marker+           102)
                                 (cons +cue+              103)
                                 (cons +lyric+            104)
                                 (cons +seq-specific+     105)
                                 (cons +end-of-track+     999)))


(defmethod precedence ((this list))
  (cond ((metap this)
	 (cdr (assoc (second this) +precedence-alist+)))
	((channel-message-p this)
	 (let ((status (logand #xF0 (car this))))
	   (or (cdr (assoc status +precedence-alist+))
	       (midi-error 'precedence
			   (sformat "channel-message status x~X not handled" status)))))
	(t (let ((status (car this)))
	     (or (cdr (assoc status +precedence-alist+))
		 (midi-error 'precedence
			     (sformat "Status x~X not handled" status)))))))

