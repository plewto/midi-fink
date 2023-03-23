;;;; machine-drum  write.lisp
;;;;
;;;; render & write midifile
;;;;

(in-package :machine-drum)

(labels ((render-header (this)
			(append '(#x4D #x54 #x68 #x64)
				(int->long 6)
				(int->short (mf-format this))
				(int->short (track-count this))
				(int->short (mf-division this))))
	 (save (this bytes path)
	       (format t ";;; Writing ~D bytes to MIDI file ~S~%" (length bytes) path)
	       (with-open-file (stream path
				       :direction :output
				       :if-exists :supersede
				       :if-does-not-exist :create
				       :element-type '(unsigned-byte 8))
			       (loop for byte in bytes do
				     (write-byte byte stream)))
	       (setf (mf-pathname this) path)) )
	
	(defmethod ->midi ((this midifile) &key (pad 1.0) bpm pathname &allow-other-keys)
	  "Converts MIDIFILE to list of bytes
:pad - Optional duration of silence added to end of each track.
:bpm - Optional tempo if track-0 does not have an explicit tempo event.
:pathname - Optional output pathname, if specified the results are written to that file,
            and the :pathname field of this is updated."
	  (let ((acc (render-header this)))
	    (loop for n from 0
		  for trk in (mf-tracks this) do
		  (when (zerop n)
		    (ensure-tempo-event! trk :bpm (or bpm *default-bpm*)))
		  (ensure-end-of-track! trk pad)
		  (setf acc (append acc (->midi trk))))
	    (when pathname (save this acc pathname))
	    acc)))
