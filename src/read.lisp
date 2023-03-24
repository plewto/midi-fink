;;;; midi-fink   read.lisp
;;;;
;;;; Read midifile
;;;;

(in-package :midi-fink)


(defun probe-midifile (pathname)
  "Checks for existence of a midifile.
Returns t if the file pathname exists and the first 4 bytes contains 
a midifile header id of 'MThd'."
  (and (probe-file pathname)
       (let ((id ""))
	 (with-open-file (stream pathname
				 :direction :input
				 :element-type '(unsigned-byte 8))
			 (loop for i from 0 below 4 do
			       (let ((b (read-byte stream nil 32)))
				 (setf id (str+ id (int->ascii b))))))
	 (string= id "MThd"))))

(labels ((take-event (bytes index division previous-time running-status)
		     "Extracts an EVENT form raw MIDI bytes starting at index."
		     (multiple-value-bind (delta start-of-message)
					  (take-vlv bytes index)
					  (multiple-value-bind (msg start-next-event)
							       (take-message bytes start-of-message running-status)
							       (let ((time (+ previous-time (float (/ delta division)))))
								 (values (event time msg) start-next-event)))))

	 (parse-events (bytes division)
		       "Converts vector of MIDI bytes to an eventlist."
		       (let ((previous-time 0.0)
			     (running-status 0)
			     (index 0)
			     (acc (eventlist)))
			 (while (< index (length bytes))
			   (multiple-value-bind (event next)
						(take-event bytes index division previous-time running-status)
						(push-event! acc event)
						(setf previous-time (event-time event))
						(setf running-status (if (channel-message-p event)
									 (car (event-message event))
								       0))
						(setf index next)))
			 acc))
			
	 
	 (validate-path (pathname)
			"Checks that pathname both exists, and that it's first 4-bytes is 'MThd'
                         Raise a MIDI-ERROR if pathname is not a valid midifile."
			(if (not (probe-midifile pathname))
			    (progn
			      (midi-error "Invalid MIDI file"
					  (list
					   (sformat "Either the path ~S does not exists, or" pathname)
					   "it does not contain the required header signature: 'MThd'"))
			      nil)
			  t))

	 (read-bytes (pathname)
		     "Reads all bytes from midifile."
		     (let ((acc '()))
		       (with-open-file (stream pathname
					       :direction :input
					       :element-type '(unsigned-byte 8))
				       (let ((b (read-byte stream nil nil)))
					 (while b
					   (push b acc)
					   (setf b (read-byte stream nil nil)))))
		       (->vector (reverse acc))))

	 (is-track-p (chunk-alist)
		     "Predicate, true iff chunk id is 'MTrk'."
		     (= (cdr (assoc :id chunk-alist))
			#x4D54726B))
	 
	 (mark-track-chunks (chunk-alist)
			    "Returns a list of Boolean flags for each chunk in chunk-alist
                             A flag is true if the corresponding chuck is a track (id = 'MTrk'),
                             otherwise the flag is nil.  Only two chunk types are supported:
                             'MThd' and 'MTrk'.  If an un-recognized chuck is detected, a 
                              warning message is displayed, and the chunk contents are ignored."
			     (let ((count 0)
				   (acc '()))
			       (loop for i from 0
				     for chunk-alist in chunk-alist do
				     (if (is-track-p chunk-alist)
					 (progn
					   (push t acc)
					   (setf count (1+ count)))
				       (progn
					 (push nil acc)
					 (when (plusp i)
					   (format t "*** MIDI WARNING: Encountered non-track chunk~%" )
					   (format t "*** MIDI WARNING: in midifile~%")
					   (format t "*** MIDI WARNING: Expected 'MTrk' got '~A'~%"
						   (int->ascii-string (cdr (assoc :id chunk-alist))))))))
			       (cons (reverse acc) count))) )

	(defmethod read-midifile ((pathname string))
	  "Reads midifile form given pathname."
	  (format t ";;;; Reading MIDI file ~s~%" pathname)
	  (when (validate-path pathname)
	    (let* ((chunk-alist (chunks (read-bytes pathname)))
		   (header (cdr (assoc :data (first chunk-alist))))
		   (form (take-short header 0))
		   (division (take-short header 4))
		   (track-flags (mark-track-chunks chunk-alist))
		   (track-count (cdr track-flags))
		   (mfile (midifile :division division :format form :tracks track-count))
		   (track-number 0))
	      (setf (mf-pathname mfile) pathname)
	      (loop for flag in (car track-flags)
		    for chunk in chunk-alist do
		    (when flag
		      (let ((bytes (cdr (assoc :data chunk))))
			(merge-events! mfile (parse-events bytes division) :track track-number)
			(setf track-number (1+ track-number)))))
	      mfile)))) 
