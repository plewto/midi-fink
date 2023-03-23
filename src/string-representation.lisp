;;;; machine-drum string-representation.lisp
;;;;

(in-package :machine-drum)

(labels ((channel->str (this)
		       (let* ((status (car this))
			      (cmd (gethash (logand #xF0 status) +nomenclature+))
			      (channel (1+ (logand #x0F status)))
			      (data ""))
			 (loop for d in (cdr this)
			       for i from 1 do
			       (setf data (str+ data (sformat "D~d: ~3d  " i d))))
			 (sformat "~A  :channel ~2D  ~A" cmd channel data)))
	 
	 (system->str (this)
		      (let* ((status (car this))
			     (nom (gethash status +nomenclature+))
			     (count 0))
			(loop for item in (cdr this) 
			      never (plusp (logand #x80 item)) do
			      (setf count (1+ count)))
			(sformat "~A  data count: ~D" nom count)))
	 
	 (eot->str (this)
		   (str+ (gethash (car this) +nomenclature+)
			 "  "
			 (gethash (second this) +nomenclature+)))

	 (text->str (this)
		    (let ((status (car this))
			  (mtype (second this))
			  (text ""))
		      (multiple-value-bind (count index)(take-vlv (->vector this) 2)
					   (loop for i from 0 below count
						 for j from index do
						 (setf text (str+ text (int->ascii (nth j this))))))
		      (sformat "~A  ~A ~s"
			       (gethash status +nomenclature+)
			       (gethash mtype +nomenclature+)
			       text)))

	 (sequence-number->str (this)
			       (let ((status (car this))
				     (mtype (second this))
				     (seqn (take-short (->vector this) 3)))
				 (sformat "~A  ~A  ~D"
					  (gethash status +nomenclature+)
					  (gethash mtype +nomenclature+)
					  seqn)))

	 (channel-prefix->str (this)
			      (let ((status (car this))
				    (mtype (second this))
				    (channel (1+ (fourth this))))
				(sformat "~A ~A ~D"
					 (gethash status +nomenclature+)
					 (gethash mtype +nomenclature+)
					 channel)))
	 
	 (tempo->str (this)
		     (let ((status (car this))
			   (mtype (second this)))
		       (sformat "~A  ~A ~A BPM"
				(gethash status +nomenclature+)
				(gethash mtype +nomenclature+)
				(tempo this))))

	 ;; ISSUE TODO Validate
	 (smpte->str (this)
		     (let ((status (car this))
			   (mtype (second this))
			   (hr (third this))
			   (min (fourth this))
			   (sec (fifth this))
			   (frame (sixth this))
			   (fframe (seventh this)))
		       (sformat "~A  ~A  HR: ~D  :MIN ~D  :SEC ~D  :FRAME ~D/~D"
				(gethash status +nomenclature+)
				(gethash mtype +nomenclature+)
				hr min sec frame fframe)))
	 
	 (keysig->str (this)
		      (let ((status (car this))
			    (mtype (second this))
			    (accidentals (meta-keysig-accidentals this)))
			(sformat "~A  ~A  ~D ~A  ~A"
				 (gethash status +nomenclature+)
				 (gethash mtype +nomenclature+)
				 (abs accidentals)
				 (if (minusp accidentals) "flats" "sharps")
				 (if (meta-keysig-minor-p this) "minor" "major"))))

	 (sequencer->str (this)
			 (let ((status (car this))
			       (mtype (second this))
			       (count (take-vlv (->vector this) 2)))
			   (sformat "~A  ~A  count: ~D"
				    (gethash status +nomenclature+)
				    (gethash mtype +nomenclature+)
				    count)))

	 (eot->string (this)
		      (let ((status (car this))
			    (mtype (second this)))
			(sformat "~A ~A"
				 (gethash status +nomenclature+)
				 (gethash mtype +nomenclature+))))
	 
	 (timesig->str (this) 
		       (let ((status (car this))
			     (mtype (second this))
			     (beat (meta-timesig-beat this))
			     (unit (meta-timesig-unit this))
			     (clocks (nth 5 this))
			     (met (nth 6 this)))
			 (sformat "~A  ~A  ~D/~A   clocks: ~D  met: ~D"
				  (gethash status +nomenclature+)
				  (gethash mtype +nomenclature+)
				  beat unit clocks met)))

	 
	 (default-list-handler (this)
	   (sformat "~A" this))  )
	 
	(defmethod ->string ((this t))
	  (sformat "~A" this))

	(defmethod ->string ((this list))
	  (let ((status (car this)))
	    (cond ((channel-message-p status)(channel->str this))
		  ((or (system-exclusive-p status)
		       (end-system-exclusive-p status))
		   (system->str this))
		  ((metap status)
		    (let ((mtype (second this)))
		      (cond ((meta-text-p mtype)(text->str this))
			    ((meta-sequence-number-p mtype)(sequence-number->str this))
			    ((meta-channel-prefix-p mtype)(channel-prefix->str this))
			    ((meta-tempo-p mtype)(tempo->str this))
			    ((meta-smpte-p mtype)(smpte->str this))
			    ((meta-keysig-p mtype)(keysig->str this))
			    ((meta-end-of-track-p mtype)(eot->str this))
			    ((meta-timesig-p mtype)(timesig->str this))
			    ((meta-sequencer-p mtype)(sequencer->str this))
			    (t (midi-error "MACHINE-DRUM::->STRING dispatch function"
					   (list (sformat "Encountered an unknwon meta type: ~A" mtype)
						 (sformat "MIDI message was ~A" this)))))))
		  (t (default-list-handler this))))) )

