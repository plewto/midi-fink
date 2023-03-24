;;;; midi-fink track.lisp
;;;;

(in-package :midi-fink)

(defclass track (eventlist)
  ((parent :accessor track-parent
	   :initarg :parent
	   :documentation "Reference to the parent MIDI file.")
   (track-number :accessor track-number
		 :initarg :number
		 :initform 0
		 :type integer)
   (current-tempo :reader track-tempo
		  :initform 120.0
		  :type float
		  :documentation "Value extracted from meta-tempo events.")
   (current-unit :reader track-beat-unit
		 :initform 'q
		 :type symbol
		 :documentation "Value extracted from meta-timesig events."))
  (:documentation "A TRACK is an extension of eventlist.
Tracks may only exists as a component of a MIDIFILE."))
   
(defmethod trackp ((this t)) nil)

(defmethod trackp ((this track)) t)

(defun track (parent &key (events '()) (number 0))
  "Constructs new instance of TRACK.
parent  - must be an instance of MIDIFILE.
:events - intiial list of events.
:number - the track number within the MIDIFILE."
  (when (and (expect 'track parent #'midifile-p "Instance of MIDIFILE")
	     (expect 'track events #'all-events-p "A valid list of MIDI events."))
    (let ((this (make-instance 'track
			       :parent parent
			       :number number
			       :events events)))
      (midi-sort! this)
      this)))

(defmethod mf-division ((this track))
  (mf-division (track-parent this)))

(defmethod midi-clone ((this track) &key (parent nil) &allow-other-keys)
  (track parent
	 :events (midi-clone (events this))
	 :number (track-number this)))


(labels ((track-header (bytes)
		       (append '(#x4D #x54 #x72 #x6B)
			       (int->long (length bytes)))) )

	(defmethod ->midi ((this track) &key (pad 1.0) (number 0) bpm &allow-other-keys)
	  (ensure-end-of-track! this (or pad 1.0))
	  (when (zerop number)
	    (ensure-tempo-event! this :bpm (or bpm *default-bpm*)))
	  (let ((division (mf-division this))
		(previous-time 0.0)
		(acc '()))
	    (loop for event in (events this) do
		  (let* ((current-time (event-time event))
			 (delta (- current-time previous-time))
			 (ticks (round (* delta division)))
			 (message (event-message event)))
		    (setf acc (append acc (int->vlv ticks) message))
		    (setf previous-time current-time)))
	    (append (track-header acc) acc))))



			      
    
    
	

