;;;; machine-drum eventlist.lisp
;;;;

(in-package :machine-drum)

(defclass eventlist ()
  ((events :accessor events
	   :initarg :events
	   :initform '()
	   :type list))
  (:documentation "An EVENTLIST is an ordered sequence of MIDI events."))

(defmethod eventlist-p ((this t)) nil)

(defmethod eventlist-p ((this eventlist)) t)

(defun eventlist (&optional events)
  "Constructs new instance of EVENTLIST."
  (when (expect 'eventlist events #'all-events-p "A list of MIDI events")
    (let ((this (make-instance 'eventlist :events events)))
      (midi-sort! this)
      this)))
	
(defmethod push-event! ((this eventlist)(event event) &key &allow-other-keys)
  "Inserts event into this eventlist.
The eventlist is sorted after the insertion."
  (push event (events this))
  (midi-sort! this)
  this)

(defmethod event-count ((this eventlist))
  (length (events this)))

(defmethod midi-clone ((this eventlist) &key &allow-other-keys)
  (eventlist (midi-clone (events this))))

(defmethod merge-events! ((this eventlist)(other list) &key &allow-other-keys)
  (when (expect 'merge-events! other #'all-events-p "A list of MIDI events")
    (setf (events this)
	  (append (events this)(midi-clone other)))
    (midi-sort! this)
    this))

(defmethod merge-events! ((this eventlist)(other eventlist) &key &allow-other-keys)
  (setf (events this)
	(append (events this)(midi-clone (events other))))
  (midi-sort! this)
  this)

(defmethod find-events ((this eventlist) &key (test #'true) &allow-other-keys)
  (let ((acc '()))
    (loop for event in (events this) do
	  (when (funcall test event)
	    (push event acc)))
    (reverse acc)))

(defmethod events-end-time ((this eventlist))
  "Returns the maximum event time from this eventlist.
The eventlist is sorted as a side-effect."
  (if (plusp (event-count this))
      (progn
	(midi-sort! this)
	(event-time (car (reverse (events this)))))
    0.0))

(labels ((remove-eot (this)
		   (setf (events this)
			 (remove-if #'meta-end-of-track-p (events this)))) )

	(defmethod ensure-end-of-track! ((this eventlist) &optional (pad 0.0))
	  "Ensures this eventlist has a MIDI meta-end-of-track event.
If an end-of-track event is inserted it is placed pad seconds after the final (non EOT) time."
	(remove-eot this)
	(let ((end-time (+ (events-end-time this) pad)))
	  (push-event! this (event end-time (meta-end-of-track))))))

(labels ((has-time-zero-tempo (this)
			    (find-events this
					 :test #'(lambda (ev)
						   (and (zerop (event-time ev))
							(meta-tempo-p (event-message ev))))))
	 (has-any-tempo (this)
			(let ((found (find-events this :test #'meta-tempo-p)))
			  (sort found #'(lambda (a b)
					  (< (event-time a)
					     (event-time b)))))) )
	
	(defmethod ensure-tempo-event! ((this eventlist) &key bpm)
	  "Ensures this eventlist has a meta-tempo event at time 0.0
There are three scenarios:

1) The eventlist already contains a tempo event at time 0.0:  do nothing
2) There is at least one existing tempo event, but none have a time of 0:
   Insert a copy of the earliest tempo event at time 0.  The existing event is unchanged.
3) The eventlist contains no tempo events:  Insert a default tempo event at time 0."

	  (let* ((zero (has-time-zero-tempo this))
		 (any (car (has-any-tempo this))))
	    (cond (zero nil)
		  (any (let ((zero-tempo (midi-clone any)))
			 (setf (event-time zero-tempo) 0.0)
			 (push-event! this zero-tempo)))
		  (t (push-event! this (event 0.0 (meta-tempo (or bpm *default-bpm*)))))))) )
	    
(defmethod ->string ((this eventlist))
  (sformat "Eventlist   ~D events" (event-count this)))

(defmethod dump-events ((this list) &key (stream t))
  (loop for i from 0
	for event in this do
	(format stream "[~4D] ~A~%" i (->string event))))

(defmethod dump-events ((this eventlist) &key (stream t))
  (format stream "~A~%" (type-of this))
  (dump-events (events this)))

(defmethod midi-sort ((this list))
  (when (expect 'midi-sort this #'all-events-p "List of MIDI events")
    (sort (midi-clone this) #'midi<)))

(defmethod midi-sort! ((this eventlist))
  (setf (events this)
	(midi-sort (events this)))
  this)

