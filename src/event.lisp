;;;; midi-fink event.lisp
;;;;
;;;; An event combines a MIDI message with a time.
;;;;

(in-package :midi-fink)

(defclass event ()
  ((time :accessor event-time
	 :initarg :time
	 :initform 0.0
	 :type float)
   (message :accessor event-message
	    :initarg :message
	    :initform '()
	    :type list))
  (:documentation "An EVENT assigns a time to a MIDI message."))

(defmethod eventp ((this t)) nil)

(defmethod eventp ((this event)) t)

(defmethod all-events-p ((this t)) nil)

(defmethod all-events-p ((this list))
  (every #'eventp this))

(defmethod channel-message-p ((this event))
  (channel-message-p (event-message this)))

(defmethod note-off-p ((this event))
  (note-off-p(event-message this)))

(defmethod note-on-p ((this event))
  (note-on-p(event-message this)))

(defmethod polyphonic-pressure-p ((this event))
  (polyphonic-pressure-p(event-message this)))

(defmethod keyed-message-p ((this event))
  (keyed-message-p(event-message this)))

(defmethod system-exclusive-p ((this event))
  (system-exclusive-p(event-message this)))

(defmethod end-system-exclusive-p ((this event))
  (end-system-exclusive-p(event-message this)))

(defmethod metap ((this event))
  (metap(event-message this)))

(defmethod meta-channel-prefix-p ((this event))
  (meta-channel-prefix-p(event-message this)))

(defmethod meta-end-of-track-p ((this event))
  (meta-end-of-track-p(event-message this)))

(defmethod meta-keysig-p ((this event))
  (meta-keysig-p(event-message this)))

(defmethod meta-sequence-number-p ((this event))
  (meta-sequence-number-p(event-message this)))

(defmethod meta-sequencer-p ((this event))
  (meta-sequencer-p(event-message this)))

(defmethod meta-text-p ((this event))
  (meta-text-p(event-message this)))

(defmethod meta-smpte-p ((this event))
  (meta-smpte-p (event-message this)))

(defmethod meta-tempo-p ((this event))
  (meta-tempo-p (event-message this)))

(defmethod meta-timesig-p ((this event))
  (meta-timesig-p (event-message this)))

(defmethod event ((time float)(message list))
  "Constructs new Event."
  (when (expect "EVENT constructor" message #'messagep "A MIDI message")
    (make-instance 'event :time time :message message)))

(defmethod event ((time integer)(message list))
  (event (float time) message))

(defmethod precedence ((this event))
  "Returns the message precedence of this event."
  (precedence (event-message this)))

(defmethod midi< ((a list)(b list))
  "Compare two MIDI messages.
Message a is 'less-then' message b, if the precedence of a is less then that of b."
  (when (and (expect 'midi< a #'messagep "A MIDI message")
	     (expect 'midi< b #'messagep "A MIDI Message"))
    (< (precedence a)
       (precedence b))))

(defmethod midi< ((a event)(b event))
  "Compare two events.
Event a is 'less-then' b, if either:
1) The time of a is less then b
2) The event times re equal and the precedence of a is less then b."
  (cond ((meta-end-of-track-p a) nil)
	((meta-end-of-track-p b) t)
	((< (event-time a)(event-time b)) t)
	((> (event-time a)(event-time b)) nil)
	(t (< (precedence (event-message a))
	      (precedence (event-message b))))))

(defmethod midi= ((a event)(b event))
  (and (= (event-time a)
	  (event-time b))
       (midi= (event-message a)
	      (event-message b))))

(defmethod events-end-time ((this list))
  "Returns the maximum event-time from list of events."
  (let ((events (midi-sort this)))
    (event-time (car (reverse events)))))

(defmethod ->string ((this event))
  (sformat "Event ~9,4F  ~A" (event-time this)(->string (event-message this))))

(defmethod midi-clone ((this event) &key &allow-other-keys)
  (event (event-time this)
	 (midi-clone (event-message this))))

