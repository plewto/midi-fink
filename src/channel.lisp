;;;; machine-drum channel.lisp
;;;;
;;;; MIDI channel messages: note-off  note-on  poly-pressure
;;;;                        control-change program-change
;;;;                        channel-pressure  pitch-bend
;;;;

(in-package :machine-drum)

(defmethod messagep ((this t)) nil)

(defmethod messagep ((this integer))
  (or (channel-message-p this)
      (metap this)
      (system-exclusive-p this)
      (end-system-exclusive-p this)))

(defmethod messagep ((this list))
  (messagep (car this)))

(defmethod channel-message-p ((this t)) nil)

(defmethod channel-message-p ((this integer))
  (member (logand this #xF0)
	  (list +note-off+ +note-on+ +poly-pressure+
		+control-change+ +program-change+ +channel-pressure+
		+pitch-bend+)))

(defmethod channel-message-p ((this list))
  (channel-message-p (car this)))

(defun expect-midi-channel (fn-name c &optional (message ""))
  "Produces an error if argument is not a valid MIDI channel.
The value must be an integer in the closed interval [1,16]"
  (expect fn-name  c #'(lambda (q) (and (integerp q)(in-bounds q 1 16)))
	  "MIDI channel 1..16"
	  (sformat "~A  ~A" (type-of c) c)
	  message)
  nil)

(defun expect-data-byte (fn-name value &optional (message ""))
  "Produces an error if values is not a valid MIDI data byte.
value must be an integer in the closed interval [0,127]"
  (expect fn-name value #'(lambda (q)(and (integerp q)(in-bounds q 0 127)))
	  "MIDI data bytes 0..127"
	  (sformat "~A  ~A" (type-of value) value)
	  message)
  nil)

(defun note-off (channel key &optional (velocity 64))
  "Creates new NOTE-OFF message."
  (expect-midi-channel 'note-off channel "note-off")
  (expect-data-byte 'note-off key "mote-off key")
  (expect-data-byte 'note-off velocity "note-off velocity")
  (list (logior +note-off+ (1- channel)) key velocity))

(defmethod note-off-p ((this t)) nil)

(defmethod note-off-p ((this integer))(= (logand this #xF0) +note-off+))

(defmethod note-off-p ((this list))
  (let ((status (car this))
	(velocity (third this)))
    (if (integerp status)
	(cond ((note-off-p status) t)
	      ((and (note-on-p status)(integerp velocity)(zerop velocity)) t)
	      (t nil))
      nil)))

(defun note-on (channel key &optional (velocity 64))
  "Creates new NOTE-ON message.
If velocity is 0 , the result is a NOTE-OFF instead."
  (expect-midi-channel 'note-on channel "note-on")
  (expect-data-byte 'note-on key "note-on key")
  (expect-data-byte 'note-on velocity "note-on velocity")
  (let ((status (if (zerop velocity) +note-off+ +note-on+)))
    (list (logior status (1- channel)) key velocity)))

(defmethod note-on-p ((this t)) nil)

(defmethod note-on-p ((this integer))(= (logand this #xF0) +note-on+))

(defmethod note-on-p ((this list))
  (let ((status (car this))
	(velocity (third this)))
    (and (integerp status)
	 (integerp velocity)
	 (= (logand status #xF0) +note-on+)
	 (plusp velocity))))
	
(defun polyphonic-pressure (channel key pressure)
  "Creates new POLYPHONIC-PRESSURE message."
  (expect-midi-channel 'polyphonic-pressure channel "polyphonic-pressure")
  (expect-data-byte 'polyphonic-pressure key "polyphonic-pressure key")
  (expect-data-byte 'poyphonic-pressure pressure "polyphonic-pressure pressure")
  (list (logior +poly-pressure+ (1- channel)) key pressure))

(defmethod polyphonic-pressure-p ((this t)) nil)

(defmethod polyphonic-pressure-p ((this integer))
  (= (logand this #xF0) +poly-pressure+))

(defmethod polyphonic-pressure-p ((this list))
  (polyphonic-pressure-p (car this)))

(defmethod keyed-message-p ((this t))
  (or (note-off-p this)
      (note-on-p this)
      (polyphonic-pressure-p this)))

(defun control-change (channel controller value)
  "Creates new CONTROL-CHANGE message."
  (expect-midi-channel 'control-change channel "control-change")
  (expect-data-byte 'control-change controller "control-change controller")
  (expect-data-byte 'control-change value "control-change value")
  (list (logior +control-change+ (1- channel)) controller value))

(defmethod control-change-p ((this t)) nil)

(defmethod control-change-p ((this integer))
  (= (logand this #xF0) +control-change+))

(defmethod control-change-p ((this list))
  (control-change-p (car this)))

(defun program-change (channel program-number)
  "Creates new PROGRAM-CHANGE message."
  (expect-midi-channel 'program-change channel "program-change")
  (expect-data-byte 'program-change program-number "program-change program")
  (list (logior +program-change+ (1- channel)) program-number))

(defmethod program-change-p ((this t)) nil)

(defmethod program-change-p ((this integer))
  (= (logand this #xF0) +program-change+))

(defmethod program-change-p ((this list))
  (program-change-p (car this)))

(defun channel-pressure (channel pressure)
  "Creates new CHANNEL-PRESSURE message."
  (expect-midi-channel 'channel-pressure channel "channel-pressure")
  (expect-data-byte 'channel-pressure pressure "channel-pressure pressure")
  (list (logior +channel-pressure+ (1- channel)) pressure))

(defmethod channel-pressure-p ((this t)) nil)

(defmethod channel-pressure-p ((this integer))
  (= (logand this #xF0) +channel-pressure+))

(defmethod channel-pressure-p ((this list))
  (channel-pressure-p (car this)))

(defun pitch-bend (channel msb lsb)
  "Creates new PITCH-BEND message."
  (expect-midi-channel 'pitch-bend channel "pitch-bend")
  (expect-data-byte 'pitch-bend msb "pitch-bend msb")
  (expect-data-byte 'pitch-bend lsb "pitch-bend lsb")
  (list (logior +pitch-bend+ (1- channel)) msb lsb))

(defmethod pitch-bend-p ((this t)) nil)

(defmethod pitch-bend-p ((this integer))
  (= (logand this #xF0) +pitch-bend+))

(defmethod pitch-bend-p ((this list))
  (pitch-bend-p (car this)))


;; ISSUE: CAN THIS BE ELIMINATED
(labels ((dcount (status)
		 (if (member (logand status #xF0)
			     (list +channel-pressure+ +program-change+))
		     1
		   2)))

	(defmethod data-count ((this integer)(running-status integer))
	  (cond ((channel-message-p this)
		 (dcount this))
		((channel-message-p running-status)
		 (dcount running-status))
		((end-system-exclusive-p this) 0)
		((system-exclusive-p this) :sysex)
		((metap this) :meta)
		(t (midi-error
		    "(DATA-COUNT integer integer)"
		    (list "Expected either a MIDI status byte or RUNNING-STATUS"
			  (sformat "Got: this: x~2X   running-status: x~2X"
				   this running-status)))))))
