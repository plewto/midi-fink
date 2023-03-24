;;;; midi-fink midifile
;;;; Standard MIDI File
;;;;

(in-package :midi-fink)

(defclass midifile ()
  ((division :reader mf-division
	     :initarg :division
	     :initform 96
	     :type integer
	     :documentation "Clock resolution in ticks per beat.")
   (format :reader mf-format
	   :initarg :format
	   :initform 1
	   :type integer
	   :documentation "File format, maybe either 0 or 1. Format 2 midifiles are not supported.")
   (tracks :reader mf-tracks
	   :initarg :tracks
	   :initform '()
	   :type list
	   :documentation "List of instances of TRACK.")
   (pathname :accessor mf-pathname
	     :initarg :path
	     :initform #p""
	     :type pathname
	     :documentation  "File system pathname most recently used to read/write this to midi file."))
  (:documentation  "Models a standard MIDI file as a set of tracks together with common header values."))

(defmethod midifile-p ((this t)) nil)

(defmethod midifile-p ((this midifile)) t)

(defun midifile (&key (division nil)(format 0)(tracks 1))
  "Constructs new instance of MIDIFILE
:division - The resolution of clock ticks per beat.
            Defaults to *DEFAULT-MIDIFILE-DIVISION*
:format   - The MIDIFILE format. May be either 0, 1, or 2, default 0.
            Format 2 is not supported.
:tracks   - Number of tracks, default 1"
  (when (and (zerop format)(> tracks 1))
    (progn (midi-warning "MIDIFILE has incompatible format and track count."
			 (sformat "format      : ~A" format)
			 (sformat "track-count : ~A" tracks)
			 "Changing to format 1.")
	   (setf format 1)))
  (let ((this (make-instance 'midifile
			     :division (or division *default-midifile-division*)
			     :format format)))
    (setf (slot-value this 'tracks)
	  (loop for i from 0 below tracks collect (track this :number i)))
    this))

(defmethod track-count ((this midifile))
  "Returns number of MIDIFILE tracks."
  (length (mf-tracks this)))

(defmethod mf-track ((this midifile)(n integer) &key (expect-header 'mf-track))
  "Returns specific track from MIDIFILE.
this - the MIDIFILE.
n    - the track number.
:expect-head - name of calling function used for error message only."
  (when (expect expect-header n #'(lambda (q)(in-bounds q 0 (1- (track-count this))))
		(sformat "Track number in interval [0,~D]" (1- (track-count this))))
    (nth n (mf-tracks this))))

(defmethod midi-clone ((this midifile) &key &allow-other-keys)
  "Creates deep copy of the MIDIFILE."
  (let* ((other (midifile :division (mf-division this)
			 :format (mf-format this)
			 :tracks (track-count this)))
	 (trks (loop for i from 0 below (track-count this) collect
		     (midi-clone (mf-track this i :expect-header 'midi-clone) :parent other))))
    (setf (slot-value other 'tracks) trks)
    other))

(defmethod push-event! ((this midifile)(event event) &key (track 0) &allow-other-keys)
  "Inserts an event into specific track
this   - The MIDIFILE
event  - The event
:track - track number."
  (push-event! (mf-track this track :expect-header 'push-event!) event))

(defmethod merge-events! ((this midifile)(events list) &key (track 0) &allow-other-keys)
  (merge-events! (mf-track this track :expect-header 'merge-events!) events))

(defmethod merge-events! ((this midifile)(events eventlist) &key (track 0) &allow-other-keys)
  "Merges eventlist into track
this   - An instance of MIDIFILE 
events - The events to be merged.
:track - Track number."
  (merge-events! (mf-track this track :expect-header 'merge-events!) events))

(defmethod dump-events ((this midifile) &key (stream t))
  (format stream "MIDIFILE division : ~D~%" (mf-division this))
  (format stream "         format   : ~D~%" (mf-format this))
  (format stream "         tracks   : ~D~%" (track-count this))
  (format stream "         pathname : ~S~%" (mf-pathname this))
  (loop for n from 0 below (track-count this) do
	(format stream "TRACK ~2D~%" n)
	(dump-events (mf-track this n) :stream stream)))

(defmethod chunks ((this midifile))
  "Returns collection of chunks for this MIDIFILE."
  (chunks (->vector (->midi this))))


