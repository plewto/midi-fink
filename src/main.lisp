;;;; midi-fink main.lisp
;;;;
;;;; Defines generic functions.

(in-package :midi-fink)

(defvar *default-midifile-division* 98)
(defvar *default-bpm* 120.0)

(defun set-default-division (n)
  "Sets the default MIDIFILE clock division."
  (setf *default-midifile-division* n))

(defun set-default-tempo (bpm)
  (setf *default-bpm* (float bpm)))

(defgeneric channel-message-p (this)
  (:documentation "Predicate, true if argument is any MIDI channel message or event."))
  
(defgeneric note-off-p (this)
  (:documentation "Predicate, true if argument is a NOTE-OFF message or event.
NOTE-ON messages with velocity 0 are considered to be NOTE-OFF."))

(defgeneric note-on-p (this)
  (:documentation "Predicate, true if argument is a NOTE-ON message or event.
NOTE-ON messages with velocity 0 are considered to be NOTE-OFF."))

(defgeneric polyphonic-pressure-p (this)
  (:documentation "Predicate, true if argument is a POLYPHONIC-PRESSURE message or event."))

(defgeneric keyed-message-p (this)
  (:documentation "Predicate, true if argument is any 'keyed' message or event.   
The keyed messages are: NOTE-OFF, NOTE-ON and POLYPHONIC-PRESSURE"))

(defgeneric control-change-p (this)
  (:documentation "Predicate, true if argument is a CONTROL-CHANGE message or event."))

(defgeneric program-change-p (this)
  (:documentation "Predicate, true if argument is a PROGRAM-CHANGE message or event."))

(defgeneric channel-pressure-p (this)
  (:documentation "Predicate, true if argument is a CHANNEL-PRESSURE message or event."))

(defgeneric pitch-bend-p (this)
  (:documentation "Predicate, true if argument is a PITCH-BEND message or event."))

(defgeneric system-exclusive (this)
  (:documentation "Creates new SYSTEM-EXCLUSIVE message."))

(defgeneric system-exclusive-p (this)
  (:documentation "Predicate, true if argument is a SYSTEM-EXCLUSIVE message or event."))

(defgeneric end-system-exclusive-p (this)
  (:documentation "Predicate, true if argument is and END-OF SYSTEM-EXCLUSIVE message or event."))

(defgeneric messagep (this)
  (:documentation "Predicate, true if argument is any sort of MIDI message."))

(defgeneric metap (this)
  (:documentation "Predicate, true if argument is any kind of MIDI meta message or event."))

(defgeneric meta-channel-prefix-p (this)
  (:documentation "Predicate, true if argument is a meta channel-prefix message or event."))

(defgeneric meta-end-of-track-p (this)
  (:documentation "Predicate, true if argument is a meta end-of-track message or event."))

(defgeneric meta-keysig-p (this)
  (:documentation "Predicate, true if argument is a meta key-signature message or event."))

(defgeneric meta-sequence-number-p (this)
  (:documentation "Predicate, true if argument is a meta sequence-number message or event."))

(defgeneric meta-sequencer-p (this)
  (:documentation "Predicate, true if argument is a meta sequencer specific message or event."))

(defgeneric meta-text-p (this)
  (:documentation "Predicate, true if argument is any sort of meta text message or event."))

(defgeneric meta-tempo-p (this)
  (:documentation "Predicate, true if argument is a meta tempo-change message or event."))

(defgeneric meta-smpte-p (this)
  (:documentation "Predicate, true if argument is a meta SMPTE message or event."))

(defgeneric meta-timesig-p (this)
  (:documentation "Predicate, true if argument is a meta time-signature message or event."))

(defgeneric ->vector (this)
  (:documentation "Converts argument to a vector."))

(defgeneric ->list (this)
  (:documentation "Converts argument to list."))

(defgeneric eventp (this)
  (:documentation "Predicate, true if this is an instance of event."))

(defgeneric event (time message)
  (:documentation "Constructs new instance of event."))

(defgeneric all-events-p (this)
  (:documentation "Predicate, true if every element of argument is an event."))

(defgeneric eventlist-p (this)
  (:documentation "Predicate, true if this is an instance of eventlist."))

(defgeneric push-event! (this event &key &allow-other-keys)
  (:documentation "Inserts event into this."))
			 
(defgeneric merge-events! (this events &key &allow-other-keys)
  (:documentation "Inserts copies of all events from events into this."))
  
(defgeneric midi-clone (this &key &allow-other-keys)
  (:documentation "Returns object identical to this."))

(defgeneric precedence (this)
  (:documentation "Each message type has an associated precedence.  While
  sorting an event list where two or more events have the same time, those
  events with a lower precedence are placed before those with a higher
  value."))

(defgeneric midi< (q r)
  (:documentation "For sorting purposes, an event q is considered
  less-then an event r if:
A) The event time of q is less then that of r.
B) Events q and r have the same time and the precedence of q is less-then
  the precedence of r."))

(defgeneric find-events (this &key test &allow-other-keys)
  (:documentation "Returns all events from this for which test is true."))

(defgeneric ->string (this)
  (:documentation "Returns string representation of this."))

(defgeneric tempo (this &optional unit)
  (:documentation "Creates new meta tempo-change message."))

(defgeneric meta-keysig-accidentals (this)
  (:documentation "Returns the number of sharps (positive) or flats (negative) from 
meta key-signature message."))

(defgeneric meta-keysig-minor-p (this)
  (:documentation "Returns true if meta key-signature is a minor key."))

(defgeneric dump-events (this &key stream)
  (:documentation "Displays human readable contents of argument."))

(defgeneric event-count (this)
  (:documentation "Returns the number of events contained by argument."))

(defgeneric events-end-time (this)
  (:documentation "Returns the maximum event time from eventlist."))

(defgeneric meta-timesig-beat (this)
  (:documentation "Returns numerator from meta time-signature message."))

(defgeneric meta-timesig-unit (this)
  (:documentation "Returns beat unit from meta time-signature message.
The result is one o f the following symbols: W H Q E or S."))

(defgeneric place-end-of-track (this &optional pad)
  (:documentation "Inserts a meta end-of-track event into an eventlist.
The EOT event is placed at the maximum event-time + pad. All previous EOT 
events are removed."))

(defgeneric trackp (this)
  (:documentation "Predicate, true if argument is a an instance of TRACK. "))

(defgeneric midi-sort (this)
  (:documentation "Non-destructively sorts MIDI events by time and message precedence. 
Returns a copy of the argument with the events sorted."))

(defgeneric midi-sort! (this)
  (:documentation "Destructively sort MIDI events by time and message precedence.
Returns argument."))

(defgeneric midifile-p (this)
  (:documentation "Predicate, true if argument is an instance of MIDIFILE."))

(defgeneric track-count (this)
  (:documentation "Returns number of tracks."))

(defgeneric mf-track (this n &key expect-header)
  (:documentation "Returns the nth track from this."))

(defgeneric mf-division (this)
  (:documentation "Returns MIDI file clock division."))

(defgeneric ->midi (this &key &allow-other-keys)
  (:documentation "Converts this into stream of MIDI bytes."))

(defgeneric chunks (this)
  (:documentation "Splits argument into individual MIDIFILE chunks.
The result is a nested list of association-list, each representing a chunk.
The chunk alist have the following fields:
    :type       - Always the keyword :chunk
    :start      - Start index of the chunk
    :id         - Chunk type id as 4-byte 'long'
    :byte-count - Number of data bytes.
    :data       - Actual data bytes."))

(defgeneric chunk-info (this &key hex)
  (:documentation "Displays info about midifile chunk.
When :hex is true include hexdump of chunk's data bytes."))

(defgeneric data-count (this running-status)
  (:documentation "Returns number of expected data bytes for status byte this.
If this is not a MIDI status byte, try running-status.
For END-OF-EXCLUSIVE returns 0.
For SYSTEM-EXCLUSIVE returns :SYSEX
For META returns :META
For all other possibilities raise an error."))

