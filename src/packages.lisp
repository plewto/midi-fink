;;;; midi-fink packages.lisp
;;;;
;;;; A Lisp library for creating, reading and writing midifiles.

(defpackage :midi-fink
  (:nicknames :fink)
  (:use :cl))

(in-package :midi-fink)

(export '(channel-message-p
	  channel-pressure
	  channel-pressure-p
	  control-change
	  control-change-p
	  dump-events
	  end-system-exclusive
	  end-system-exclusive-p
	  event
	  eventlist-p
	  eventp
	  find-events
	  keyed-message-p
	  merge-events
	  messagep
	  meta-channel-prefix
	  meta-channel-prefix-p
	  meta-end-of-track
	  meta-end-of-track-p
	  meta-keysig
	  meta-keysig-accidentals
	  meta-keysig-minor-p
	  meta-keysig-p
	  meta-sequence-number
	  meta-sequence-number-p
	  meta-sequencer
	  meta-sequencer-p
	  meta-smpte
	  meta-smpte-p
	  meta-tempo
	  meta-tempo-p
	  meta-text
	  meta-text-p
	  meta-timesig-p
	  metap
	  midi-clone
	  midifile
	  midifile-p
	  note-off
	  note-off-p
	  note-on
	  note-on-p
	  pitch-bend
	  pitch-bend-p
	  polyphonic-pressure
	  polyphonic-pressure-p
	  precedence
	  program-change
	  program-change-p
	  push-event!
	  set-default-division
	  set-default-tempo
	  system-exclusive
	  system-exclusive-p
	  tempo
	  trackp) :midi-fink) 
