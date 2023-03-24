;;;; machine-drum packages.lisp
;;;;
;;;; A Lisp library for creating, reading and writing midifiles.

(defpackage :machine-drum
  (:nicknames :mdrum)
  (:use :cl))

(in-package :machine-drum)

(export '(channel-message-p
	  channel-pressure-p
	  control-change-p
	  dump-events
	  end-system-exclusive-p
	  event
	  eventlist-p
	  eventp
	  find-events
	  keyed-message-p
	  merge-events
	  messagep
	  meta-channel-prefix-p
	  meta-end-of-track-p
	  meta-keysig-accidentals
	  meta-keysig-minor-p
	  meta-keysig-p
	  meta-sequence-number-p
	  meta-sequencer-p
	  meta-smpte-p
	  meta-tempo-p
	  meta-text-p
	  meta-timesig-p
	  metap
	  midi-clone
	  midifile-p
	  note-off-p
	  note-on-p
	  pitch-bend-p
	  polyphonic-pressure-p
	  precedence
	  program-change-p
	  push-event
	  set-default-division
	  set-default-tempo
	  system-exclusive
	  system-exclusive-p
	  tempo
	  trackp) :machine-drum) 
