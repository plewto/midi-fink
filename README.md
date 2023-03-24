# MIDI-FINK

MIDI-FINK is a Lisp library for creating, reading and writing
midifiles.  It is intended as a support library for CYCO4 but may be used
independently 

## Usage

A midifile is composed of a header and 1 or more tracks.
Each track is composed of a sequence of events, where an event is a
MIDI message with time-stamp.

### Messages

There is no message type per se, instead a message is a list of 1 or more MIDI
bytes.   The following functions are used to create messages:

    (note-off  channel key &optional velocity)
    (note-on   channel key &optional velocity)
    (polyphonic-pressure  channel key pressure)
    (control-change  channel controller-number value)
    (program-change  channel program-number)
    (channel-pressure  channel pressure)
    (pitch-bend  channel msb lsb)
    (system-exclusive  byte-list)
    (meta-channel-prefix channel)
    (meta-keysig number-sharps-or-flats minor)
    (meta-sequence-number n)
    (meta-sequencer byte-list)
    (meta-smpte &key hr min sec frame frame-fraction)
    (meta-tempo  bpm &optional unit)
    (meta-text  text &optional text-message-type)
    (meta-timesig beats &key unit clocks thirty-seconds)

### Events

The EVENT function creates new events, it takes a time in seconds and a MIDI
message.

    (event time message)

### Eventlist

An EVENTLIST is an ordered sequence of events.  Events may be specified at
construction time or may be added later.

    ;; Create new instance of eventlist
    (eventlist  &optional events)

    ;; push-event! adds a single event to the eventlist.
    (push-event! eventlist event)

    ;; merge-events! adds multiple events at once.
    (merge-events! eventlist list)
    (merge-events! eventlist other-eventlist)

### Track

A TRACK extends EVENTLIST for use within a midifile.  Tracks should always
be in reference to a parent midifile.

    ;; Creates new track
    (track  parent-midi-file &key (events '()) (number 0))

### MIDIFILE

A MIDIFILE is the top-level object which consist of one or more tracks and
some general information.

    ;; create new midifile
    (midifile &key division format tracks)
    
    ;; get number of midifile tracks
    (track-count midifile)
    
    ;; get track from midifile
    (mf-track midifile track-number)
    
    ;; insert event in track 
    (push-event! midifile event &key (track 0))
    
    ;; merge events into track
    (merge-events! midifile events &key (track 0))
    
    ;; render midifile to bytes and optionally save to the file-system
    (->midi midifile &key pad BPM pathname)
    
    ;; check if a file exists AND that it is a midifile
    (probe-midifile pathname)
    
    ;; read file and create midifile object.
    (read-midifile pathname)

## Installation

For the moment install from source.  Eventually midi-fink should be
available as a quicklisp package. 