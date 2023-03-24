# MIDI-FINK

MIDI-FINK is a Lisp library for creating, reading and writing
midifiles.  It is intended as a support library for CYCO4 but may be used
independently. 

## Usage

A **midifile** is composed of a header and 1 or more **tracks**.
Each track is composed of a sequence of **events**, where an event is a
time-stamped MIDI **message**.

### Messages

There are no message types per'se.  Instead a message is a list of 1 or more 
bytes, and are identified by signature.   The following functions create new messages:

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

The EVENT function creates a new instance of the EVENT class. It takes a time in seconds and a MIDI
message as arguments.

    (event time message)

### Eventlist

An EVENTLIST is an ordered sequence of events.  Events may be specified at
construction time or may be added later. 

    ;; Creates new instance of EVENTLIST.
    (eventlist  &optional events)

    ;; push-event! adds a single event to the eventlist.
    (push-event! eventlist event)

    ;; merge-events! adds multiple events to an eventlist.
    (merge-events! eventlist list)
    (merge-events! eventlist other-eventlist)

### Track

The TRACK class extends EVENTLIST for use within a midifile.  Tracks should always
be in reference to a parent midifile.

    ;; Creates new track
    (track  parent-midi-file &key (events '()) (number 0))

### MIDIFILE

A MIDIFILE is the top-level object which consist of one or more tracks and
some general information.  They may be created either by the MIDIFILE or
READ-MIDIFILE functions.

    ;; Creates new midifile
    (midifile &key division format tracks)
    
        :division - resolution in ticks per beat,  defaults to \*DEFAULT-MIDIFILE-DIVISION*

        :format - midifile format, may be 0, 1 or 2.  defaults to 0.
                  format 2 is included for completeness but not really supported.

        :tracks - number of tracks.  For format 0 the track count must be 1.
                  Defaults to 1.

        Each track's tempo defaults to \*DEFAULT-BPM* unless an explicit
        meta-tempo event is present. 
    
    ;; Returns number of midifile tracks
    (track-count midifile)
    
    ;; Returns a specific track from midifile.
    (mf-track midifile track-number)
    
    ;; Inserts an event into a midifile track.
    (push-event! midifile event &key (track 0))
    
    ;; Merges an eventlist, or list of events into a midifile track.
    (merge-events! midifile events &key (track 0))
    
    ;; Renders midifile to bytes and optionally saves it to the file-system
    (->midi midifile &key pad BPM pathname)
    
    ;; Checks if a file exists AND that it is a midifile
    (probe-midifile pathname)
    
    ;; Creates MIDIFILE object by reading a midi file.
    (read-midifile pathname)

Two global variables set default midifile parameters

* **\*DEFAULT-MIDIFILE-DIVISION***  the default clock resolution in ticks per beat.
* **\*DEFAULT-BPM***  the default tempo in  Beats Per Minute.


## Installation

For the moment load from source.
Eventually midi-fink should be available as a quicklisp package. 