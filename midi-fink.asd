(defsystem "midi-fink"
  :version "0.1.0"
  :author "Steven Jones  :  plewto@gmail.com"
  :license "GPL"
  :depends-on ()
  :components ((:module "src"
			:serial t
			:components
			((:file "packages")
			 (:file "constants")
			 (:file "main")
			 (:file "utils")
			 (:file "exceptions")
			 (:file "types")
			 (:file "hexdump")
			 (:file "clone")
			 (:file "channel")
			 (:file "system") 
			 (:file "meta")
			 (:file "meta-text")
			 (:file "meta-sequence-number")
			 (:file "meta-channel-prefix")
			 (:file "meta-tempo")
			 (:file "meta-smpte")
			 (:file "meta-timesig")
			 (:file "meta-keysig")
			 (:file "meta-sequencer")
			 (:file "precedence")
			 (:file "event")
			 (:file "string-representation")
			 (:file "eventlist")
			 (:file "track")
			 (:file "chunk")
			 (:file "midifile")
			 (:file "write")
			 (:file "read"))))
  :description "midi-fink is a Lisp library for creating, reading and writing midifiles.
It may be used as a stand-alone library, but is intended as the MIDI backend for CYCO4."
  :in-order-to ((test-op (test-op "midi-fink/tests"))))

(defsystem "midi-fink/tests"
  :author ""
  :license ""
  :depends-on ("midi-fink"
               "rove")
  :components ((:module "tests"
			:serial t
			:components
			((:file "test-main")
			 (:file "test-types")
			 (:file "test-channel")
			 (:file "test-system")
			 (:file "test-meta")
			 (:file "test-meta-text")
			 (:file "test-meta-sequence-number")
			 (:file "test-meta-channel-prefix")
			 (:file "test-meta-tempo")
			 (:file "test-meta-smpte")
			 (:file "test-meta-timesig")
			 (:file "test-meta-keysig")
			 (:file "test-meta-sequencer")
			 (:file "test-precedence")
			 (:file "test-event")
			 (:file "test-eventlist")
			 (:file "test-midifile"))))
  :description "Test system for midi-fink"
  :perform (test-op (op c) (symbol-call :rove :run c)))
