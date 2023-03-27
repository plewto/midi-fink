;;;; midi-fink  test-channel
;;;;

(in-package :midi-fink/tests/main)

(deftest test-note-off
  (testing "NOTE-OFF message")
  (ok (let* ((chan 6)
	     (key 48)
	     (velocity 72)
	     (msg (midi-fink::note-off chan key velocity)))
	(and (listp msg)
	     (= (length msg) 3)
	     (midi-fink::messagep msg)
	     (midi-fink::channel-message-p msg)
	     (midi-fink::keyed-message-p msg)
	     (midi-fink::note-off-p msg)
	     (not (midi-fink::note-on-p msg))))))

(deftest test-note-on
  (testing "NOTE-ON message")
  (ok (let* ((chan 6)
	     (key 48)
	     (velocity 72)
	     (msg (midi-fink::note-on chan key velocity)))
	(and (listp msg)
	     (= (length msg) 3)
	     (midi-fink::messagep msg)
	     (midi-fink::channel-message-p msg)
	     (midi-fink::keyed-message-p msg)
	     (midi-fink::note-on-p msg)
	     (not (midi-fink::note-off-p msg))))))

(deftest test-note-on-zero-velocity
  (testing "NOTE-ON message with zero-velocity")
  (ok (let* ((chan 6)
	     (key 48)
	     (velocity 0)
	     (msg (midi-fink::note-on chan key velocity)))
	(and (midi-fink::note-off-p msg)
	     (not (midi-fink::note-on-p msg))))))
	     
      
(deftest test-polyphonic-pressure
  (testing "POLYPHONIC-PRESSURE message")
  (ok (let ((msg (midi-fink::polyphonic-pressure 1 2 3)))
	(and (listp msg)
	     (= (length msg) 3)
	     (midi-fink::messagep msg)
	     (midi-fink::channel-message-p msg)
	     (midi-fink::keyed-message-p msg)))))

(deftest test-control-change
  (testing "CONTROL-CHANGE message")
  (ok (let ((msg (midi-fink::control-change 1 2 3)))
	(and (listp msg)
	     (= (length msg) 3)
	     (midi-fink::messagep msg)
	     (midi-fink::channel-message-p msg)
	     (midi-fink::control-change-p msg)))))

(deftest test-pitch-bend
  (testing "PITCH-BEND message")
  (ok (let ((msg (midi-fink::pitch-bend 1 2 3)))
	(and (listp msg)
	     (= (length msg) 3)
	     (midi-fink::messagep msg)
	     (midi-fink::channel-message-p msg)
	     (midi-fink::pitch-bend-p msg)))))

(deftest test-program-change
  (testing "PROGRAM-CHANGE message")
  (ok (let ((msg (midi-fink::program-change 1 2)))
	(and (listp msg)
	     (= (length msg) 2)
	     (midi-fink::messagep msg)
	     (midi-fink::channel-message-p msg)
	     (midi-fink::program-change-p msg)))))

(deftest test-channel-pressure
  (testing "CHANNEL-PRESSURE message")
  (ok (let ((msg (midi-fink::channel-pressure 1 2)))
	(and (listp msg)
	     (= (length msg) 2)
	     (midi-fink::messagep msg)
	     (midi-fink::channel-message-p msg)
	     (midi-fink::channel-pressure-p msg)))))

(deftest test-channel-predicates
  (testing "Channel message predicates")
  (ok (and (not (midi-fink::messagep 'foo))
	   (not (midi-fink::channel-message-p 'foo))
	   (not (midi-fink::keyed-message-p (midi-fink::program-change 1 2))))))


(deftest test-data-count-integer
  (testing "data-count with integer arguments")
  (ok (and (eq 1 (midi-fink::data-count midi-fink::+program-change+ 0))
	   (eq 2 (midi-fink::data-count midi-fink::+note-on+ 0))
	   (eq 1 (midi-fink::data-count 0 midi-fink::+program-change+))
	   (eq 2 (midi-fink::data-count 0 midi-fink::+note-on+)) 
	   (eq 0 (midi-fink::data-count midi-fink::+end-exclusive+ 0))
	   (eq :sysex (midi-fink::data-count midi-fink::+system-exclusive+ 0))
	   (eq :meta (midi-fink::data-count midi-fink::+meta+ 0)))))

(deftest test-midi=
  (testing "MIDI= on message list.")
  (ok (and (midi-fink::midi= (midi-fink::note-on 1 2 3)
			     (midi-fink::note-on 1 2 3))
	   (not (midi-fink::midi= (midi-fink::note-on 1 2 3)
				  (midi-fink::note-on 2 3 4))))))
