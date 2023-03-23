;;;; cyco-midi  test-channel
;;;;

(in-package :cyco-midi/tests/main)

(deftest test-note-off
  (testing "NOTE-OFF message")
  (ok (let* ((chan 6)
	     (key 48)
	     (velocity 72)
	     (msg (cyco-midi::note-off chan key velocity)))
	(and (listp msg)
	     (= (length msg) 3)
	     (cyco-midi::messagep msg)
	     (cyco-midi::channel-message-p msg)
	     (cyco-midi::keyed-message-p msg)
	     (cyco-midi::note-off-p msg)
	     (not (cyco-midi::note-on-p msg))))))

(deftest test-note-on
  (testing "NOTE-ON message")
  (ok (let* ((chan 6)
	     (key 48)
	     (velocity 72)
	     (msg (cyco-midi::note-on chan key velocity)))
	(and (listp msg)
	     (= (length msg) 3)
	     (cyco-midi::messagep msg)
	     (cyco-midi::channel-message-p msg)
	     (cyco-midi::keyed-message-p msg)
	     (cyco-midi::note-on-p msg)
	     (not (cyco-midi::note-off-p msg))))))

(deftest test-note-on-zero-velocity
  (testing "NOTE-ON message with zero-velocity")
  (ok (let* ((chan 6)
	     (key 48)
	     (velocity 0)
	     (msg (cyco-midi::note-on chan key velocity)))
	(and (cyco-midi::note-off-p msg)
	     (not (cyco-midi::note-on-p msg))))))
	     
      
(deftest test-polyphonic-pressure
  (testing "POLYPHONIC-PRESSURE message")
  (ok (let ((msg (cyco-midi::polyphonic-pressure 1 2 3)))
	(and (listp msg)
	     (= (length msg) 3)
	     (cyco-midi::messagep msg)
	     (cyco-midi::channel-message-p msg)
	     (cyco-midi::keyed-message-p msg)))))

(deftest test-control-change
  (testing "CONTROL-CHANGE message")
  (ok (let ((msg (cyco-midi::control-change 1 2 3)))
	(and (listp msg)
	     (= (length msg) 3)
	     (cyco-midi::messagep msg)
	     (cyco-midi::channel-message-p msg)
	     (cyco-midi::control-change-p msg)))))

(deftest test-pitch-bend
  (testing "PITCH-BEND message")
  (ok (let ((msg (cyco-midi::pitch-bend 1 2 3)))
	(and (listp msg)
	     (= (length msg) 3)
	     (cyco-midi::messagep msg)
	     (cyco-midi::channel-message-p msg)
	     (cyco-midi::pitch-bend-p msg)))))

(deftest test-program-change
  (testing "PROGRAM-CHANGE message")
  (ok (let ((msg (cyco-midi::program-change 1 2)))
	(and (listp msg)
	     (= (length msg) 2)
	     (cyco-midi::messagep msg)
	     (cyco-midi::channel-message-p msg)
	     (cyco-midi::program-change-p msg)))))

(deftest test-channel-pressure
  (testing "CHANNEL-PRESSURE message")
  (ok (let ((msg (cyco-midi::channel-pressure 1 2)))
	(and (listp msg)
	     (= (length msg) 2)
	     (cyco-midi::messagep msg)
	     (cyco-midi::channel-message-p msg)
	     (cyco-midi::channel-pressure-p msg)))))

(deftest test-channel-predicates
  (testing "Channel message predicates")
  (ok (and (not (cyco-midi::messagep 'foo))
	   (not (cyco-midi::channel-message-p 'foo))
	   (not (cyco-midi::keyed-message-p (cyco-midi::program-change 1 2))))))


(deftest test-data-count-integer
  (testing "data-count with integer arguments")
  (ok (and (eq 1 (cyco-midi::data-count cyco-midi::+program-change+ 0))
	   (eq 2 (cyco-midi::data-count cyco-midi::+note-on+ 0))
	   (eq 1 (cyco-midi::data-count 0 cyco-midi::+program-change+))
	   (eq 2 (cyco-midi::data-count 0 cyco-midi::+note-on+)) 
	   (eq 0 (cyco-midi::data-count cyco-midi::+end-exclusive+ 0))
	   (eq :sysex (cyco-midi::data-count cyco-midi::+system-exclusive+ 0))
	   (eq :meta (cyco-midi::data-count cyco-midi::+meta+ 0)))))
