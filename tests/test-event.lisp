;;;; cyco-midi  test-event.lisp
;;;;

(in-package :cyco-midi/tests/main)

(deftest test-event
  (testing "Event basics")
  (ok (let ((ev1 (cyco-midi::event 1.0 (cyco-midi::note-on 1 2 3))))
	(and (cyco-midi::eventp ev1)
	     (not (cyco-midi::eventp 'foo))
	     (= (cyco-midi::event-time ev1) 1.0)
	     (cyco-midi::note-on-p ev1)
	     (cyco-midi::note-on-p (cyco-midi::event-message ev1))))))
	     
(deftest test-event-ordering
  (testing "Event ordering")
  (ok (let ((tempo (cyco-midi::event 1.0 (cyco-midi::meta-tempo 123)))
	    (ev1.0 (cyco-midi::event 1.0 (cyco-midi::note-on 1 2 3)))
	    (ev2.0 (cyco-midi::event 2.0 (cyco-midi::note-on 1 2 3)))
	    (ev2.1 (cyco-midi::event 2.0 (cyco-midi::note-off 1 2 3))))
	(and (cyco-midi::midi< tempo ev1.0)
	     (cyco-midi::midi< ev1.0 ev2.0)
	     (cyco-midi::midi< ev2.0 ev2.1)))))


(deftest test-event-clone
  (testing "Event clone method")
  (ok (let* ((msg1 (cyco-midi::note-on 1 2 3))
	     (t1 12.0)
	     (ev1 (cyco-midi::event t1 msg1))
	     (ev2 (cyco-midi::midi-clone ev1))
	     (msg2 (cyco-midi::event-message ev2)))
	(and (not (eq ev1 ev2))
	     (cyco-midi::eventp ev2)
	     (= (cyco-midi::event-time ev2) t1)
	     (not (eq msg1 msg2))
	     (equal msg1 msg2)))))
