;;;; midi-fink  test-event.lisp
;;;;

(in-package :midi-fink/tests/main)

(deftest test-event
  (testing "Event basics")
  (ok (let ((ev1 (midi-fink::event 1.0 (midi-fink::note-on 1 2 3))))
	(and (midi-fink::eventp ev1)
	     (not (midi-fink::eventp 'foo))
	     (= (midi-fink::event-time ev1) 1.0)
	     (midi-fink::note-on-p ev1)
	     (midi-fink::note-on-p (midi-fink::event-message ev1))))))
	     
(deftest test-event-ordering
  (testing "Event ordering")
  (ok (let ((tempo (midi-fink::event 1.0 (midi-fink::meta-tempo 123)))
	    (ev1.0 (midi-fink::event 1.0 (midi-fink::note-on 1 2 3)))
	    (ev2.0 (midi-fink::event 2.0 (midi-fink::note-on 1 2 3)))
	    (ev2.1 (midi-fink::event 2.0 (midi-fink::note-off 1 2 3))))
	(and (midi-fink::midi< tempo ev1.0)
	     (midi-fink::midi< ev1.0 ev2.0)
	     (midi-fink::midi< ev2.0 ev2.1)))))


(deftest test-event-clone
  (testing "Event clone method")
  (ok (let* ((msg1 (midi-fink::note-on 1 2 3))
	     (t1 12.0)
	     (ev1 (midi-fink::event t1 msg1))
	     (ev2 (midi-fink::midi-clone ev1))
	     (msg2 (midi-fink::event-message ev2)))
	(and (not (eq ev1 ev2))
	     (midi-fink::eventp ev2)
	     (= (midi-fink::event-time ev2) t1)
	     (not (eq msg1 msg2))
	     (equal msg1 msg2)))))
