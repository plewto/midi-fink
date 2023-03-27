
;;;; midi-fink  test-eventlist.lisp
;;;;

(in-package :midi-fink/tests/main)

(let* ((ev1 (midi-fink::event 0.0 (midi-fink::meta-text "Alpha" :track-name)))
       (ev2 (midi-fink::event 1.0 (midi-fink::meta-text "Start time 1.0" :marker)))
       (ev3 (midi-fink::event 1.0 (midi-fink::control-change 1 2 3)))
       (ev4 (midi-fink::event 1.0 (midi-fink::note-off 1 2 3)))
       (ev5 (midi-fink::event 1.0 (midi-fink::note-on 1 2 3)))
       (ev6 (midi-fink::event 2.0 (midi-fink::meta-text "Start time 2.0" :marker)))
       (ev7 (midi-fink::event 2.0 (midi-fink::control-change 4 5 6)))
       (events (list ev7 ev5 ev3 ev1 ev6 ev4 ev2))
       (events-2 (midi-fink::midi-clone events))
       (events-3 (list ev7 ev5 ev3 ev1 ev6 ev4))
       (eventlist (midi-fink::eventlist events)))
  
  (deftest test-eventlist
    (testing "Eventlist basics")
    (ok (and (midi-fink::eventlist-p eventlist)
	     (not (midi-fink::eventlist-p 'foo))
	     (= (midi-fink::event-count eventlist)(length events))
	     (= (midi-fink::events-end-time eventlist)(midi-fink::event-time ev7)))))

  (deftest test-find-events
    (testing "find-events")
    (ok (let ((found (midi-fink::find-events eventlist :test #'(lambda (q)
								 (= 1.0 (midi-fink::event-time q))))))
	  (= 4 (length found)))))

  
  (deftest test-eventlist-eot
    (testing "Eventlist ensure-end-of-track!")
    (ok (progn
	  (midi-fink::ensure-end-of-track! eventlist 2.0)
	  (and
	   (= (midi-fink::event-count eventlist)(1+ (length events)))
	   (= (midi-fink::events-end-time eventlist) 4.0)))))

  (deftest test-ensure-tempo-event
    (testing "ensure-tempo-event!  with no existing tempo event.")
    (ok (let ((test-list (midi-fink::midi-clone eventlist))
	      (found nil))
	  (midi-fink::ensure-tempo-event! test-list)
	  (midi-fink::midi-sort! test-list)
	  (setf found (midi-fink::find-events test-list :test #'midi-fink::meta-tempo-p))
	  (and (= 1 (length found))
	       (midi-fink::meta-tempo-p (car found))
	       (zerop (midi-fink::event-time (car found)))))))

  (deftest test-ensure-tempo-event-2
    (testing "ensure-tempo-event!  with existing non-zero time tempo")
    (ok (let ((test-list (midi-fink::midi-clone eventlist))
	      (tempo-event (midi-fink::event 1.5 (midi-fink::meta-tempo 123.0)))
	      (found nil))
	  (midi-fink::push-event! test-list tempo-event)
	  (midi-fink::ensure-tempo-event! test-list)
	  (setf found (car (midi-fink::find-events test-list
						   :test #'(lambda (q)
							     (and (zerop (midi-fink::event-time q))
								  (midi-fink::meta-tempo-p q))))))
	  (and (midi-fink::meta-tempo-p found)
	       (zerop (midi-fink::event-time found))
	       (equal (midi-fink::event-message found)
		      (midi-fink::event-message tempo-event))))))

  (deftest test-ensure-tempo-3
    (testing "ensure-tempo-event! with existiong zero-time tempo")
    (ok (let* ((test-list (midi-fink::push-event! (midi-fink::midi-clone eventlist)
						  (midi-fink::event 0.0 (midi-fink::meta-tempo 123.0))))
	       (init-count (midi-fink::event-count test-list)))
	  (midi-fink::ensure-tempo-event! test-list)
	  (= init-count (midi-fink::event-count test-list)))))

  (deftest test-eventlist-midi=
    (testing "MIDI= on eventlist")
    (ok (let ((a (and (midi-fink::midi= events events-2)))
	      (b (not (midi-fink::midi= events-2 events-3))))
	  (and a b)))) )
