
;;;; cyco-midi  test-eventlist.lisp
;;;;

(in-package :cyco-midi/tests/main)

(let* ((ev1 (cyco-midi::event 0.0 (cyco-midi::meta-text "Alpha" :track-name)))
       (ev2 (cyco-midi::event 1.0 (cyco-midi::meta-text "Start time 1.0" :marker)))
       (ev3 (cyco-midi::event 1.0 (cyco-midi::control-change 1 2 3)))
       (ev4 (cyco-midi::event 1.0 (cyco-midi::note-off 1 2 3)))
       (ev5 (cyco-midi::event 1.0 (cyco-midi::note-on 1 2 3)))
       (ev6 (cyco-midi::event 2.0 (cyco-midi::meta-text "Start time 2.0" :marker)))
       (ev7 (cyco-midi::event 2.0 (cyco-midi::control-change 4 5 6)))
       (events (list ev7 ev5 ev3 ev1 ev6 ev4 ev2))
       (eventlist (cyco-midi::eventlist events)))
  
  (deftest test-eventlist
    (testing "Eventlist basics")
    (ok (and (cyco-midi::eventlist-p eventlist)
	     (not (cyco-midi::eventlist-p 'foo))
	     (= (cyco-midi::event-count eventlist)(length events))
	     (= (cyco-midi::events-end-time eventlist)(cyco-midi::event-time ev7)))))

  (deftest test-find-events
    (testing "find-events")
    (ok (let ((found (cyco-midi::find-events eventlist :test #'(lambda (q)
								 (= 1.0 (cyco-midi::event-time q))))))
	  (= 4 (length found)))))

  
  (deftest test-eventlist-eot
    (testing "Eventlist ensure-end-of-track!")
    (ok (progn
	  (cyco-midi::ensure-end-of-track! eventlist 2.0)
	  (and
	   (= (cyco-midi::event-count eventlist)(1+ (length events)))
	   (= (cyco-midi::events-end-time eventlist) 4.0)))))

 

  (deftest test-ensure-tempo-event
    (testing "ensure-tempo-event!  with no existing tempo event.")
    (ok (let ((test-list (cyco-midi::midi-clone eventlist))
	      (found nil))
	  (cyco-midi::ensure-tempo-event! test-list)
	  (cyco-midi::midi-sort! test-list)
	  (setf found (cyco-midi::find-events test-list :test #'cyco-midi::meta-tempo-p))
	  (and (= 1 (length found))
	       (cyco-midi::meta-tempo-p (car found))
	       (zerop (cyco-midi::event-time (car found)))))))

  (deftest test-ensure-tempo-event-2
    (testing "ensure-tempo-event!  with existing non-zero time tempo")
    (ok (let ((test-list (cyco-midi::midi-clone eventlist))
	      (tempo-event (cyco-midi::event 1.5 (cyco-midi::meta-tempo 123.0)))
	      (found nil))
	  (cyco-midi::push-event! test-list tempo-event)
	  (cyco-midi::ensure-tempo-event! test-list)
	  (setf found (car (cyco-midi::find-events test-list
						   :test #'(lambda (q)
							     (and (zerop (cyco-midi::event-time q))
								  (cyco-midi::meta-tempo-p q))))))
	  (and (cyco-midi::meta-tempo-p found)
	       (zerop (cyco-midi::event-time found))
	       (equal (cyco-midi::event-message found)
		      (cyco-midi::event-message tempo-event))))))

  (deftest test-ensure-tempo-3
    (testing "ensure-tempo-event! with existiong zero-time tempo")
    (ok (let* ((test-list (cyco-midi::push-event! (cyco-midi::midi-clone eventlist)
						  (cyco-midi::event 0.0 (cyco-midi::meta-tempo 123.0))))
	       (init-count (cyco-midi::event-count test-list)))
	  (cyco-midi::ensure-tempo-event! test-list)
	  (= init-count (cyco-midi::event-count test-list))))) )
