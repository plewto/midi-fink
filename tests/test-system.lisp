;;;; midi-fink  test-system
;;;;

(in-package :midi-fink/tests/main)

(deftest test-system-exclusive
  (testing "SYSTEM-EXCLUSIVE messages")
  (ok (let* ((data '(1 2 3 4))
	     (msg (midi-fink::system-exclusive data)))
	(and (listp msg)
	     (= (length msg) (+ (length data) 2))
	     (midi-fink::messagep msg)
	     (not (midi-fink::channel-message-p msg))
	     (midi-fink::system-exclusive-p msg)
	     (= (car msg) midi-fink::+system-exclusive+)
	     (= (car (reverse msg)) midi-fink::+end-exclusive+)))))

(deftest test-system-predicates
  (testing "System message predicates")
  (ok (and (midi-fink::system-exclusive-p midi-fink::+system-exclusive+)
	   (not (midi-fink::system-exclusive-p 0))
	   (midi-fink::end-system-exclusive-p midi-fink::+end-exclusive+)
	   (not (midi-fink::end-system-exclusive-p 0)))))
	    
