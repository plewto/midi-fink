;;;; cyco-midi  test-system
;;;;

(in-package :cyco-midi/tests/main)

(deftest test-system-exclusive
  (testing "SYSTEM-EXCLUSIVE messages")
  (ok (let* ((data '(1 2 3 4))
	     (msg (cyco-midi::system-exclusive data)))
	(and (listp msg)
	     (= (length msg) (+ (length data) 2))
	     (cyco-midi::messagep msg)
	     (not (cyco-midi::channel-message-p msg))
	     (cyco-midi::system-exclusive-p msg)
	     (= (car msg) cyco-midi::+system-exclusive+)
	     (= (car (reverse msg)) cyco-midi::+end-exclusive+)))))

(deftest test-system-predicates
  (testing "System message predicates")
  (ok (and (cyco-midi::system-exclusive-p cyco-midi::+system-exclusive+)
	   (not (cyco-midi::system-exclusive-p 0))
	   (cyco-midi::end-system-exclusive-p cyco-midi::+end-exclusive+)
	   (not (cyco-midi::end-system-exclusive-p 0)))))
	    
