;;;; cyco-midi  test-meta-tempo.lisp
;;;;

(in-package :cyco-midi/tests/main)

(deftest test-meta-tempo-1
  (testing "meta-tempo predicates")
  (ok (let ((msg (cyco-midi::meta-tempo 120.0)))
	(and (listp msg)
	     (cyco-midi::metap msg)
	     (cyco-midi::meta-tempo-p msg)
	     (not (cyco-midi::meta-tempo-p 'foo))))))

(deftest test-meta-tempo-2
  (testing "meta-tempo  tempo method")
  (ok (let* ((t1 79.0)
	     (t2 101.0)
	     (msg1 (cyco-midi::meta-tempo t1))
	     (msg2 (cyco-midi::meta-tempo t2)))
	(and (= (round (cyco-midi::tempo msg1)) t1)
	     (= (round (cyco-midi::tempo msg2)) t2)))))
	    

	     
