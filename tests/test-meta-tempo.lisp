;;;; midi-fink  test-meta-tempo.lisp
;;;;

(in-package :midi-fink/tests/main)

(deftest test-meta-tempo-1
  (testing "meta-tempo predicates")
  (ok (let ((msg (midi-fink::meta-tempo 120.0)))
	(and (listp msg)
	     (midi-fink::metap msg)
	     (midi-fink::meta-tempo-p msg)
	     (not (midi-fink::meta-tempo-p 'foo))))))

(deftest test-meta-tempo-2
  (testing "meta-tempo  tempo method")
  (ok (let* ((t1 79.0)
	     (t2 101.0)
	     (msg1 (midi-fink::meta-tempo t1))
	     (msg2 (midi-fink::meta-tempo t2)))
	(and (= (round (midi-fink::tempo msg1)) t1)
	     (= (round (midi-fink::tempo msg2)) t2)))))
	    

	     
