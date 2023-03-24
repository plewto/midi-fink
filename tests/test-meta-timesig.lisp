;;;; midi-fink  test-meta-timesig.lisp
;;;;

(in-package :midi-fink/tests/main)

(deftest test-meta-timesig
  (testing "meta-timesig")
  (ok (let ((msg (midi-fink::meta-timesig 5 :unit :e)))
	(and (listp msg)
	     (midi-fink::metap msg)
	     (midi-fink::meta-timesig-p msg)
	     (not (midi-fink::meta-timesig-p 'foo))
	     (= (midi-fink::meta-timesig-beat msg) 5)
	     (eq (midi-fink::meta-timesig-unit msg) :e)))))
