;;;; cyco-midi  test-meta-timesig.lisp
;;;;

(in-package :cyco-midi/tests/main)

(deftest test-meta-timesig
  (testing "meta-timesig")
  (ok (let ((msg (cyco-midi::meta-timesig 5 :unit :e)))
	(and (listp msg)
	     (cyco-midi::metap msg)
	     (cyco-midi::meta-timesig-p msg)
	     (not (cyco-midi::meta-timesig-p 'foo))
	     (= (cyco-midi::meta-timesig-beat msg) 5)
	     (eq (cyco-midi::meta-timesig-unit msg) :e)))))
