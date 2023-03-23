;;;; cyco-midi  test-meta-keysig.lisp
;;;;

(in-package :cyco-midi/tests/main)

(deftest test-meta-keysig
  (testing "meta-keysig")
  (ok (let ((msg (cyco-midi::meta-keysig 0 nil)))
	(and (listp msg)
	     (cyco-midi::metap msg)
	     (cyco-midi::meta-keysig-p msg)
	     (not (cyco-midi::meta-keysig-p 'foo))
	     (zerop (cyco-midi::meta-keysig-accidentals msg))
	     (not (cyco-midi::meta-keysig-minor-p msg))))))

(deftest test-meta-keysig-accidentals
  (testing "meta-keysig accidentals and minor flag.")
  (ok (let ((d-major (cyco-midi::meta-keysig 2 nil))
	    (b-major (cyco-midi::meta-keysig -2 nil))
	    (a-minor (cyco-midi::meta-keysig 0 t)))
	(and (not (cyco-midi::meta-keysig-minor-p d-major))
	     (not (cyco-midi::meta-keysig-minor-p b-major))
	     (cyco-midi::meta-keysig-minor-p a-minor)
	     (= (cyco-midi::meta-keysig-accidentals d-major) 2)
	     (= (cyco-midi::meta-keysig-accidentals b-major) -2)))))

	     
  

