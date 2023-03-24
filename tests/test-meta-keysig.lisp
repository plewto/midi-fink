;;;; midi-fink  test-meta-keysig.lisp
;;;;

(in-package :midi-fink/tests/main)

(deftest test-meta-keysig
  (testing "meta-keysig")
  (ok (let ((msg (midi-fink::meta-keysig 0 nil)))
	(and (listp msg)
	     (midi-fink::metap msg)
	     (midi-fink::meta-keysig-p msg)
	     (not (midi-fink::meta-keysig-p 'foo))
	     (zerop (midi-fink::meta-keysig-accidentals msg))
	     (not (midi-fink::meta-keysig-minor-p msg))))))

(deftest test-meta-keysig-accidentals
  (testing "meta-keysig accidentals and minor flag.")
  (ok (let ((d-major (midi-fink::meta-keysig 2 nil))
	    (b-major (midi-fink::meta-keysig -2 nil))
	    (a-minor (midi-fink::meta-keysig 0 t)))
	(and (not (midi-fink::meta-keysig-minor-p d-major))
	     (not (midi-fink::meta-keysig-minor-p b-major))
	     (midi-fink::meta-keysig-minor-p a-minor)
	     (= (midi-fink::meta-keysig-accidentals d-major) 2)
	     (= (midi-fink::meta-keysig-accidentals b-major) -2)))))

	     
  

