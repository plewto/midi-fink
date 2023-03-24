;;;; midi-fink  test-meta-smpte.lisp
;;;;

(in-package :midi-fink/tests/main)

(deftest test-meta-smpte
  (testing "meta-smpte")
  (ok (let ((msg (midi-fink::meta-smpte)))
	(and (listp msg)
	     (midi-fink::metap msg)
	     (midi-fink::meta-smpte-p msg)
	     (not (midi-fink::meta-smpte-p 'foo))))))

