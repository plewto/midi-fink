;;;; midi-fink  test-meta-sequence-number
;;;;

(in-package :midi-fink/tests/main)

(deftest test-meta-text
  (testing "meta-sequence-number")
  (ok (let ((msg (midi-fink::meta-sequence-number 4)))
	(and (listp msg)
	     (midi-fink::metap msg)
	     (midi-fink::meta-sequence-number-p msg)
	     (not (midi-fink::meta-sequence-number-p '(foo)))))))
	     
	     
