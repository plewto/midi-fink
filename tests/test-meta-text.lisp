;;;; midi-fink  test-meta-text
;;;;

(in-package :midi-fink/tests/main)

(deftest test-meta-text
  (testing "meta text messages")
  (ok (let ((msg1 (midi-fink::meta-text "Alpha Beta")))
	(and (listp msg1)
	     (midi-fink::metap msg1)
	     (midi-fink::meta-text-p msg1)
	     (not (midi-fink::meta-text-p '(foo)))))))
	     
	     
