;;;; midi-fink  test-meta
;;;;

(in-package :midi-fink/tests/main)

(deftest test-meta-text
  (testing "meta text messages")
  (ok (let ((eot (midi-fink::meta-end-of-track)))
	(and (listp eot)
	     (midi-fink::metap eot)
	     (not (midi-fink::metap 'foo))
	     (midi-fink::meta-end-of-track-p eot)))))
