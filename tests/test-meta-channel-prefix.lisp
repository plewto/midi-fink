;;;; midi-fink  test-meta-channel-prefix
;;;;

(in-package :midi-fink/tests/main)

(deftest test-meta-text
  (testing "meta-channel-prefix")
  (ok (let ((msg (midi-fink::meta-channel-prefix 4)))
	(and (listp msg)
	     (midi-fink::metap msg)
	     (midi-fink::meta-channel-prefix-p msg)
	     (not (midi-fink::meta-channel-prefix-p '(foo)))))))
	     
	     
