;;;; cyco-midi  test-meta-channel-prefix
;;;;

(in-package :cyco-midi/tests/main)

(deftest test-meta-text
  (testing "meta-channel-prefix")
  (ok (let ((msg (cyco-midi::meta-channel-prefix 4)))
	(and (listp msg)
	     (cyco-midi::metap msg)
	     (cyco-midi::meta-channel-prefix-p msg)
	     (not (cyco-midi::meta-channel-prefix-p '(foo)))))))
	     
	     
