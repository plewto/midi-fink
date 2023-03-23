;;;; cyco-midi  test-meta-sequence-number
;;;;

(in-package :cyco-midi/tests/main)

(deftest test-meta-text
  (testing "meta-sequence-number")
  (ok (let ((msg (cyco-midi::meta-sequence-number 4)))
	(and (listp msg)
	     (cyco-midi::metap msg)
	     (cyco-midi::meta-sequence-number-p msg)
	     (not (cyco-midi::meta-sequence-number-p '(foo)))))))
	     
	     
