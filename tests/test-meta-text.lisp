;;;; cyco-midi  test-meta-text
;;;;

(in-package :cyco-midi/tests/main)

(deftest test-meta-text
  (testing "meta text messages")
  (ok (let ((msg1 (cyco-midi::meta-text "Alpha Beta")))
	(and (listp msg1)
	     (cyco-midi::metap msg1)
	     (cyco-midi::meta-text-p msg1)
	     (not (cyco-midi::meta-text-p '(foo)))))))
	     
	     
