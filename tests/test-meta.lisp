;;;; cyco-midi  test-meta
;;;;

(in-package :cyco-midi/tests/main)

(deftest test-meta-text
  (testing "meta text messages")
  (ok (let ((eot (cyco-midi::meta-end-of-track)))
	(and (listp eot)
	     (cyco-midi::metap eot)
	     (not (cyco-midi::metap 'foo))
	     (cyco-midi::meta-end-of-track-p eot)))))
