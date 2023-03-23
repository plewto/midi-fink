;;;; cyco-midi  test-meta-smpte.lisp
;;;;

(in-package :cyco-midi/tests/main)

(deftest test-meta-smpte
  (testing "meta-smpte")
  (ok (let ((msg (cyco-midi::meta-smpte)))
	(and (listp msg)
	     (cyco-midi::metap msg)
	     (cyco-midi::meta-smpte-p msg)
	     (not (cyco-midi::meta-smpte-p 'foo))))))

