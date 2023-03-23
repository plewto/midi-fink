;;;; cyco-midi  test-meta-sequencer.lisp
;;;;

(in-package :cyco-midi/tests/main)

(deftest test-meta-sequencer
  (testing "meta-sequencer")
  (ok (let ((msg (cyco-midi::meta-sequencer '(1 2 3 4))))
	(and (listp msg)
	     (cyco-midi::metap msg)
	     (cyco-midi::meta-sequencer-p msg)
	     (not (cyco-midi::meta-sequencer-p 'foo))))))
