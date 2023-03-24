;;;; midi-fink  test-meta-sequencer.lisp
;;;;

(in-package :midi-fink/tests/main)

(deftest test-meta-sequencer
  (testing "meta-sequencer")
  (ok (let ((msg (midi-fink::meta-sequencer '(1 2 3 4))))
	(and (listp msg)
	     (midi-fink::metap msg)
	     (midi-fink::meta-sequencer-p msg)
	     (not (midi-fink::meta-sequencer-p 'foo))))))
