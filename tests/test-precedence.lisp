;;;; midi-fink  test-precendence.lisp
;;;;

(in-package :midi-fink/tests/main)

(deftest test-precendence
  (testing "Message precendence")
  (ok (let* ((msg1 (midi-fink::meta-tempo 123.0))
	     (msg2 (midi-fink::control-change 1 2 3))
	     (msg3 (midi-fink::note-on 1 2 3))
	     (msg4 (midi-fink::note-off 1 2 3))
	     (p1 (midi-fink::precedence msg1))
	     (p2 (midi-fink::precedence msg2))
	     (p3 (midi-fink::precedence msg3))
	     (p4 (midi-fink::precedence msg4)))
	(< p1 p2 p3 p4))))
	     
