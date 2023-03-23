;;;; cyco-midi  test-precendence.lisp
;;;;

(in-package :cyco-midi/tests/main)

(deftest test-precendence
  (testing "Message precendence")
  (ok (let* ((msg1 (cyco-midi::meta-tempo 123.0))
	     (msg2 (cyco-midi::control-change 1 2 3))
	     (msg3 (cyco-midi::note-on 1 2 3))
	     (msg4 (cyco-midi::note-off 1 2 3))
	     (p1 (cyco-midi::precedence msg1))
	     (p2 (cyco-midi::precedence msg2))
	     (p3 (cyco-midi::precedence msg3))
	     (p4 (cyco-midi::precedence msg4)))
	(< p1 p2 p3 p4))))
	     
