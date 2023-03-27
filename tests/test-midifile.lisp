;;;; midi-fink  test-midifile.lisp
;;;;

(in-package :midi-fink/tests/main)

(defun temp-filename (filename)
  (let ((os (uiop:detect-os)))
    (cond ((eq os :os-unix)
	   (merge-pathnames #P"/tmp/" (pathname filename)))
	  ((eq os :os-windows)
	   (let ((tmp (format nil "~A\\" (uiop:getenv "temp"))))
	     (merge-pathnames (pathname tmp)(pathname filename))))
	  (t (midi-fink::midi-warning
	      (format nil "Can not create a temp directory for testing MIDIFILE read/write")
	      (format nil "for ~A operating system" os))
	     nil))))


(deftest test-midifile
  (testing "MIDIFILE constructor")
  (ok (let ((mfile (midi-fink::midifile :division 96 :format 1 :tracks 2)))
	(and (midi-fink::midifile-p mfile)
	     (= 2 (midi-fink::track-count mfile))
	     (= 1 (midi-fink::mf-format mfile))))))


(deftest test-midifile-read-write
  (testing "MIDIFILE read/write")
  (ok (let ((filename (temp-filename "midi-fink-test.mid")))
	(if (not filename)
	    (progn 
	      (format t "*** Skiping MIDIFILE-READ-WRITE test~%")
	      t)
	  (let ((mfile-1 (midi-fink::midifile :format 0 :tracks 1)))
	    (loop for ev in (list (midi-fink::event 0.0 (midi-fink::meta-text "Alpha" :track-name))
				  (midi-fink::event 1.0 (midi-fink::meta-text "Start time 1.0" :marker))
				  (midi-fink::event 1.0 (midi-fink::control-change 1 2 3))
				  (midi-fink::event 1.0 (midi-fink::note-off 1 2 3))
				  (midi-fink::event 1.0 (midi-fink::note-on 1 2 3))
				  (midi-fink::event 2.0 (midi-fink::meta-text "Start time 2.0" :marker))
				  (midi-fink::event 2.0 (midi-fink::control-change 4 5 6)))
		  do (midi-fink::push-event! mfile-1 ev))
	    (midi-fink::->midi mfile-1 :pathname filename)
	    (let ((mfile-2 (midi-fink::read-midifile filename)))
	      (midi-fink::midi= mfile-1 mfile-2)))))))
