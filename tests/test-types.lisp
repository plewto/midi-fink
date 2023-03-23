;;;; cyco-midi test-types

(in-package :cyco-midi/tests/main)

(defmacro while (test &rest body)
  "Executes body until test is nil."
  `(do ()
       ((not ,test))
       ,@body))

(deftest test-int->long
  (testing "Conversion of integer to 4-byte array 'long'")
  (ok (and (equal '(0 0 0 0) (cyco-midi::int->long 0))
	   (equal '(0 0 0 #xFF) (cyco-midi::int->long #x000000ff))
	   (equal '(0 0 #xFF 0) (cyco-midi::int->long #x0000ff00))
	   (equal '(0 #xFF 0 0) (cyco-midi::int->long #x00ff0000))
	   (equal '(#xFF 0 0 0) (cyco-midi::int->long #xff000000))
	   (equal '(#xFF #xFF #xFF #xFF) (cyco-midi::int->long #xffffffff)))))

(deftest test-int->n24
  (testing "Conversion of integer to 3-byte array 'n24'")
  (ok (and (equal '(0 0 0) (cyco-midi::int->n24 0))
	   (equal '(0 0 #xFF) (cyco-midi::int->n24 #x0000ff))
	   (equal '(0 #xFF 0) (cyco-midi::int->n24 #x00ff00))
	   (equal '(#xFF 0 0) (cyco-midi::int->n24 #xff0000))
	   (equal '(#xFF #xFF #xFF)(cyco-midi::int->n24 #xffffff)))))

(deftest test-int->short
  (testing "Conversion of integer to 2-byte array 'short'")
  (ok (and (equal '(0 0) (cyco-midi::int->short 0))
	   (equal '(0 #xFF) (cyco-midi::int->short #x00ff))
	   (equal '(#xFF 0) (cyco-midi::int->short #xff00))
	   (equal '(#xFF 0) (cyco-midi::int->short #xff00))
	   (equal '(#xFF #xFF)(cyco-midi::int->short #xffff)))))

(deftest test-long->int
  (testing "Conversion of 4-byte vector to 'long'")
  (ok (and (= #x00000000 (cyco-midi::long->int #(#x00 #x00 #x00 #x00)))
	   (= #x000000ff (cyco-midi::long->int #(#x00 #x00 #x00 #xFF)))
	   (= #x0000ff00 (cyco-midi::long->int #(#x00 #x00 #xff #x00)))
	   (= #x00ff0000 (cyco-midi::long->int #(#x00 #xff #x00 #x00)))
	   (= #xff000000 (cyco-midi::long->int #(#xff #x00 #x00 #x00)))
	   (= #xffffffff (cyco-midi::long->int #(#xff #xff #xff #xff))))))

(deftest test-n24->int
  (testing "Conversion of 3-byte vector to 'n24'")
  (ok (and (= #x000000 (cyco-midi::n24->int #(#x00 #x00 #x00)))
	   (= #x0000ff (cyco-midi::n24->int #(#x00 #x00 #xFF)))
	   (= #x00ff00 (cyco-midi::n24->int #(#x00 #xff #x00)))
	   (= #xff0000 (cyco-midi::n24->int #(#xff #x00 #x00)))
	   (= #x000000 (cyco-midi::n24->int #(#x00 #x00 #x00)))
	   (= #xffffff (cyco-midi::n24->int #(#xff #xff #xff))))))

(deftest test-short->int
  (testing "Conversion of 3-byte vector to 'short'")
  (ok (and (= #x0000 (cyco-midi::short->int #(#x00 #x00)))
	   (= #x00ff (cyco-midi::short->int #(#x00 #xFF)))
	   (= #xff00 (cyco-midi::short->int #(#xff #x00)))
	   (= #x0000 (cyco-midi::short->int #(#x00 #x00)))
	   (= #x0000 (cyco-midi::short->int #(#x00 #x00)))
	   (= #xffff (cyco-midi::short->int #(#xff #xff))))))

(deftest test-vlv
  (testing "MIDI variable length value conversion.")
  (ok (let ((flag 't)
	    (trials '((#x00000000 (#x00))
		      (#x00000040 (#x40))
		      (#x0000007F (#x7F))
		      (#x00000080 (#x81 #x00))
		      (#x00002000 (#xC0 #x00))
		      (#x00003FFF (#xFF #x7F))
		      (#x00004000 (#x81 #x80 #x00))
		      (#x00100000 (#xC0 #x80 #x00))
		      (#x001FFFFF (#xFF #xFF #x7F))
		      (#x00200000 (#x81 #x80 #x80 #x00))
		      (#x08000000 (#xC0 #x80 #x80 #x00))
		      (#x0FFFFFFF (#xFF #xFF #xFF #x7F)))))
	(loop for trial in trials do
	      (let* ((n (first trial))
		     (vlv (second trial))
		     (n2 (cyco-midi::vlv->int vlv))
		     (vlv2 (cyco-midi::int->vlv n)))
		(setf flag (and flag (= n n2)(equal vlv vlv2)))))
	flag)))

(deftest test-ascii
  (testing "ASCII character conversion.")
  (ok (let ((flag t))
	(loop for n from 32 below 127 do
	      (let* ((c (cyco-midi::int->ascii n #\space))
		     (n2 (cyco-midi::ascii->int c 32)))
		(setf flag (and flag (= n n2)))))
	flag)))


(let ((mock-data #(#x00 #x00 #x00 #x00
		     #x10 #x10 #x10 #x10
		     #xc0 #x80 #x80 #x00)))
		     
  (deftest test-take
    (testing "take-long")
    (ok (and (multiple-value-bind (value index)(cyco-midi::take-long mock-data 0)
				  (and (zerop value)(= index 4)))
	     (multiple-value-bind (value index)(cyco-midi::take-long mock-data 4)
				  (and (= value #x10101010)(= index 8)))
	     (multiple-value-bind (value index)(cyco-midi::take-n24 mock-data 0)
				  (and (zerop value)(= index 3)))
	     (multiple-value-bind (value index)(cyco-midi::take-n24 mock-data 4)
				  (and (= value #x101010)(= index 7)))
	     (multiple-value-bind (value index)(cyco-midi::take-short mock-data 0)
				  (and (zerop value)(= index 2)))
	     (multiple-value-bind (value index)(cyco-midi::take-short mock-data 4)
				  (and (= value #x1010)(= index 6)))
	     (multiple-value-bind (value index)(cyco-midi::take-byte mock-data 0)
				  (and (zerop value)(= index 1)))
	     (multiple-value-bind (value index)(cyco-midi::take-byte mock-data 4)
				  (and (= value #x10)(= index 5))))))

  (deftest test-take-vlv
    (testing "take-vlv")
    (ok (and (multiple-value-bind (value index)(cyco-midi::take-vlv mock-data 0)
				  (and (zerop value)(= index 1)))
	     (multiple-value-bind (value index)(cyco-midi::take-vlv mock-data 8)
				  (and (= value #x08000000)(= index 12)))))))

(let ((mock-chunk  #(#x4D #x4F #x43 #x4B                  ;; ID
			 #x00 #x00 #x00 #x06              ;; byte-count
			 #x00 #x01 #x00 #x02 #x00 #x03))) ;; 6 data bytes

  (deftest test-take-chunk
    (testing "take-chunk")
    (ok (multiple-value-bind (alist next)(cyco-midi::take-chunk mock-chunk 0)
			     (and (= (cdr (assoc :id alist)) #x4D4F434B)
				  (= (cdr (assoc :byte-count alist)) 6)
				  (equalp (cdr (assoc :data alist))
					  #(#x00 #x01 #x00 #x02 #x00 #x03))
				  (= next 14))))))


(deftest test-message-type
  (testing "message-type")
  (ok (and (eq :channel (cyco-midi::message-type cyco-midi::+note-on+ 0))
	   (eq :end-sysex (cyco-midi::message-type cyco-midi::+end-exclusive+ 0))
	   (eq :sysex  (cyco-midi::message-type cyco-midi::+system-exclusive+ 0))
	   (eq :meta (cyco-midi::message-type cyco-midi::+meta+ 0))
	   (eq :running-status (cyco-midi::message-type 0 cyco-midi::+note-on+))
	   (eq :error (cyco-midi::message-type 0 0)))))


(let ((mock-bytes #(0 1 2 3 #x90 5 6 7)))
  
  (deftest test-take-running-status-1
    (testing "take-running-status with single data byte")
    (ok (multiple-value-bind (msg next)
			     (cyco-midi::take-running-status mock-bytes
							     1 cyco-midi::+channel-pressure+)
			     (and (listp msg)
				  (= (length msg) 2)
				  (cyco-midi::channel-pressure-p msg)
				  (= next 2)))))

  (deftest test-take-running-status-2
    (testing "take-running-status with two data bytes")
    (ok (multiple-value-bind (msg next)
			     (cyco-midi::take-running-status mock-bytes
							     1 cyco-midi::+note-on+)
			     (and (listp msg)
				  (= (length msg) 3)
				  (cyco-midi::note-on-p msg)
				  (= next 3)))))

  (deftest test-take-channel-message-not-running-status
    (testing 'test-take-channel-message-not-running-status)
    (ok (multiple-value-bind (message next)
			     (cyco-midi::take-channel-message-not-running-status
			      mock-bytes 4)
			     (and (listp message)
				  (= (length message) 3)
				  (cyco-midi::note-on-p message)
				  (= next 7)
				  )))))

(let ((mock-bytes #(#xF0 1 2 3 #xf7 5)))

  (deftest test-take-system-message-1
    (testing "TEST-TAKE-SYSTEM-MESSAGE-1  single end-of-exclusive message")
    (ok (multiple-value-bind (message next)
			     (cyco-midi::take-system-message mock-bytes 4)
			     (format t "DEBUG message -> ~A~%" message)
			     (format t "DEBUG next    -> ~A~%" next)
			     (and
			      (listp message)
			      (= (length message) 1)
			      (cyco-midi::end-system-exclusive-p message)
			      (= next 5)))))

  
  (deftest test-take-system-message-2
    (testing "TEST-TAKE-SYSTEM-MESSAGE-2  entire system-exclusive message, including EOX")
    (ok (multiple-value-bind (message next)
			     (cyco-midi::take-system-message mock-bytes 0)
			     (and (listp message)
				  (= (length message) 5)
				  (cyco-midi::system-exclusive-p message)
				  (= next 5))))) )

(let ((mock-bytes #(#xFF #x01 #x04 #x74 #x65 #x73 #x74 #x00)))

  (deftest take-meta
    (testing 'test-take-meta)
    (ok (multiple-value-bind (message next)
			     (cyco-midi::take-meta-message mock-bytes 0)
			     (declare (ignore message next))
			     (and
			      (listp message)
			      (= (length message) 7)
			      (cyco-midi::meta-text-p message)
			      (= next 7))))))

(let ((mock-bytes #(#x80 1 2                                    ;;  0 note-on no running status
			 3 4                                    ;;  3 note-on running-status
			 #xf0 6 7 8 #xf7                        ;;  5 sysex
			 #xff #x01 #x04 #x74 #x65 #x73 #x74)))  ;; 10 meta - text 
  (deftest take-message
    (testing "take-message")
    (ok (let ((fail nil)
	      (running-status 0)
	      (index 0)
	      (counter 0)
	      (expected-length '(3 3 5 7)))
	  (while (and (not fail)(< index (length mock-bytes)))
	    (multiple-value-bind (msg next)
				 (cyco-midi::take-message mock-bytes index running-status)
				 (setf index next)
				 (setf running-status
				       (or (and (cyco-midi::channel-message-p msg)(car msg))
					   0))
				 (setf fail (not (= (length msg)
						    (nth counter expected-length))))
				 (setf counter (1+ counter))))
	  (not fail)))))
			       
	
