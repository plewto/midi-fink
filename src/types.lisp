;;;; midi-fink types.lisp
;;;;
;;;; Defines several primitive MIDI data types
;;;;
;;;;  long  - 4-byte 32-bit integer
;;;;  n24   - 3-byte 24-bit integer
;;;;  short - 2-byte 16-bit integer
;;;;  byte  - 1-byte  8-bit integer
;;;;  vlv   - Variable Length Value
;;;;
;;;; For each type the following three functions are defined
;;;;
;;;;   (int->type  value)          converts integer value to list of bytes
;;;;   (type->int  bytes)          converts list of bytes to integer
;;;;   (take-type  vector index)   Extracts value from vector starting at index
;;;;                               Returns two-values
;;;;                                 1.  The extract value.
;;;;                                 2.  The index immediately after the extracted data
;;;;
;;;; Also defines functions for conversion between integers and ASCII, and
;;;; for extraction MIDI file chunks from vectors.

(in-package :midi-fink)

(labels ((int->bytes (value count)
		     (loop for i from 0 below count
			   for shift from (* -8 (1- count)) by 8 collect
			   (logand #xFF (ash value shift))))

	 (bytes->int (fn-name bytes count)
		     (setf bytes (->vector bytes))
		     (when (expect fn-name (length bytes)
				   #'(lambda (q)(= q count))
				   (sformat "Byte list of length ~D" count))
		       (let ((scale 1)
			     (acc 0))
			 (loop for i from (1- count) downto 0 do
			       (setf acc (+ acc (* scale (logand #xFF (aref bytes i)))))
			       (setf scale (ash scale 8)))
			 acc)))

	 (take-n (fn-name vector index n)
		 (let ((end (+ index n)))
		   (when (expect fn-name end
				 #'(lambda (q)(>= (length vector) q))
				 (sformat "Byte vector of least length ~A" end))
		     (let ((bytes (make-array n)))
		       (loop for i from 0 below n
			     for j from index below end do
			     (setf (aref bytes i)(aref vector j)))
		       (values (bytes->int fn-name bytes n) end))))) )
	 
	(defun int->long (value)
	  "Converts integer to 4-byte list."
	  (int->bytes value 4))

	(defun int->n24 (value)
	  "Converts integer to 3-byte list."
	  (int->bytes value 3))

	(defun int->short (value)
	  "Converts integer to 2-byte list."
	  (int->bytes value 2))

	(defun int->byte (value)
	  "Converts integer to single-byte list"
	  (list (logand #xFF value)))
	
	(defun long->int (bytes)
	  "Converts first 4 bytes from list or vector to an integer."
	  (bytes->int 'long->int bytes 4))

	(defun n24->int (bytes)
	  "Converts first 3 bytes from list or vector to an integer."
	  (bytes->int 'n24->int bytes 3))

	(defun short->int (bytes)
	  "Converts first 2 bytes from list or vector to an integer."
	  (bytes->int 'short->int bytes 2))

	(defun byte->int (bytes)
	  "Converts first bytes from list or vector to byte."
	  (logand #xFF (cond ((and bytes (listp bytes))
			      (car bytes))
			     ((and (or (vectorp bytes)(arrayp bytes))
				   (plusp (length bytes)))
			      (aref bytes 0))
			     (t 0)))) 

	(defun take-long (vector index)
	  "Extracts 4-byte 'long' form vector starting at index.
Returns 1. The value, 2. index+4"
	  (take-n 'take-long vector index 4))

	(defun take-n24 (vector index)
	  "Extracts 3-byte 'n24' form vector starting at index.
Returns 1. The value, 2. index+3"
	  (take-n 'take->n24 vector index 3))

	(defun take-short (vector index)
	  "Extracts 2-byte 'short' form vector starting at index.
Returns 1. The value, 2. index+2"
	  (take-n 'take-short vector index 2))

	(defun take-byte (vector index)
	  "Extracts byte from vector at index.
Returns 1. The value, 2 index+1"
	  (take-n 'take-byte vector index 1)) )
	
(defun int->vlv (value)
  "Converts int to MIDI variable-length value.
Returns list."
  (let ((bytes (list (logand value #x7f)))
	(n (ash value -7)))
    (while (plusp n)
      (push (+ #x80 (logand n #x7f)) bytes)
      (setf n (ash n -7)))
    bytes))

(defun vlv->int (mbv)
  "Converts list holding MIDI variable-length value to int."
  (let ((result 0)
	(scale 1))
    (dolist (byte (reverse mbv))
      (setf result (+ result (* scale (logand byte #x7f))))
      (setf scale (* scale 128)))
    result))

(defun take-vlv (vector index)
  "Extracts MIDI variable-length-value from vector starting at index.
Returns two values:
   1.  The value
   2.  Index just after final byte of the VLV."
  (let* ((b (aref vector index))
	 (acc (list b)))
    (setf index (1+ index))
    (while (plusp (logand b #x80))
      (setf b (aref vector index))
      (push b acc)
      (setf index (1+ index)))
    (values (vlv->int (reverse acc)) index)))


(let ((atab (str+ " !" #\"
		  "#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_"
		  "`abcdefghijklmnopqrstuvwxyz{|}~")))

  (defun int->ascii (n &optional (default #\.))
    "Converts int to ASCII character.
default is returned for unprintable characters."
    (cond ((< n #x20) default)
	  ((> n #x7e) default)
	  (t (char atab (- n #x20)))))

  (defun ascii->int (c &optional (default 32))
    (let ((n (char-int c)))
      (if (and (>= n 32)(< n 128))
	  n
	default))))

(defun int->ascii-string (n &optional (default #\.))
  (let ((acc ""))
    (while (plusp n)
      (let ((tail (logand #xFF n)))
	(setf acc (str+ acc (int->ascii tail default)))
	(setf n (ash n -8))))
    (reverse acc)))

(defun take-chunk (bytes index)
  "Extracts a MIDI file chunk from bytes starting at index,
Returns two values
1) An association list containing info about the chunk
   :type       :chunk
   :start      the starting index of the chunk
   :id         the 4-byte chunk id
   :byte-count The number of data bytes
   :data       vector of the data bytes
2) index immediately after the extracted data."
  (when (expect 'take-chunk (length bytes) #'(lambda (q)(>= q 8))
		"Chunk byte array must be at least 8-bytes in length.")
    (let* ((id (take-long bytes index))
	   (sid (int->ascii-string id))
	   (byte-count (take-long bytes (+ index 4)))
	   (start (+ index 8))
	   (end (+ start byte-count))
	   (remaining (- (length bytes) start)))
      (when (expect 'take-chunk remaining #'(lambda (q)(>= q byte-count))
		    (sformat "At least ~A remaining bytes" byte-count)
		    (sformat "chunk id is x~X ~S" id sid)
		    (sformat "index is x~X" index)
		    (sformat "byte-count is x~X" byte-count)
		    (sformat "Remaining bytes are: x~X" remaining))
	(let ((acc '()))
	  (loop for i from start below end do
		(push (aref bytes i) acc))
	  (values (list
		   (cons :type :chunk)
		   (cons :start index)
		   (cons :id id)
		   (cons :byte-count byte-count)
		   (cons :data (->vector (reverse acc))))
		  end))))))

(defun message-type (gnomon running-status)
  "Qualify message type, result is one of 
If gnomon is a valid MIDI status byte the result is one of
   :channel :end-sysex :sysex or :meta
If gnomon is not a valid MIDI status the result is either
   :running-status  if running-status is a channel-message status
   :error  if running status is not a valid channel-message status."
  (cond ((channel-message-p gnomon) :channel)
	((end-system-exclusive-p gnomon) :end-sysex)
	((system-exclusive-p gnomon) :sysex)
	((metap gnomon) :meta)
	((channel-message-p running-status) :running-status)
	(t :error)))

(defun take-running-status (bytes index running-status)
  "Extracts channel-message from bytes starting at index using running-status
Returns two values:
1) The channel-message.
2) index immediately after extracted message."
  (let* ((acc (list running-status))
	 (dcount (if (member (logand #xF0 running-status)
			     (list +channel-pressure+ +program-change+))
		     1
		   2))
	 (end (+ index dcount)))
    (loop for i from index below end do
	  (push (aref bytes i) acc))
    (values (reverse acc) end)))

(defun take-channel-message-not-running-status (bytes index)
  "Extracts channel-message from bytes starting at index.
Returns 2 values:
1) The channel-message
2) Index immediately after the extracted message."
  (let* ((status (aref bytes index))
	 (byte-count (if (member (logand #xF0 status)
			     (list +channel-pressure+ +program-change+))
			 2
		       3))
	 (end (+ index byte-count)))
    (values 
     (loop for i from index below end collect (aref bytes i))
     end)))
	 
(defun take-system-message (bytes index)
  "Extracts system-message from bytes starting at index.
Message type may either be a 'complete' system-exclusive message (including
end-of-exclusive status), or a single end-of-exclusive status-byte.
Returns two values:
1) The system-message
2) Index immediately after the extracted message."
  (let ((status (aref bytes index)))
    (if (end-system-exclusive-p status)
	(values (list +end-exclusive+) (1+ index))
      (let ((acc '())
	    (end-not-located t))
	(while (and end-not-located
		    (< index (length bytes)))
	  (let ((byte (aref bytes index)))
	    (push byte acc)
	    (setf index (1+ index))
	    (setf end-not-located (not (end-system-exclusive-p byte)))))
	(values (reverse acc) index)))))
	
(defun take-meta-message (bytes index)
  "Extracts meta-message from bytes starting at index.
Returns two values:
1) The meta-message
2) Index immediately after the extracted message."
  (let* ((status (aref bytes index))
	 (mtype (aref bytes (1+ index)))
	 (vlv-start (+ 2 index))
	 (message-end nil)
	 (message (list status mtype)))
    (multiple-value-bind (dcount dstart)(take-vlv bytes vlv-start)
			 (setf message-end (+ dstart dcount))
			 (setf message (append message
					       (loop for i from vlv-start below message-end
						     collect (aref bytes i))))
			 (values message message-end))))


(defun take-message (bytes index running-status)
  (let* ((gnomon (aref bytes index))
	 (msgtype (message-type gnomon running-status)))
    (cond ((eq msgtype :channel)
	   (take-channel-message-not-running-status bytes index))
	  ((or (eq msgtype :end-sysex)(eq msgtype :sysex))
	   (take-system-message bytes index))
	  ((eq msgtype :meta)
	   (take-meta-message bytes index))
	  ((eq msgtype :running-status)
	   (take-running-status bytes index running-status))
	  (t (midi-error (sformat "Expected start of MIDI message at index: x~X" index)
			 (list (sformat "Status bytes  was : x~X" gnomon)
			       (sformat "Running-Staus was : x~X" running-status)))))))



