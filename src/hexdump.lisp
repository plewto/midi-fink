;;;; midi-fink hexdump.lisp
;;;;

(in-package :midi-fink)


(labels ((format-index (index offset)
		       (sformat "[~6X] : " (+ offset index)))

	 (format-hex (bytes index width)
		     (let ((end (min (+ index width)(length bytes)))
			   (acc ""))
		       (loop for i from index below end do
			     (setf acc (sformat "~A~2X " acc (aref bytes i))))
		       (while (< (length acc)(* 3 width))
			 (setf acc (str+ acc " ")))
		       (str+ acc " : ")))

	 (format-char (bytes index width)
		      (let ((end (min (+ index width)(length bytes)))
			    (acc ""))
			(loop for i from index below end do
			      (setf acc (sformat "~A~A" acc (int->ascii (aref bytes i)))))
			acc)))

	(defmethod hexdump ((this vector) &key (start 0)(end nil)(width 12)(headline nil)(offset 0)(tab 0))
	  "Prints hexdump for array contents
:start - Starting index, defaults to 0
:end   - End index, defaults to length of this.
:width - Number of values per line."
	  (let ((spaces (let ((acc ""))
			  (when (plusp tab)
			    (loop for i from 0 below tab do
				  (setf acc (str+ acc "    "))))
			  acc)))
	    (when headline
	      (format t "~A~A~%" spaces headline))
	    (loop for index from start below (min (or end (length this))(length this)) by width do
		  (format t "~A" spaces)
		  (format t "~A" (format-index index offset))
		  (format t "~A" (format-hex this index width))
		  (format t "~A" (format-char this index width))
		  (format t "~%")))) )
	  

		       
		       
(defmethod hexdump ((this list) &key (start 0)(end nil)(width 12)(headline nil))
  (hexdump (->vector this) :start start :end end :width width :headline headline))
