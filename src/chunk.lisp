;;;; midi-fink chunk
;;;;
;;;; Parse chunks from midifile bytes

(in-package :midi-fink)

	
(defmethod chunks ((this vector))
  (let ((chunks '())
	(index 0))
    (while (< index (length this))
      (multiple-value-bind (chnk next)(take-chunk this index)
			   (push chnk chunks)
			   (setf index next)))
    (reverse chunks)))

(labels ((long-name (id)
		    (cond ((string= id "MThd") "Header")
			  ((string= id "MTrk") "Track")
			  (t "*** UNKNOWN ***")))
	 
	 (common-overview (number id alist)
			  (format t "Chunk ~D  type: ~A  ~A~%" number id (long-name id))
			  (format t "~Abyte-count     : ~4D~%" +tab-1+ (cdr (assoc :byte-count alist)))
			  (format t "~Astart          : ~4X~%" +tab-1+ (cdr (assoc :byte-count alist))))

	 (header-overview (number id alist)
			  (common-overview number id alist)
			  (let ((data (cdr (assoc :data alist))))
			    (format t "~Aformat         : ~4D~%" +tab-1+ (take-short data 0))
			    (format t "~Atrack count    : ~4D~%" +tab-1+ (take-short data 2))
			    (format t "~Adivision       : ~4D~%" +tab-1+ (take-short data 4))))

	 (track-overview (number id alist)
			 (common-overview number id alist))

	 (unknown-overview (number id alist)
			   (common-overview number id alist))
	 
	 (hex-dump (alist)
		   (let ((start (cdr (assoc :start alist)))
			 (data (cdr (assoc :data alist))))
		   (hexdump data :offset start :tab 1))) )

	(defmethod chnuk-info ((this vector) &key (hex nil))
	  (loop for n from 0
		for chunk in (chunks this) do
		(let ((id (int->ascii-string (cdr (assoc :id chunk)))))
		  (cond ((string= id "MThd")
			 (header-overview n id chunk))
			((string= id "MTrk")
			 (track-overview n id chunk))
			(t (unknown-overview n id chunk)))
		  (when hex (hex-dump (cdr (assoc :data chunk))))))) )
	 


