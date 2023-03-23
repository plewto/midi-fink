;;;; machine-drum utils.lisp
;;;;

(in-package :machine-drum)

(defmacro while (test &rest body)
  "Executes body until test is nil."
  `(do ()
       ((not ,test))
       ,@body))

(defun false (&rest _)
  "Constantly returns nil."
  (declare (ignore _))
  nil)

(defun true (&rest _)
  "Constantly returns t."
  (declare (ignore _))
  t)

(defun sformat (frmt &rest args)
  "Returns formatted string
sformat is a convenience function identical to calling format with first argument nil."
  (apply #'format (append (list nil frmt) args)))

(defun str+ (&rest args)
  "Concatenates strings."
  (let ((acc ""))
    (loop for s in args do
	  (setf acc (sformat "~A~A" acc s)))
    acc))

(defun in-bounds (value floor ceiling)
  "Returns t if value is in closed interval [floor, ceiling]"
  (and (<= floor value)
       (<= value ceiling)))

(defmethod every-in-bounds ((seq list)(floor number)(ceiling number))
  (every #'(lambda (q)(and (numberp q)(<= floor q)(<= q ceiling))) seq))

(defmethod every-in-bounds ((seq vector)(floor number)(ceiling number))
  (every-in-bounds (coerce seq 'list) floor ceiling))

(defmethod ->vector ((this t))
  #(this))

(defmethod ->vector ((this vector)) this)

(defmethod ->vector ((this list))
  (coerce this 'vector))

(defmethod ->list ((this t))
  (list this))

(defmethod ->list ((this list))
  this)

(defmethod ->list ((this vector))
  (coerce this 'list))

