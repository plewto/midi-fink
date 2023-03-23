;;;; machine-drum  clone.lisp
;;;;

(in-package :machine-drum)

(defmethod midi-clone ((this t) &key &allow-other-keys) this)

(defmethod midi-clone ((this list) &key &allow-other-keys)
  (mapcar #'midi-clone this))

