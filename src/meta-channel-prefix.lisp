;;;; machine-drum meta-channel-prefix.p
;;;;

(in-package :machine-drum)

(defmethod meta-channel-prefix-p ((this t)) nil)

(defmethod meta-channel-prefix-p ((this integer))
  (= this +channel-prefix+))

(defmethod meta-channel-prefix-p ((this list))
  (and (metap this)
       (meta-channel-prefix-p (second this))))

;; NOTE channel is actual MIDI channel 1..16
;;
(defun meta-channel-prefix (channel)
  (list +meta+ +channel-prefix+ 1 (logand #x0f (1- channel))))
					
