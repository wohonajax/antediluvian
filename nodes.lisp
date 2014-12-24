(in-package #:dhticl-nodes)

;;; distance is bit-wise XOR
(defun node-distance (a b)
  "Returns the distance between nodes A and B."
  (logxor a b))

(defconstant +my-id+ (format nil "~44,'0,':,8:x" (random (expt 2 160))))
