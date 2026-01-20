(in-package #:antediluvian)

(defconstant +bits-per-byte+ 8
  "The number of bits in one byte.")

(defun minutes-since (time)
  "Returns the time in minutes that has elapsed since TIME."
  (/ (- (get-universal-time)
        time)
     60))

(defun calculate-distance (a b)
  "Returns the distance between A and B."
  (logxor a b))

(defun convert-id-to-int (id)
  "Converts a node ID from an ID byte vector to a decimal integer."
  (octets-to-integer id))

(defun concat-vec (x y)
  "Concatenates X and Y into a byte vector."
  (concatenate '(vector (unsigned-byte 8)) x y))

(defun contains (item vector &key (test #'eql))
  "Returns T if ITEM is present in VECTOR under TEST, NIL otherwise."
  (loop for elt across vector
        when (funcall test item elt)
          return t))

(defun filespecp (object)
  "Determines whether OBJECT is a file specifier or not."
  (or (stringp object) (pathnamep object)))

(defun make-octets (length &rest args)
  "Makes a vector of length LENGTH with an element type of (UNSIGNED-BYTE 8)."
  (apply #'make-array length :element-type '(unsigned-byte 8) args))
#+mezzano
(defun random-data (num-bytes)
  "Generates an (unsigned-byte 8) vector of length NUM-BYTES filled with
random data."
  (loop with result = (make-octets num-bytes)
        with random-state = (make-random-state t)
        for i below num-bytes
        do (setf (aref result i) (random 256 random-state))
        finally (return result)))
;;; this code is adapted from the
;;; do-urlencode library without much change
(defun urlencode-binary-data (byte-array)
  "Percent-encodes BYTE-ARRAY."
  (loop with result = (make-string (* 3 (length byte-array)))
        with i of-type fixnum = 0
        for octet across byte-array
        do (flet ((push-char (char)
                    (setf (aref result i) char)
                    (incf i)))
             (if (do-urlencode::unreserved-octet-p octet)
                 (push-char (do-urlencode::octet-to-ascii octet))
                 (let ((big-byte (digit-char (ash (dpb 0 (byte 4 0) octet) -4) 16))
                       (little-byte (digit-char (dpb 0 (byte 4 4) octet) 16)))
                   (push-char #\%)
                   (push-char big-byte)
                   (push-char little-byte))))
        finally (return (subseq result 0 i))))