(in-package #:dhticl)

(defun within (number first-bound end-bound)
  "Tests whether NUMBER is contained within the range bounded by FIRST-BOUND
  and END-BOUND."
  (= number
     (clamp number
            first-bound
            end-bound)))

(defun minutes-since (time)
  "Returns the time in minutes that has elapsed since TIME."
  (/ (- (get-universal-time)
        time)
     60))

(defun calculate-distance (a b)
  "Returns the distance between A and B."
  (logxor a b))

(defun convert-id-to-int (id)
  "Converts a node ID from an ID byte-vector to a decimal integer."
  (octets-to-integer id))

(defun make-hash (byte-vector)
  "Returns the hash of BYTE-VECTOR using the SHA1 algorithm."
  (digest-sequence :sha1 byte-vector))

(defmacro insert (item list predicate &key key)
  "Inserts ITEM into LIST and sorts it according to PREDICATE."
  `(setf ,list (sort (cons ,item ,list) ,predicate :key ,key)))

(defun concat-vec (x y)
  "Concatenates X and Y into a byte-vector."
  (concatenate '(vector (unsigned-byte 8)) x y))