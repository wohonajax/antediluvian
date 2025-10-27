(in-package #:antediluvian)

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

(defun insert (item list predicate &key (key #'identity))
  "Inserts ITEM into LIST and sorts it according to PREDICATE. Duplicates are
not allowed."
  (cond ((equalp item (first list)) list)
        ((funcall predicate
                  (funcall key item)
                  (funcall key (first list)))
         (cons item list))
        (t (cons (first list)
                 (insert item (rest list) predicate :key key)))))

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

(defun byte-array-to-ascii-string (byte-array)
  "Converts BYTE-ARRAY to an ASCII string."
  (map 'string #'code-char byte-array))
#+mezzano
(defun random-data (num-bytes)
  "Generates an (unsigned-byte 8) vector of length NUM-BYTES filled with
random data."
  (loop with result = (make-octets num-bytes)
        with random-state = (make-random-state t)
        for i below num-bytes
        do (setf (aref result i) (random 256 random-state))
        finally (return result)))

(defun read-octets (byte-vector stream)
  "Reads bytes from STREAM into BYTE-VECTOR until it's full."
  (loop with total-length = (length byte-vector)
        with bytes-read = 0
        until (= bytes-read total-length)
        do (setf bytes-read (read-sequence result stream :start bytes-read))))