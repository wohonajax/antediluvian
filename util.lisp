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

(defmacro insert (item list predicate &key (key #'identity))
  "Inserts ITEM into LIST and sorts it according to PREDICATE. Duplicates are
not allowed."
  (labels ((insert-item (itm lst)
             (cond ((equalp itm (first lst)) lst)
                   ((funcall predicate
                             (funcall key itm)
                             (funcall key (first lst)))
                    (cons itm lst))
                   (t (cons (first lst)
                            (insert-item itm (rest lst)))))))
    `(setf ,list (insert-item ,item ,list))))

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