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
  (declare (type fixnum time))
  (/ (the fixnum
          (- (the fixnum (get-universal-time))
             time))
     60))

(declaim (inline calculate-distance))
(defun calculate-distance (a b)
  "Returns the distance between A and B."
  (logxor a b))

(defun octets-to-string (byte-vector)
  "Coerces BYTE-VECTOR to an ASCII string."
  (map 'string #'code-char byte-vector))

(defun convert-id-to-int (id)
  "Converts a node ID from an ID string to a decimal integer."
  (octets-to-integer (ascii-string-to-byte-array id)))

(defmacro with-listening-usocket (socket-var &body body)
  "Creates a listening UDP socket and binds it to SOCKET-VAR."
  `(usocket:with-connected-socket
       (,socket-var (usocket:socket-connect nil nil
                                            :protocol :datagram
                                            :local-port *default-port*))
     ,@body))

(defmacro insert (item list predicate &key key)
  "Inserts ITEM into the correct position in LIST according to PREDICATE."
  `(setf ,list (sort (cons ,item ,list) ,predicate :key ,key)))