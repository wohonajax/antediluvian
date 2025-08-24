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

(defun convert-id-to-int (id)
  "Converts a node ID from an ID byte-vector to a decimal integer."
  (octets-to-integer id))

(defmacro with-listening-usocket (socket-var &body body)
  "Creates a listening UDP socket and binds it to SOCKET-VAR."
  `(with-connected-socket
       (,socket-var (socket-connect nil nil
                                    :protocol :datagram
                                    :local-port *default-port*))
     ,@body))

(defmacro insert (item list predicate &key key)
  "Inserts ITEM into LIST and sorts it according to PREDICATE."
  `(setf ,list (sort (cons ,item ,list) ,predicate :key ,key)))