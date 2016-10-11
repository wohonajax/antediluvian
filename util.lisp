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

(defun calculate-distance (a b)
  "Returns the distance between A and B."
  (logxor a b))

(defun random-byte (x)
  "MAPpable function for seeding random bytes."
  (declare (ignore x))
  (random 256))

(defun make-string-from-bytes (byte-vector)
  "Returns a string of characters made by using BYTE-VECTOR as an array of
character codes."
  (map 'string #'code-char byte-vector))

(defun make-bytes-from-string (string)
  "Returns a byte vector representation of STRING."
  (map '(vector (unsigned-byte 8)) #'char-code string))

(defun convert-id-to-int (id)
  "Converts a node ID from an ID string to a decimal integer."
  (reduce #'+ (make-bytes-from-string id)))

(defmacro with-listening-usocket-stream ((socket-var stream-var host port) &body body)
  (with-gensyms (socket)
    `(usocket:with-socket-listener (,socket ,host ,port :reuse-address t)
       (usocket:with-connected-socket (,socket-var (usocket:socket-accept ,socket))
         (let ((,stream-var (usocket:socket-stream ,socket-var)))
           ,@body)))))
