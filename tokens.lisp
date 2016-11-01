(in-package #:dhticl)

(defvar *token-history* (list))
(defvar *current-secret*)
(defvar *previous-secret*)

(defstruct token
  (birth nil :type fixnum :read-only t)
  (value)
  (hash))

(defun make-hash (byte-vector)
  "Hashes BYTE-VECTOR using the SHA1 algorithm."
  (ironclad:digest-sequence :sha1 byte-vector))

(defun ensure-hash (vague-hash)
  "Ensures VAGUE-HASH is really an SHA1 hash. If it is, return it, otherwise
hash it and return the hash."
  (if (equal '(simple-array (unsigned-byte 8) (20))
             (type-of vague-hash))
      vague-hash
      (make-hash vague-hash)))

(defun de-bencode (object)
  "Takes a bencoded OBJECT and decodes it."
  (if (pathnamep object)
      (with-open-file (file object :element-type '(unsigned-byte 8))
        (bencode:decode file))
      (bencode:decode object)))

(defun really-send (data socket)
  "Forces DATA to be sent through SOCKET rather than accumulating in a buffer."
  (let ((stream (usocket:socket-stream socket)))
    (format stream "~A" data)
    (force-output stream)))

(defun generate-transaction-id (&aux (array (make-array 2)))
  "Creates a transaction ID and writes it as a string to STREAM. If STREAM is
NIL (the default), returns the string directly."
  (format nil "~A"
          (make-string-from-bytes (map '(vector (unsigned-byte 8))
                                       #'random-byte array))))

(defun get-transaction-id (query)
  "Retrieves the transaction ID from the Bencoded QUERY."
  (gethash "t" (de-bencode query)))

(defun parse-node-ip (ip &aux (ip-vector (make-array 4 :element-type
                                                     '(unsigned-byte 8)))
                           (port-vector (make-array 2 :element-type
                                                    '(unsigned-byte 8))))
  "Parses a node's IP field into a usable form."
  (flet ((parse-char (char)
           (char-code (char ip char))))
    (setf (aref ip-vector 0) (parse-char 0)
          (aref ip-vector 1) (parse-char 1)
          (aref ip-vector 2) (parse-char 2)
          (aref ip-vector 3) (parse-char 3)
          (aref port-vector 0) (parse-char 4)
          (aref port-vector 1) (parse-char 5))
    (values ip-vector (usocket:port-from-octet-buffer port-vector))))

(defun make-secret (&aux (vec (make-array 5 :element-type '(unsigned-byte 8))))
  "Makes a secret."
  (map-into vec (lambda (x) (declare (ignore x)) (random 160)) #(0 0 0 0 0))
  (setf *previous-secret* *current-secret*
        *current-secret* (cons vec (get-universal-time)))
  vec)

(defun ensure-secret ()
  "Makes sure the current secret isn't stale. If it is, returns a fresh secret."
  (if (> (minutes-since (cdr *current-secret*))
         5)
      (make-secret)
      (car *current-secret*)))

(defun consider-token (node token)
  "Checks whether TOKEN received from NODE is valid or not."
  (let* ((node-ip-hash (make-array 20 :element-type '(unsigned-byte 8)))
         (token-value (token-value token))
         (token-hash (subseq token-value 0 20))
         (token-secret (subseq token-value 20)))
    (map-into node-ip-hash #'identity
              (make-hash (parse-node-ip (node-ip node))))
    (ensure-secret)
    (and (equal node-ip-hash token-hash)
         (or (equal token-secret (car *previous-secret*))
             (equal token-secret (car *current-secret*))))))

(defun invent-token (hash ip)
  "Creates a token associated with HASH using IP."
  (let* ((ip-vec (parse-node-ip ip))
         (token (make-token :birth (get-universal-time)
                            :value (concatenate '(vector (unsigned-byte 8))
                                                (make-hash ip-vec)
                                                (ensure-secret))
                            :hash (ensure-hash hash))))
    (setf *token-history* (cons token *token-history*))))

(defun valid-token-p (token)
  "Determines whether TOKEN is valid or not."
  (when token ; handle NILs
    (< (minutes-since (token-birth token)) 10)))

(defun recall-token (hash)
  "Retrieves the token value associated with HASH. If a recent enough token
isn't found, returns NIL."
  (dolist (x *token-history*)
    (let ((validp (valid-token-p x)))
      (cond ((and validp (equal hash (token-hash x)))
             (return x))
            ((not validp)
             (return))
            (t nil)))))

(defun refresh-tokens ()
  "Deletes every token more than 10 minutes old."
  (labels ((recur (list)
             (if (valid-token-p (second list))
                 (recur (rest list))
                 (setf (rest list) nil))))
    (if (valid-token-p (first *token-history*))
        (recur *token-history*)
        (setf *token-history* nil))))
