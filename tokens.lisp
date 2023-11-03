(in-package #:dhticl)

(defvar *token-births* (make-hash-table :test #'equalp)
  "A hash table mapping tokens to their creation times.")

(defvar *hashmap* (make-hash-table :test #'equalp)
  "A hash table containing info_hashes as keys and (token . nodes) as values.")

(defvar *current-secret*)
(defvar *previous-secret*)

(defun make-hash (byte-vector)
  "Hashes BYTE-VECTOR using the SHA1 algorithm."
  (ironclad:digest-sequence :sha1 byte-vector))

(defun generate-transaction-id ()
  "Creates a transaction ID and returns it as a string."
  (let ((array (make-array 2 :element-type '(unsigned-byte 8))))
    (setf (aref array 0) (random 256)
          (aref array 1) (random 256))
    (octets-to-string array)))

(defun parse-node-ip (ip)
  "Returns a node's IP address and port as multiple values."
  (let ((ip-vector (make-array 4 :element-type '(unsigned-byte 8)))
        (port-vector (make-array 2 :element-type '(unsigned-byte 8))))
    (flet ((parse-char (char)
             (char-code (char ip char))))
      (setf (aref ip-vector 0) (parse-char 0)
            (aref ip-vector 1) (parse-char 1)
            (aref ip-vector 2) (parse-char 2)
            (aref ip-vector 3) (parse-char 3)
            (aref port-vector 0) (parse-char 4)
            (aref port-vector 1) (parse-char 5))
      (values ip-vector (usocket:port-from-octet-buffer port-vector)))))

(defun make-secret ()
  "Makes a secret."
  (let ((vec (make-array 5 :element-type '(unsigned-byte 8))))
    (dotimes (i 5)
      (setf (aref vec i) (random 160)))
    (setf *previous-secret* *current-secret*
          *current-secret* (cons vec (get-universal-time)))
    vec))

(defun ensure-secret ()
  "Makes sure the current secret isn't stale. If it is, returns a fresh secret."
  (if (> (minutes-since (cdr *current-secret*))
         5)
      (make-secret)
      (car *current-secret*)))

(defun consider-token (hash token)
  "Checks whether TOKEN is valid for HASH or not."
  (equalp (cdr (gethash hash *hashmap*)) token)
  #|
  (let* ((node-ip-hash (make-array 20 :element-type '(unsigned-byte 8)))
         (token-value (token-value token))
         (token-hash (subseq token-value 0 20))
         (token-secret (subseq token-value 20)))
    (map-into node-ip-hash #'identity
              (make-hash (parse-node-ip (node-ip node))))
    (ensure-secret)
    (and (equal node-ip-hash token-hash)
         (or (equal token-secret (car *previous-secret*))
             (equal token-secret (car *current-secret*))))) |#
  )

(defun invent-token (info-hash node)
  "Creates a token associated with INFO-HASH and NODE."
  (let* ((ip-vec (parse-node-ip (node-ip node)))
         (token (concatenate '(vector (unsigned-byte 8))
                             (make-hash ip-vec)
                             (ensure-secret))))
    (unless (node-p (first (gethash info-hash *hashmap*)))
      (pop (gethash info-hash *hashmap*)))
    (push token (gethash info-hash *hashmap*))
    (setf (gethash token *token-births*) (get-universal-time))))

(defun valid-token-p (token)
  "Determines whether TOKEN is valid or not."
  (let ((token-birth (gethash token *token-births*)))
    (when token-birth
      (< (minutes-since token-birth) 10))))

(defun recall-token (hash)
  "Retrieves the token value associated with HASH. If a recent enough token
isn't found, returns NIL."
  (let ((token (cdr (gethash hash *hashmap*))))
    (and token (valid-token-p token) token)))

(defun refresh-tokens ()
  "Deletes every token more than 10 minutes old."
  (maphash (lambda (key value)
             (declare (ignore key))
             (unless (valid-token-p (cdr value))
               (setf (cdr value) nil)))
           *hashmap*)
  (maphash (lambda (key value)
             (declare (ignore value))
             (unless (valid-token-p key)
               (remhash key *token-births*)))
           *token-births*))
