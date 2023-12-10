(in-package #:dhticl)

(defvar *token-births* (make-hash-table :test #'equalp)
  "A hash table mapping tokens to their creation times.")

(defvar *token-nodes* (make-hash-table :test #'equalp)
  "A hash table mapping tokens to nodes each token is valid for.")

(defvar *token-hashes* (make-hash-table :test #'equalp)
  "A hash table mapping info_hashes to tokens valid for them.")

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
  (let* ((secret-length 5)
         (vec (make-array secret-length :element-type '(unsigned-byte 8))))
    (dotimes (i secret-length)
      (setf (aref vec i) (random 160)))
    (setf *previous-secret* *current-secret*
          *current-secret* (cons vec (get-universal-time)))
    vec))

(defun ensure-secret ()
  "Makes sure the current secret isn't stale. If it is, makes a fresh secret."
  (if (> (minutes-since (cdr *current-secret*))
         5)
      (make-secret)
      (car *current-secret*)))

(defun consider-token (token info-hash node)
  "Checks whether TOKEN is valid for INFO-HASH and NODE or not."
  (and (member node (gethash token *token-nodes*) :test #'equalp)
       (member info-hash (gethash token *token-hashes*) :test #'equalp))
  #| this old code checked to see if TOKEN came from our INVENT-TOKEN
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
  (let* ((ip-vec (node-ip node))
         (token (concatenate '(vector (unsigned-byte 8))
                             (make-hash ip-vec)
                             (ensure-secret))))
    (setf (gethash token *token-births*) (get-universal-time))
    (pushnew info-hash (gethash token *token-hashes*) :test #'equalp)
    (pushnew node (gethash token *token-nodes) :test #'equalp)
    token))

(defun valid-token-p (token)
  "Determines whether TOKEN is valid or not."
  (let ((token-birth (gethash token *token-births*)))
    (when token-birth
      (< (minutes-since token-birth) 10))))

(defun recall-token (info-hash)
  "Retrieves the token values associated with INFO-HASH. If a recent enough
token isn't found, returns NIL."
  (let ((tokens (gethash info-hash *token-hashes*)))
    (remove-if (complement #'valid-token-p) tokens)))

(defun refresh-tokens ()
  "Deletes every token more than 10 minutes old."
  (maphash (lambda (info-hash tokens)
             (mapc (lambda (token)
                     (unless (valid-token-p token)
                       (setf (gethash info-hash *token-hashes*)
                             (delete token tokens :test #'equalp))
                       (remhash token *token-births*)
                       (remhash token *token-nodes*)))
                   tokens))
           *token-hashes*))
