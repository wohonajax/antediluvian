(in-package #:dhticl)

(defvar *token-births* (make-hash-table :test #'equalp)
  "A hash table mapping tokens to their creation times.")

(defvar *token-hashes* (make-hash-table :test #'equalp)
  "A hash table mapping info_hashes to tokens valid for them.")

(defun generate-transaction-id ()
  "Creates a transaction ID and returns it as a string."
  (let ((array (make-array 2 :element-type '(unsigned-byte 8))))
    (setf (aref array 0) (random 256)
          (aref array 1) (random 256))
    array))

(defun parse-node-ip (byte-vector)
  "Takes a byte-vector in compact peer format and returns an IP address and
port as multiple values."
  (assert (= (length byte-vector) 6)) ; TODO: support IPv6
  (values (subseq byte-vector 0 4)
          (port-from-octet-buffer (subseq byte-vector 4))))

(defun make-secret ()
  "Makes a secret."
  (let* ((secret-length 5)
         (secret (make-array secret-length :element-type '(unsigned-byte 8))))
    (dotimes (i secret-length secret)
      (setf (aref secret i) (strong-random 160)))))

(defparameter *current-secret* (cons (make-secret) (get-universal-time)))
(defvar *previous-secret*)

(defun ensure-secret ()
  "Makes sure the current secret isn't stale. If it is, makes a fresh secret."
  (when (> (minutes-since (cdr *current-secret*))
           5)
    (shiftf *previous-secret*
            *current-secret*
            (cons (make-secret) (get-universal-time)))
  (car *current-secret*)))

(defun hash-ip-and-secret (ip secret)
  "Returns the SHA1 hash of IP concatenated onto SECRET."
  (make-hash (concatenate '(vector (unsigned-byte 8)) ip secret)))

(defun invent-token (info-hash node)
  "Creates a token associated with INFO-HASH and NODE."
  (let ((token (hash-ip-and-secret (node-ip node) (ensure-secret))))
    (setf (gethash token *token-births*) (get-universal-time))
    (push token (gethash info-hash *token-hashes*))
    token))

(defun verify-token (token node)
  "Verifies whether TOKEN comes from us and is valid for a given NODE."
  (let ((ip (node-ip node)))
    (or (equalp token (hash-ip-and-secret ip *current-secret*))
        (equalp token (hash-ip-and-secret ip *previous-secret*)))))

(defun consider-token (token info-hash node)
  "Checks whether TOKEN is valid for INFO-HASH and NODE or not."
  (and (member token (gethash info-hash *token-hashes*) :test #'equalp)
       (verify-token token node)))

(defun valid-token-p (token)
  "Determines whether TOKEN is valid or not."
  (let ((token-birth (gethash token *token-births*)))
    (and token-birth
         (< (minutes-since token-birth) 10))))

(defun recall-token (info-hash)
  "Retrieves the token values associated with INFO-HASH. If a recent enough
token isn't found, returns NIL."
  (let ((tokens (gethash info-hash *token-hashes*)))
    (remove-if-not #'valid-token-p tokens)))

(defun refresh-tokens ()
  "Deletes every token more than 10 minutes old."
  (maphash (lambda (info-hash tokens)
             (mapc (lambda (token)
                     (unless (valid-token-p token)
                       (setf (gethash info-hash *token-hashes*)
                             (remove token tokens :test #'equalp :count 1))
                       (remhash token *token-births*)))
                   tokens))
           *token-hashes*))