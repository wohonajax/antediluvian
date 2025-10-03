(in-package #:antediluvian)

(defvar *current-secret* nil)
(defvar *previous-secret* nil)

(defvar *token-births* (make-hash-table :test #'equalp)
  "A hash table mapping tokens to their creation times.")

(defvar *token-hashes* (make-hash-table :test #'equalp)
  "A hash table mapping info_hashes to tokens valid for them.")

(defvar *token-ips* (make-hash-table :test #'equalp)
  "A hash table mapping IP addresses to tokens valid for them.")

(defun parse-node-ip (byte-vector)
  "Takes a byte vector in compact peer format and returns an IP address and
port as multiple values."
  (assert (= (length byte-vector) 6)) ; TODO: support IPv6
  (values (subseq byte-vector 0 4)
          (port-from-octet-buffer (subseq byte-vector 4))))

(defun make-secret ()
  "Makes a secret out of random data."
  (random-data 16))

(defun start-sercret-rotation-thread ()
  "Starts a thread that rotates secrets every 5 minutes."
  (make-thread (lambda ()
                 (loop (shiftf *previous-secret*
                               *current-secret*
                               (make-secret))
                       (sleep 300)))
               :name "dht-secret-rotation"))

(defvar *secret-rotation-thread* nil
  "A thread that rotates secrets for tokens every 5 minutes.")

(defun hash-ip-and-secret (ip secret)
  "Returns the SHA1 hash of IP concatenated onto SECRET."
  (digest-sequence :sha1 (concat-vec ip secret)))

(defun invent-token (info-hash node)
  "Creates a token associated with INFO-HASH and NODE."
  (let* ((ip (node-ip node))
         (token (hash-ip-and-secret ip *current-secret*)))
    (setf (gethash token *token-births*) (get-universal-time))
    (pushnew token (gethash ip *token-ips*) :test #'equalp)
    (pushnew token (gethash info-hash *token-hashes*) :test #'equalp)
    token))
;; TODO: verify that tokens came from us (in networking.lisp probably)
(defun verify-token (token node)
  "Verifies whether TOKEN comes from us and is valid for a given NODE."
  (let ((ip (node-ip node)))
    (or (equalp token (hash-ip-and-secret ip *current-secret*))
        (equalp token (hash-ip-and-secret ip *previous-secret*)))))

(defun valid-token-p (token)
  "Determines whether TOKEN is valid or not."
  ;; make sure we don't try to calculate the time elapsed
  ;; since nil, in case the token isn't in the hash table
  (when-let (token-birth (gethash token *token-births*))
    (< (minutes-since token-birth) 10)))

(defun recall-tokens (info-hash)
  "Retrieves the token values associated with INFO-HASH. If a recent enough
token isn't found, returns NIL."
  (let ((tokens (gethash info-hash *token-hashes*)))
    (remove-if-not #'valid-token-p tokens)))

(defun consider-token (token info-hash node)
  "Checks whether TOKEN is valid for INFO-HASH and NODE or not."
  (and (member token (recall-tokens info-hash) :test #'equalp)
       (verify-token token node)))

(defun refresh-tokens ()
  "Deletes every token more than 10 minutes old."
  (maphash (lambda (ip tokens)
             (setf (gethash ip *token-ips*)
                   (remove-if-not #'valid-token-p tokens)))
           *token-ips*)
  (maphash (lambda (info-hash tokens)
             (mapc (lambda (token)
                     (unless (valid-token-p token)
                       (setf (gethash info-hash *token-hashes*)
                             (remove token tokens :test #'equalp :count 1))
                       (remhash token *token-births*)))
                   tokens))
           *token-hashes*))
