(in-package #:dhticl-krpc)

(defvar *default-port* 8999)

(defvar *token-table* (make-hash-table))

(defstruct token
  (birth nil :type fixnum :read-only t)
  (value))

(defun make-hash (byte-vector)
  "Hashes BYTE-VECTOR using the SHA1 algorithm."
  (ironclad:digest-sequence :sha1 byte-vector))

(defun ensure-hash (source)
  ""
  (if (equal (list 'simple-array
		   (list 'unsigned-byte 8)
		   (list 20))
	     (type-of source))
      source
      (make-hash source)))

(defun de-bencode (object)
  "Takes a bencoded OBJECT and decodes it. Dictionaries are mapped
  to hash tables, and lists are mapped to lists."
  (if (pathnamep object)
      (with-open-file (file object :element-type '(unsigned-byte 8))
	(bencode:decode object))
      (bencode:decode object)))

(defun make-char-list (byte-vector)
  "Returns a list of characters made by using BYTE-VECTOR as an array of
  character codes."
  (map 'list (lambda (x) (code-char x)) byte-vector))

(defun generate-transaction-id (&optional (stream nil))
  "Creates a transaction ID and writes it as a string to STREAM. If STREAM is
  NIL (the default), returns the string directly."
  (let ((array (make-array 8))) ; 2 bytes
    (flet ((random-bit (x)
	     (declare (ignore x))
	     (random 2)))
      (format stream "~{~S~}~{~S~}"
	      (map 'list #'random-bit array)
	      (map 'list #'random-bit array)))))

(defmacro send-message (type stream &body body)
  "Sends a TYPE (query, response, or error) KRPC message to STREAM."
  `(bencode:encode
    (bencode:decode (format nil
			    (concatenate 'string
					 "d1:t16:~A1:y1:~A"
					 ,@body
					 "e")
			    (generate-transaction-id)
			    ,(case type
			       ((:query) "q")
			       ((:response) "r")
			       ((:error) "e"))))
    ,stream))

(defvar *parsed-id*
  (make-char-list (ironclad:hex-string-to-byte-array +my-id+)))

(defun memorize-token (torrent token)
  "Associates TOKEN with TORRENT."
  (let ((hash (ensure-hash torrent)))
    (setf (gethash hash *token-table*) token)))

(defun forget-token (torrent)
  "Purges the token associated with TORRENT."
  (let ((hash (ensure-hash torrent)))
    (remhash hash *token-table*)))

(defun recall-token (torrent)
  "Retrieves the token value associated with TORRENT."
  (let ((hash (ensure-hash torrent)))
    (token-value (gethash hash *token-table*))))

(defun ponder-token (torrent)
  "Decides whether to keep the token currently associated with TORRENT or not
  based on its age."
  (let ((hash (ensure-hash torrent)))
    (when (> (minutes-since (token-birth (gethash hash *token-table*)))
	     10)
      (forget-token hash))))

(defun reconsider-tokens ()
  "Decides whether to keep any tokens we have based on their age."
  (let ((slate nil))
    (maphash (lambda (hash token)
	       (when (> (minutes-since (token-birth token))
			10)
		 (push hash slate)))
	     *token-table*)
    (mapcan (lambda (key) (remhash key *token-table*))
	    slate)))

;;; TODO: figure out usocket and make this actually work
(defun ping (node &optional (stream nil))
  "Pings NODE and returns its response, or NIL after too much time has passed."
  (send-message :query stream
    (format nil "1:q4:ping1:ad2:id20:~{~C~}e" *parsed-id*)))

(defun find-node (node stream)
  "Asks NODE for contact information."
  (let ((target (node-id node)))
    (send-message :query stream
      (format nil "1:q9:find_node1:ad2:id20:~{~C~}6:target20:~{~C~}e"
	      *parsed-id* (make-char-list
			   (ironclad:hex-string-to-byte-array target))))))

(defun get-peers (torrent stream)
  "Asks for peers associated with TORRENT's infohash."
  (let ((hash (make-char-list (make-hash torrent))))
    (send-message :query stream
      (format nil "1:q9:get_peers1:ad2:id20:~{~C~}9:info_hash20:~{~C~}e"
	      *parsed-id* hash))))

(defun announce-peer (torrent stream)
  "Announces us as a peer of TORRENT."
  (let* ((hash (make-hash torrent))
	 (info-hash (make-char-list hash))
	 (token (recall-token hash)))
    (send-message :query stream
      (format nil "1:q13:announce_peer1:ad2:id20:~{~C~}9:info_hash20:~{~C~}4:porti~De5:token~D:~Ae"
	      *parsed-id* info-hash *default-port* (length token) token))))

;;;;TODO: make another layer of abstraction
(defun ping-node (node))
