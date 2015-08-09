(in-package #:dhticl-krpc)

(defvar *default-port* 8999)

(defvar *token-table* (make-hash-table))

(defvar *current-secret* nil)

(defvar *previous-secret* nil)

(defstruct token
  (birth nil :type fixnum :read-only t)
  (value))

(defun make-hash (byte-vector)
  "Hashes BYTE-VECTOR using the SHA1 algorithm."
  (ironclad:digest-sequence :sha1 byte-vector))

(defun ensure-hash (vague-hash)
  "Ensures VAGUE-HASH is really an SHA1 hash. If it is, return it, otherwise
  hash it and return the hash."
  (if (equal (list 'simple-array
		   (list 'unsigned-byte 8)
		   (list 20))
	     (type-of vague-hash))
      vague-hash
      (make-hash vague-hash)))

(defun de-bencode (object)
  "Takes a bencoded OBJECT and decodes it. Dictionaries are mapped
  to hash tables, and lists are mapped to lists."
  (if (pathnamep object)
      (with-open-file (file object :element-type '(unsigned-byte 8))
	(bencode:decode object))
      (bencode:decode object)))

(defun really-send (data socket)
  "Forces DATA to be sent through SOCKET rather than accumulating in a buffer."
  (let ((stream (usocket:socket-stream socket)))
    (format stream "~A" data)
    (force-output stream)))

(defun make-char-list (byte-vector)
  "Returns a list of characters made by using BYTE-VECTOR as an array of
  character codes."
  (map 'list (lambda (x) (code-char x)) byte-vector))

(defun generate-transaction-id (&optional (stream nil))
  "Creates a transaction ID and writes it as a string to STREAM. If STREAM is
  NIL (the default), returns the string directly."
  (let ((array (make-array 8))) ; 2 byte transaction id
    (flet ((random-bit (x)
	     (declare (ignore x))
	     (random 2)))
      (format stream "~{~S~}~{~S~}"
	      (map 'list #'random-bit array)
	      (map 'list #'random-bit array)))))

(defun parse-node-ip (ip)
  "Parses a node's IP field into a usable form."
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

(defun parse-node-id (id)
  "Parses a node's ID field into a usable form."
  (make-char-list (ironclad:hex-string-to-byte-array id)))

(defvar *parsed-id*
  (parse-node-id +my-id+))

(defun make-secret ()
  "Makes a secret."
  (let ((vec (make-array 5 :element-type '(unsigned-byte 8))))
    (map-into vec (lambda (x) (declare (ignore x)) (random 160)) (make-array 5))
    (setf *previous-secret* *current-secret*
	  *current-secret* (cons vec (get-universal-time)))
    vec))

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

(defun invent-token (ip)
  "Creates a token using IP."
  (let ((ip-vec (parse-node-ip ip)))
    (make-token :birth (get-universal-time)
		:value (concatenate 'vector (make-hash ip-vec)
				    (ensure-secret)))))

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
  "Decides whether to keep any tokens we have based on their age. Deletes every
  token more than 10 minutes old."
  (let ((slate nil))
    (maphash (lambda (hash token)
	       (when (> (minutes-since (token-birth token))
			10)
		 (push hash slate)))
	     *token-table*)
    (mapcan (lambda (key) (remhash key *token-table*))
	    slate)))

(defmacro send-message (type target
			&key (transaction-id (generate-transaction-id))
			&body body)
  "Sends a TYPE (query, response, or error) KRPC message to TARGET."
  (let ((real-target (multiple-value-call (lambda (ip port)
					    (usocket:socket-connect
					     ip port :protocol :datagram))
		       (parse-node-ip (node-ip target)))))
    `(bencode:encode
      (bencode:decode (format nil
			      (concatenate 'string
					   "d1:t16:~A1:y1:~A1:~:*~A"
					   ,@body
					   "e")
			      ,transaction-id
			      ,(case type
				     ((:query) "q")
				     ((:response) "r")
				     ((:error) "e"))))
      ,real-target)))

;;;; official RPC stuff
(defun ping (node)
  "Sends a PING query to NODE."
  (send-message :query node
    (format nil "4:ping1:ad2:id20:~{~C~}e" *parsed-id*)))

(defun find-node (node)
  "Sends a FIND_NODE query to NODE."
  (let ((target (node-id node)))
    (send-message :query node
      (format nil "9:find_node1:ad2:id20:~{~C~}6:target20:~{~C~}e"
	      *parsed-id* (parse-node-id target)))))

(defun get-peers (torrent node)
  "Sends a GET_PEERS query to NODE with respect to torrent."
  (let ((hash (make-char-list (ensure-hash torrent))))
    (send-message :query node
      (format nil "9:get_peers1:ad2:id20:~{~C~}9:info_hash20:~{~C~}e"
	      *parsed-id* hash))))

(defun announce-peer (torrent node)
  "Sends an ANNOUNCE_PEER query to NODE with respect to TORRENT."
  (let* ((hash (ensure-hash torrent))
	 (info-hash (make-char-list hash))
	 (token (recall-token hash)))
    (send-message :query node
      (format nil "13:announce_peer1:ad2:id20:~{~C~}9:info_hash20:~{~C~}4:porti~De5:token~D:~Ae"
	      *parsed-id* info-hash *default-port* (length token) token))))

(defun respond-to-query (node string &rest args)
  "Responds to a querying NODE."
  (let ((transaction-id ))
    (send-message :response node :transaction-id transaction-id
		  (format nil string args))))

(flet ((respond-to-query (string node)
	 (let ((transaction-id ))
	   (send-message :response node :transaction-id transaction-id
			 (format nil
				 (concatenate 'string
					      string
					      "e"))))))
  (defun respond-to-ping (query node)
    "Sends a PING response to the querying NODE."
    (respond-to-query (concatenate 'string
				   "d2:id20:"
				   (format nil "~{~C~}"
					   *parsed-id*))
		      node))

  (defun respond-to-find-node (query node response)
    "Sends a FIND_NODE response to the querying NODE."
    (let ((listp (listp response)))
      (respond-to-query (concatenate 'string
				     (format nil "d2:id20:~{~C~}5:nodes"
					     *parsed-id*)
				     (format nil
					     (if listp
						 "48:~{~{~C~}~}"
						 "6:~{~C~}")
					     (if listp
						 (mapcan (lambda (x)
							   (parse-node-ip
							    (node-ip x)))
							 response)
						 (parse-node-ip
						  (node-ip response)))))
			node)))

  (defun respond-to-get-peers (query node)
    "Sends a GET_PEERS response to the querying NODE."
    (respond-to-query (concatenate 'string
				   "")
		      ;; TODO
		      node))

  (defun respond-to-announce-peer (query node)
    "Sends an ANNOUNCE_PEER response to the querying NODE."
    (respond-to-query (concatenate 'string
				   "")
		      ;; TODO
		      node)))

(defun dht-error ()
  "Emits an error message."
  ;; TODO
  )

;;;; ends here
(defun listen-closely ()
  "Creates a temporary listening socket to receive responses."
  (flet ((parse-response (socket)
	   (let ((stream (usocket:socket-stream socket)))
	     (awhen (dotimes (i 30)
		      (let ((read (read stream)))
			(when read
			  (return read))
			(sleep .25)))
	       it))))
    (parse-response (usocket:socket-listen
		     (vector 127 0 0 1) *default-port*
		     :element-type '(unsigned-byte 8)))))

;;;;TODO: make another layer of abstraction
(defun poke-node (node)
  "Pings NODE and waits a short length of time for a response. Returns the
  response if we get one, otherwise returns NIL."
  (ping node)
  (listen-closely))

(defun hit-on-node (node)
  "Asks NODE for its contact information."
  (find-node node)
  (listen-closely))

(defun ask-node-for-peers (node torrent)
  "Asks NODE for peers of TORRENT."
  (get-peers torrent node)
  (listen-closely))

(defun inform-node (node torrent)
  "Tells NODE we're entering the swarm for TORRENT."
  (announce-peer torrent node)
  (listen-closely))
