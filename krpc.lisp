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

(defun make-string-from-bytes (byte-vector)
  "Returns a string of characters made by using BYTE-VECTOR as an array of
  character codes."
  (map 'string (lambda (x) (code-char x)) byte-vector))

(defun generate-transaction-id (&optional (stream nil))
  "Creates a transaction ID and writes it as a string to STREAM. If STREAM is
  NIL (the default), returns the string directly."
  (let ((array (make-array 8))) ; 2 byte transaction id
    (flet ((random-bit (x)
	     (declare (ignore x))
	     (random 2)))
      (format stream "~A~A"
	      (make-string-from-bytes (map 'vector #'random-bit array))
	      (make-string-from-bytes (map 'vector #'random-bit array))))))

(defun get-transaction-id (query)
  "Retrieves the transaction ID from the Bencoded QUERY."
  (gethash "t" (de-bencode query)))

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
  (make-string-from-bytes (ironclad:hex-string-to-byte-array id)))

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

(defun send-message (type target transaction-id &rest body)
  "Sends a TYPE (query, response, or error) KRPC message to TARGET."
  (let ((real-target (multiple-value-call (lambda (ip port)
					    (usocket:socket-connect
					     ip port :protocol :datagram))
		       (parse-node-ip (node-ip target)))))
    (bencode:encode
     (bencode:decode (format nil
			     (concatenate 'string
					  "d1:t16:~{~A~}1:y1:~A1:~:*~A"
					  body
					  "e")
			     transaction-id
			     (case type
			       ((:query) "q")
			       ((:response) "r")
			       ((:error) "e"))))
     real-target)))

;;;; official RPC stuff
(defun ping (node)
  "Sends a PING query to NODE."
  (send-message :query node (generate-transaction-id)
    (format nil "4:ping1:ad2:id20:~Ae" *parsed-id*)))

(defun find-node (node)
  "Sends a FIND_NODE query to NODE."
  (let ((target (node-id node)))
    (send-message :query node (generate-transaction-id)
      (format nil "9:find_node1:ad2:id20:~A6:target20:~Ae"
	      *parsed-id* (parse-node-id target)))))

(defun get-peers (torrent node)
  "Sends a GET_PEERS query to NODE with respect to torrent."
  (let ((hash (make-string-from-bytes (ensure-hash torrent))))
    (send-message :query node (generate-transaction-id)
      (format nil "9:get_peers1:ad2:id20:~A9:info_hash20:~Ae"
	      *parsed-id* hash))))

(defun announce-peer (torrent node)
  "Sends an ANNOUNCE_PEER query to NODE with respect to TORRENT."
  (let* ((hash (ensure-hash torrent))
	 (info-hash (make-string-from-bytes hash))
	 (token (recall-token hash)))
    (send-message :query node (generate-transaction-id)
      (format nil "13:announce_peer1:ad2:id20:~A9:info_hash20:~A4:porti~De5:token~D:~Ae"
	      *parsed-id* info-hash *default-port* (length token) token))))

(flet ((respond-to-query (string node transaction-id)
	 (send-message :response node transaction-id
	   (format nil
		   (concatenate 'string
				string
				"e")))))
  (defun respond-to-ping (query node)
    "Sends a PING response to the querying NODE."
    (respond-to-query (concatenate 'string
				   "d2:id20:"
				   (format nil "~A"
					   *parsed-id*))
		      node
		      (get-transaction-id query)))

  (defun respond-to-find-node (query node response)
    "Sends a FIND_NODE response to the querying NODE."
    (let ((listp (listp response)))
      (respond-to-query (concatenate 'string
				     (format nil "d2:id20:~A5:nodes"
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
			node
			(get-transaction-id query))))

  (defun respond-to-get-peers (query node)
    "Sends a GET_PEERS response to the querying NODE."
    (respond-to-query (concatenate 'string
				   "")
		      ;; TODO
		      node
		      (get-transaction-id query)))

  (defun respond-to-announce-peer (query node)
    "Sends an ANNOUNCE_PEER response to the querying NODE."
    (respond-to-query (concatenate 'string
				   "")
		      ;; TODO
		      node
		      (get-transaction-id query))))

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

(defun beg-node (node torrent)
  "Asks NODE for peers of TORRENT."
  (get-peers torrent node)
  (listen-closely))

(defun divulge-to-node (node torrent)
  "Tells NODE we're entering the swarm for TORRENT."
  (announce-peer torrent node)
  (listen-closely))

(defun calculate-elapsed-inactivity (node)
  "Returns the time in minutes since NODE's last seen activity."
  (let ((last-activity (node-last-activity node)))
    (and last-activity (minutes-since last-activity))))

(defun calculate-last-activity (node)
  "Returns the universal timestamp of NODE's last seen activity."
  (let ((time-inactive (calculate-elapsed-inactivity node)))
    (cond (time-inactive time-inactive)
	  ((poke-node node) (get-universal-time))
	  (t nil))))

(defun calculate-node-health (node)
  "Returns the node's health as a keyword, either :GOOD, :QUESTIONABLE, or :BAD."
  (let ((time-inactive (calculate-elapsed-inactivity node)))
    (cond ((null time-inactive) :questionable)
	  ((< time-inactive 15) :good)
	  ((poke-node node) :good)
	  (t :bad))))

(defun update-node (node)
  "Recalculates the time since NODE's last activity and updates its health
  accordingly."
  (setf (node-last-activity node) (calculate-last-activity node)
	(node-health node) (calculate-node-health node)))
