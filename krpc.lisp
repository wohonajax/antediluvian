(in-package #:dhticl)

(defvar *default-port* 6881)

(defvar *use-implied-port-p* nil)

(defun receive-data (socket length)
  (let ((buffer (make-array length :element-type '(unsigned-byte 8))))
    (map 'string #'code-char (usocket:socket-receive socket buffer length))))

;;; Queries

(defun ping-node (node client-socket-stream)
  "Sends NODE a ping message."
  (with-listening-usocket socket
    (let ((query-dict (make-hash-table))
          (query-arguments (make-hash-table)))
      (setf (gethash "id" query-arguments) +my-id+

            (gethash "t" query-dict) (generate-transaction-id)
            (gethash "y" query-dict) "q"
            (gethash "q" query-dict) "ping"
            (gethash "a" query-dict) query-arguments)
      (bencode:encode query-dict client-socket-stream)
      (force-output client-socket-stream))
    (bencode:decode (receive-data socket 47))))

(defun find-node (client-socket-stream node-id)
  "Asks a peer for a node's contact information."
  (with-listening-usocket socket
    (let ((query-dict (make-hash-table))
          (query-arguments (make-hash-table)))
      (setf (gethash "id" query-arguments) +my-id+
            (gethash "target" query-arguments) node-id

            (gethash "t" query-dict) (generate-transaction-id)
            (gethash "y" query-dict) "q"
            (gethash "q" query-dict) "find_node"
            (gethash "a" query-dict) query-arguments)
      (bencode:encode query-dict client-socket-stream)
      (force-output client-socket-stream))
    ;; TODO: figure out the right length for RECEIVE-DATA here
    ;; parse the result of RECEIVE-DATA and return a vector of proper length
    ;; parsing should look at the value for "nodes" key's length
    ;; length is 60 + (x * 6), where x is either 1 or k
    (let* ((x (if :k-nodes +k+ 1))
           (buffer-length (+ 60 (* x 6))))
      (bencode:decode (receive-data socket buffer-length)))))

(defun get-peers (client-socket-stream info-hash)
  "Asks for peers under INFO-HASH."
  (with-listening-usocket socket
    (let ((query-dict (make-hash-table))
          (query-arguments (make-hash-table)))
      (setf (gethash "id" query-arguments) +my-id+
            (gethash "info_hash" query-arguments) (ensure-hash info-hash)

            (gethash "t" query-dict) (generate-transaction-id)
            (gethash "y" query-dict) "q"
            (gethash "q" query-dict) "get_peers"
            (gethash "a" query-dict) query-arguments)
      (bencode:encode query-dict client-socket-stream)
      (force-output client-socket-stream))
    ;; TODO: figure out the right length for a buffer here
    ;; if there's a "nodes" key the length is x + (k * 6)
    ;; if there's a "values" key the length is potentially unbounded...
    (let ((buffer-length 1000)) ; FIXME: filler value here
      (bencode:decode (receive-data socket buffer-length)))))

(defun announce-peer (client-socket-stream info-hash)
  "Announces peer status under INFO-HASH."
  (with-listening-usocket socket
    (let* ((query-dict (make-hash-table))
           (query-arguments (make-hash-table))
           (surely-hash (ensure-hash info-hash))
           (token (recall-token surely-hash)))
      (setf (gethash "id" query-arguments) +my-id+
            (gethash "implied_port" query-arguments) (if *use-implied-port-p* 1 0)
            (gethash "info_hash" query-arguments) surely-hash
            (gethash "port" query-arguments) *default-port*
            (gethash "token" query-arguments) token

            (gethash "t" query-dict) (generate-transaction-id)
            (gethash "y" query-dict) "q"
            (gethash "q" query-dict) "announce_peer"
            (gethash "a" query-dict) query-arguments)
      (bencode:encode query-dict client-socket-stream)
      (force-output client-socket-stream))
    (bencode:decode (receive-data socket 47))))
;;; TODO: dht_error correctly
(defun dht-error (client-socket-stream type dict)
  "Sends a DHT error message."
  (let ((error-dict (make-hash-table)))
    (setf (gethash "t" error-dict) (gethash "t" dict)
          (gethash "y" error-dict) "e"
          (gethash "e" error-dict) (case type
                                     (:generic (list 201 "Generic Error"))
                                     (:server (list 202 "Server Error"))
                                     (:protocol (list 203 "Protocol Error"))
                                     (:method (list 204 "Method Unknown"))))
    (bencode:encode error-dict client-socket-stream)
    (force-output client-socket-stream)))

(defun send-message (type node &key info-hash error-type)
  "Sends NODE a TYPE message. TYPE should be a keyword like
:PING or :FIND_NODE."
  (multiple-value-bind (ip port)
      (parse-node-ip (node-ip node))
    (usocket:with-client-socket
        (target-socket target-stream ip port
                       :protocol :datagram
                       :element-type '(unsigned-byte 8)
                       :timeout 5)
      (handler-case (case type
                      (:ping (ping-node node target-stream))
                      (:store)
                      (:find_node (find-node node target-stream (node-id node)))
                      (:find_value)
                      (:get_peers (get-peers node target-stream info-hash))
                      (:announce_peer (announce-peer target-stream info-hash)))
        (simple-error () (invoke-restart :continue))))))

;;; Responses to queries

(defun respond-to-ping (client-socket-stream dict node)
  "Responds to a ping query."
  (let ((response-dict (make-hash-table))
        (response-arguments (make-hash-table)))
    (setf (gethash "id" response-arguments) +my-id+

          (gethash "t" response-dict) (gethash "t" dict)
          (gethash "y" response-dict) "r"
          (gethash "r" response-dict) response-arguments)
    (setf (node-health node) :good)
    (bencode:encode response-dict client-socket-stream)
    (force-output client-socket-stream)))

(defun respond-to-find-node (client-socket-stream dict)
  "Responds to a find_node query."
  (let ((response-dict (make-hash-table))
        (response-arguments (make-hash-table)))
    (setf (gethash "id" response-arguments) +my-id+
          (gethash "nodes" response-arguments)
          (let* ((target (gethash "target" dict))
                 (have-target-p (member target *node-list*)))
            (if have-target-p
                (compact-node-info (car have-target-p)) ; MEMBER returns a list
                (with-output-to-string (str)
                  (mapc (lambda (peer) (princ (compact-node-info peer) str))
                        (find-closest-nodes target)))))

          (gethash "t" response-dict) (gethash "t" dict)
          (gethash "y" response-dict) "r"
          (gethash "r" response-dict) response-arguments)
    (setf (node-health node) :good)
    (bencode:encode response-dict client-socket-stream)
    (force-output client-socket-stream)))

(defun respond-to-get-peers (client-socket-stream dict node)
  "Responds to a get_peers query."
  (let* ((arguments-dict (gethash "a" dict))
         (hash (gethash "info_hash" arguments-dict))
         (peers (have-peers hash))
         (response-dict (make-hash-table))
         (response-arguments (make-hash-table)))
    (setf (gethash "id" response-arguments) +my-id+
          (gethash "token" response-arguments) (invent-token hash node)

          (gethash "t" response-dict) (gethash "t" dict)
          (gethash "y" response-dict) "r"
          (gethash "r" response-dict) response-arguments)
    (if peers
        (setf (gethash "values" response-arguments)
              (mapcar #'compact-node-info peers))
        (setf (gethash "nodes" response-arguments) ;; TODO: is a list correct?
              (mapcar #'compact-node-info (find-closest-nodes hash))))
    (setf (node-health node) :good)
    (bencode:encode response-dict client-socket-stream)
    (force-output client-socket-stream)))

(defun respond-to-announce-peer (client-socket-stream dict client-socket)
  "Responds to an announce_peer query. If the received token isn't valid,
sends a protocol error message."
  (let* ((response-dict (make-hash-table))
         (response-arguments (make-hash-table))
         (argument-dict (gethash "a" dict))
         (id (gethash "id" argument-dict))
         (implied-port-p (gethash "implied_port" argument-dict))
         (port (if (and implied-port-p (= implied-port-p 1))
                   ;; if implied_port is 1, use the source port
                   (usocket:get-peer-port client-socket) ; FIXME: actually get a new port
                   ;; otherwise use the supplied port
                   (gethash "port" argument-dict)))
         (token (gethash "token" argument-dict))
         (node (create-node :id id
                            :ip (usocket:get-peer-address client-socket)
                            :port port
                            :distance
                            (calculate-distance (convert-id-to-int id)
                                                (convert-id-to-int +my-id+))
                            :last-activity (get-universal-time)
                            :health :good
                            ;; TODO: associate a node with multiple hashes
                            :hashes (list (gethash "info_hash"
                                                   argument-dict)))))
    (if (consider-token node token)
        (progn (add-to-bucket node)
               (setf (gethash "id" response-arguments) +my-id+

                     (gethash "t" response-dict) (gethash "t" dict)
                     (gethash "y" response-dict) "r"
                     (gethash "r" response-dict) response-arguments)
               (bencode:encode response-dict client-socket-stream)
               (force-output client-socket-stream))
        (dht-error client-socket-stream :protocol dict))))

(defun send-response (type node dict)
  (multiple-value-bind (ip port)
      (parse-node-ip (node-ip node))
    (usocket:with-client-socket
        (target-socket target-stream ip port
                       :protocol :datagram
                       :element-type '(unsigned-byte 8)
                       :timeout 5)
      (handler-case (case type
                      (:ping
                       (respond-to-ping target-stream dict node))
                      (:find_node
                       (respond-to-find-node target-stream dict node))
                      (:get_peers
                       (respond-to-get-peers target-stream dict node))
                      (:announce_peer
                       (respond-to-announce-peer target-stream
                                                 dict
                                                 target-socket)))
        (simple-error () (invoke-restart :continue))))))
