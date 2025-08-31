;;;; KRPC--Kademlia Remote Procedure Call
;;;; This file contains messages to be sent
;;;; over the network, including queries,
;;;; responses, and error messages.

(in-package #:dhticl)

(defvar *default-port* 6881
  "The default port to use when listening.")

(defvar *use-implied-port-p* nil
  "Whether peers should use the visible port (T) or the given port (NIL).
Useful for NATs.")

(defvar *transactions* (make-hash-table :test #'equalp)
  "A hash table storing valid, active transaction IDs we've generated.
Maps to info_hash when applicable.")

(defun send-bencoded-data (socket data)
  (let ((bencoded-data (encode data nil)))
    (socket-send socket bencoded-data (length bencoded-data))))

(defun connect-listening-socket (ip port)
  "Connects a UDP socket to IP and PORT, binding it to the default port for
listening."
  (socket-connect ip port :local-port *default-port*
                  :protocol :datagram
                  :element-type '(unsigned-byte 8)))

(defun generate-transaction-id ()
  "Creates a transaction ID and returns it as a byte-vector."
  (random-data 2))
;;; TODO: potentially include our own contact information
(defun pack-nodes-response (target)
  "Packs the contact information for the k closest nodes to TARGET into compact
format."
  (reduce #'concat-vec (find-closest-nodes target) :key #'compact-node-info))

;;; Queries

(defun ping-node (socket transaction-id)
  "Sends a ping message."
  (let ((query-dict (make-hash-table :test #'equal))
        (query-arguments (make-hash-table :test #'equal)))
    (setf (gethash "id" query-arguments) *id*

          (gethash "t" query-dict) transaction-id
          (gethash "y" query-dict) "q"
          (gethash "q" query-dict) "ping"
          (gethash "a" query-dict) query-arguments

          (gethash transaction-id *transactions*) t)
    (send-bencoded-data socket query-dict)))

(defun find-node (socket node-id transaction-id)
  "Asks for a node's contact information."
  (let ((query-dict (make-hash-table :test #'equal))
        (query-arguments (make-hash-table :test #'equal)))
    (setf (gethash "id" query-arguments) *id*
          (gethash "target" query-arguments) node-id

          (gethash "t" query-dict) transaction-id
          (gethash "y" query-dict) "q"
          (gethash "q" query-dict) "find_node"
          (gethash "a" query-dict) query-arguments

          (gethash transaction-id *transactions*) node-id)
    (send-bencoded-data socket query-dict)))

(defun get-peers (socket info-hash transaction-id)
  "Asks for peers under INFO-HASH."
  (let ((query-dict (make-hash-table :test #'equal))
        (query-arguments (make-hash-table :test #'equal)))
    (setf (gethash "id" query-arguments) *id*
          (gethash "info_hash" query-arguments) info-hash

          (gethash "t" query-dict) transaction-id
          (gethash "y" query-dict) "q"
          (gethash "q" query-dict) "get_peers"
          (gethash "a" query-dict) query-arguments

          (gethash transaction-id *transactions*) info-hash)
    (send-bencoded-data socket query-dict)))

(defun announce-peer (socket info-hash transaction-id token)
  "Announces peer status under INFO-HASH."
  (let ((query-dict (make-hash-table :test #'equal))
        (query-arguments (make-hash-table :test #'equal)))
    (setf (gethash "id" query-arguments) *id*
          (gethash "implied_port" query-arguments) (if *use-implied-port-p* 1 0)
          (gethash "info_hash" query-arguments) info-hash
          (gethash "port" query-arguments) *default-port*
          (gethash "token" query-arguments) token

          (gethash "t" query-dict) transaction-id
          (gethash "y" query-dict) "q"
          (gethash "q" query-dict) "announce_peer"
          (gethash "a" query-dict) query-arguments

          (gethash transaction-id *transactions*) info-hash)
    (send-bencoded-data socket query-dict)))

(defun send-message (type ip port transaction-id &key id info-hash)
  "Sends NODE a TYPE message. TYPE should be a keyword like
:PING or :FIND_NODE."
  (with-connected-socket (socket (connect-listening-socket ip port))
    (handler-case (case type
                    (:ping (ping-node socket transaction-id))
                    (:store)
                    (:find_node (find-node socket id transaction-id))
                    (:find_value)
                    (:get_peers (get-peers socket info-hash transaction-id))
                    (:announce_peer (announce-peer socket info-hash
                                                   transaction-id
                                                   (gethash ip *token-ips*))))
      (simple-error () (invoke-restart :continue)))))

;;; Responses to queries

(defun respond-to-ping (socket dict node)
  "Responds to a ping query."
  (let ((response-dict (make-hash-table :test #'equal))
        (response-arguments (make-hash-table :test #'equal)))
    (setf (gethash "id" response-arguments) *id*

          (gethash "t" response-dict) (gethash "t" dict)
          (gethash "y" response-dict) "r"
          (gethash "r" response-dict) response-arguments)
    (send-bencoded-data socket response-dict)))

(defun respond-to-find-node (socket dict node)
  "Responds to a find_node query."
  (let ((response-dict (make-hash-table :test #'equal))
        (response-arguments (make-hash-table :test #'equal))
        (dict-arguments (gethash "a" dict)))
    (setf (gethash "id" response-arguments) *id*

          (gethash "nodes" response-arguments)
          (let* ((target (gethash "target" dict-arguments))
                 (node-if-found (find-node-in-table target)))
            (if node-if-found
                (compact-node-info node-if-found)
                (pack-nodes-response target)))

          (gethash "t" response-dict) (gethash "t" dict)
          (gethash "y" response-dict) "r"
          (gethash "r" response-dict) response-arguments)
    (send-bencoded-data socket response-dict)))

(defun respond-to-get-peers (socket dict node)
  "Responds to a get_peers query."
  (let* ((dict-arguments (gethash "a" dict))
         (hash (gethash "info_hash" dict-arguments))
         (peers (have-peers hash))
         (response-dict (make-hash-table :test #'equal))
         (response-arguments (make-hash-table :test #'equal)))
    (setf (gethash "id" response-arguments) *id*
          (gethash "token" response-arguments) (invent-token hash node)

          (gethash "t" response-dict) (gethash "t" dict)
          (gethash "y" response-dict) "r"
          (gethash "r" response-dict) response-arguments)
    (if peers
        (setf (gethash "values" response-arguments)
              (mapcar #'compact-peer-info peers))
        (setf (gethash "nodes" response-arguments)
              (pack-nodes-response hash)))
    (send-bencoded-data socket response-dict)))

(defun respond-to-announce-peer (socket dict node source-port)
  "Responds to an announce_peer query. If the received token isn't valid,
sends a protocol error message."
  (let* ((response-dict (make-hash-table :test #'equal))
         (response-arguments (make-hash-table :test #'equal))
         (dict-arguments (gethash "a" dict))
         (id (gethash "id" dict-arguments))
         (implied-port-p (gethash "implied_port" dict-arguments))
         (info-hash (gethash "info_hash" dict-arguments))
         (port (if (and implied-port-p (= implied-port-p 1))
                   ;; if implied_port is 1, use the source port
                   source-port
                   ;; otherwise use the supplied port
                   (gethash "port" dict-arguments)))
         (token (gethash "token" dict-arguments)))
    (setf (node-port node) port)
    (if (consider-token token info-hash node)
        (progn (add-to-bucket node)
               (setf (gethash "id" response-arguments) *id*

                     (gethash "t" response-dict) (gethash "t" dict)
                     (gethash "y" response-dict) "r"
                     (gethash "r" response-dict) response-arguments)
               (send-bencoded-data socket response-dict))
        (dht-error socket :protocol dict))))

(defun dht-error (socket type dict)
  "Sends a DHT error message."
  (let ((error-dict (make-hash-table :test #'equal)))
    (setf (gethash "t" error-dict) (gethash "t" dict)
          (gethash "y" error-dict) "e"
          (gethash "e" error-dict) (case type
                                     (:generic (list 201 "Generic Error"))
                                     (:server (list 202 "Server Error"))
                                     (:protocol (list 203 "Protocol Error"))
                                     (:method (list 204 "Method Unknown"))))
    (send-bencoded-data socket error-dict)))

(defun send-response (type node dict &key error-type source-port)
  (let ((ip (node-ip node))
        (port (node-port node)))
    (with-connected-socket (socket (connect-listening-socket ip port))
      (handler-case (case type
                      (:ping
                       (respond-to-ping socket dict node))
                      (:find_node
                       (respond-to-find-node socket dict node))
                      (:get_peers
                       (respond-to-get-peers socket dict node))
                      (:announce_peer
                       (respond-to-announce-peer socket
                                                 dict
                                                 node
                                                 source-port))
                      (:dht_error (dht-error socket error-type dict)))
        (simple-error () (invoke-restart :continue))))))