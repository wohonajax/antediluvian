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

(defun generate-transaction-id ()
  "Creates a transaction ID and returns it as a byte-vector."
  (random-data 2))

(defun make-dictionary ()
  "Makes a dictionary for queries, responses, and error messages."
  (make-hash-table :test #'equal))

(defun add-to-dictionary (dictionary key value)
  "Adds a pair KEY and VALUE to DICTIONARY."
  (setf (gethash key dictionary) value))

(defun retrieve-dictionary-value (dictionary key)
  "Retrieves the value associated with KEY from DICTIONARY."
  (gethash key dictionary))

(defun pack-nodes-response (target)
  "Packs the contact information for the k closest nodes to TARGET into compact
format."
  (reduce #'concat-vec (find-closest-nodes target) :key #'compact-node-info))

;;; Queries

(defun ping-node (socket transaction-id)
  "Sends a ping message."
  (let ((query-dictionary (make-dictionary))
        (query-arguments (make-dictionary)))
    (add-to-dictionary query-arguments "id" *id*)
    (add-to-dictionary query-dictionary "t" transaction-id)
    (add-to-dictionary query-dictionary "y" "q")
    (add-to-dictionary query-dictionary "q" "ping")
    (add-to-dictionary query-dictionary "a" query-arguments)
    (setf (gethash transaction-id *transactions*) t)
    (send-bencoded-data socket query-dictionary)))

(defun find-node (socket node-id transaction-id)
  "Asks for a node's contact information."
  (let ((query-dictionary (make-dictionary))
        (query-arguments (make-dictionary)))
    (add-to-dictionary query-arguments "id" *id*)
    (add-to-dictionary query-arguments "target" node-id)
    (add-to-dictionary query-dictionary "t" transaction-id)
    (add-to-dictionary query-dictionary "y" "q")
    (add-to-dictionary query-dictionary "q" "find_node")
    (add-to-dictionary query-dictionary "a" query-arguments)
    (setf (gethash transaction-id *transactions*) node-id)
    (send-bencoded-data socket query-dictionary)))

(defun get-peers (socket info-hash transaction-id)
  "Asks for peers under INFO-HASH."
  (let ((query-dictionary (make-dictionary))
        (query-arguments (make-dictionary)))
    (add-to-dictionary query-arguments "id" *id*)
    (add-to-dictionary query-arguments "info_hash" info-hash)
    (add-to-dictionary query-dictionary "t" transaction-id)
    (add-to-dictionary query-dictionary "y" "q")
    (add-to-dictionary query-dictionary "q" "get_peers")
    (add-to-dictionary query-dictionary "a" query-arguments)
    (setf (gethash transaction-id *transactions*) info-hash)
    (send-bencoded-data socket query-dictionary)))

(defun announce-peer (socket info-hash transaction-id token)
  "Announces peer status under INFO-HASH."
  (let ((query-dictionary (make-dictionary))
        (query-arguments (make-dictionary)))
    (add-to-dictionary query-arguments "id" *id*)
    (add-to-dictionary query-arguments "implied_port" (if *use-implied-port-p* 1 0))
    (add-to-dictionary query-arguments "info_hash" info-hash)
    (add-to-dictionary query-arguments "port" *default-port*)
    (add-to-dictionary query-arguments "token" token)
    (add-to-dictionary query-dictionary "t" transaction-id)
    (add-to-dictionary query-dictionary "y" "q")
    (add-to-dictionary query-dictionary "q" "announce_peer")
    (add-to-dictionary query-dictionary "a" query-arguments)
    (setf (gethash transaction-id *transactions*) info-hash)
    (send-bencoded-data socket query-dictionary)))

(defun send-message (type ip port transaction-id &key id info-hash)
  "Sends NODE a TYPE message. TYPE should be a keyword like
:PING or :FIND_NODE."
  ;; we might not have a node object yet so we have to bind a temporary socket
  (with-connected-socket
      (socket (socket-connect
               ip port
               :protocol :datagram
               :element-type '(unsigned-byte 8)
               :timeout 5))
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
  (let ((response-dictionary (make-dictionary))
        (response-arguments (make-dictionary)))
    (add-to-dictionary response-arguments "id" *id*)
    (add-to-dictionary response-dictionary "t" (retrieve-dictionary-value dict "t"))
    (add-to-dictionary response-dictionary "y" "r")
    (add-to-dictionary response-dictionary "r" response-arguments)
    (send-bencoded-data socket response-dictionary)))

(defun respond-to-find-node (socket dict node)
  "Responds to a find_node query."
  (let ((response-dictionary (make-dictionary))
        (response-arguments (make-dictionary))
        (dict-arguments (retrieve-dictionary-value dict "a")))
    (add-to-dictionary response-arguments "id" *id*)
    (add-to-dictionary response-arguments "nodes"
                       (let* ((target (retrieve-dictionary-value dict-arguments
                                                                 "target"))
                              (node-if-found (find-node-in-table target)))
                         (if node-if-found
                             (compact-node-info node-if-found)
                             (pack-nodes-response target))))
    (add-to-dictionary response-dictionary "t" (retrieve-dictionary-value dict "t"))
    (add-to-dictionary response-dictionary "y" "r")
    (add-to-dictionary response-dictionary "r" response-arguments)
    (send-bencoded-data socket response-dictionary)))

(defun respond-to-get-peers (socket dict node)
  "Responds to a get_peers query."
  (let* ((dict-arguments (retrieve-dictionary-value dict "a"))
         (hash (retrieve-dictionary-value dict-arguments "info_hash"))
         (peers (have-peers hash))
         (response-dictionary (make-dictionary))
         (response-arguments (make-dictionary)))
    (add-to-dictionary response-arguments "id" *id*)
    (add-to-dictionary response-arguments "token" (invent-token hash node))
    (add-to-dictionary response-dictionary "t" (retrieve-dictionary-value dict "t"))
    (add-to-dictionary response-dictionary "y" "r")
    (add-to-dictionary response-dictionary "r" response-arguments)
    (if peers
        (add-to-dictionary response-arguments "values" (mapcar #'compact-peer-info peers))
        (add-to-dictionary response-arguments "nodes" (pack-nodes-response)))
    (send-bencoded-data socket response-dictionary)))

(defun respond-to-announce-peer (socket dict node source-port)
  "Responds to an announce_peer query. If the received token isn't valid,
sends a protocol error message."
  (let* ((response-dictionary (make-dictionary))
         (response-arguments (make-dictionary))
         (dict-arguments (retrieve-dictionary-value dict "a"))
         (id (retrieve-dictionary-value dict-arguments "id"))
         (implied-port-p (retrieve-dictionary-value dict-arguments "implied_port"))
         (info-hash (retrieve-dictionary-value dict-arguments "info_hash"))
         (port (if (and implied-port-p (= implied-port-p 1))
                   ;; if implied_port is 1, use the source port
                   source-port
                   ;; otherwise use the supplied port
                   (retrieve-dictionary-value dict-arguments "port")))
         (token (retrieve-dictionary-value dict-arguments "token")))
    (setf (node-port node) port)
    (cond ((consider-token token info-hash node)
           ;; TODO: add to peer list
           (add-to-dictionary response-arguments "id" *id*)
           (add-to-dictionary response-dictionary "t" (retrieve-dictionary-value dict "t"))
           (add-to-dictionary response-dictionary "y" "r")
           (add-to-dictionary response-dictionary "t" response-arguments)
           (send-bencoded-data socket response-dictionary))
          (t (dht-error socket :protocol dict)))))

(defun dht-error (socket type dict)
  "Sends a DHT error message."
  (let ((error-dictionary (make-dictionary)))
    (add-to-dictionary error-dictionary "t" (retrieve-dictionary-value dict "t"))
    (add-to-dictionary error-dictionary "y" "e")
    (add-to-dictionary error-dictionary "e" (case type
                                              (:generic (list 201 "Generic Error"))
                                              (:server (list 202 "Server Error"))
                                              (:protocol (list 203 "Protocol Error"))
                                              (:method (list 204 "Method Unknown"))))
    (send-bencoded-data socket error-dictionary)))

(defun send-response (type node dict &key error-type source-port)
  (let ((ip (node-ip node))
        (port (node-port node))
        (target-socket (node-socket node)))
    (handler-case (case type
                    (:ping
                     (respond-to-ping target-socket dict node))
                    (:find_node
                     (respond-to-find-node target-socket dict node))
                    (:get_peers
                     (respond-to-get-peers target-socket dict node))
                    (:announce_peer
                     (respond-to-announce-peer target-socket
                                               dict
                                               node
                                               source-port))
                    (:dht_error (dht-error target-socket error-type dict)))
      (simple-error () (invoke-restart :continue)))))