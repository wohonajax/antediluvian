;;;; KRPC--Kademlia Remote Procedure Call
;;;; This file contains messages to be sent
;;;; over the network, including queries,
;;;; responses, and error messages.

(in-package #:dhticl)

(defvar *default-port* 6881)

(defvar *use-implied-port-p* nil)

(defvar *transactions* (make-hash-table :test #'equal)
  "A hash table storing valid transaction IDs we've generated.
Maps to info_hash when applicable.")

(defun receive-data ()
  (socket-receive
   *listening-socket*
   nil ;; FIXME: this just hangs waiting for a huge response
   +max-datagram-packet-size+))

(defun send-bencoded-data (socket data)
  (let ((bencoded-data (bencode:encode data nil)))
    (socket-send socket bencoded-data (length bencoded-data))))

;;; Queries

(defun ping-node (socket transaction-id)
  "Sends a ping message."
  (let ((query-dict (make-hash-table))
        (query-arguments (make-hash-table)))
    (setf (gethash "id" query-arguments) *id*

          (gethash "t" query-dict) transaction-id
          (gethash "y" query-dict) "q"
          (gethash "q" query-dict) "ping"
          (gethash "a" query-dict) query-arguments

          (gethash transaction-id *transactions*) t)
    (send-bencoded-data socket query-dict)))

(defun find-node (socket node-id transaction-id)
  "Asks for a node's contact information."
  (let ((query-dict (make-hash-table))
        (query-arguments (make-hash-table)))
    (setf (gethash "id" query-arguments) *id*
          (gethash "target" query-arguments) node-id

          (gethash "t" query-dict) transaction-id
          (gethash "y" query-dict) "q"
          (gethash "q" query-dict) "find_node"
          (gethash "a" query-dict) query-arguments

          (gethash transaction-id *transactions*) t)
    (send-bencoded-data socket query-dict)))

(defun get-peers (socket info-hash transaction-id)
  "Asks for peers under INFO-HASH."
  (let ((query-dict (make-hash-table))
        (query-arguments (make-hash-table)))
    (setf (gethash "id" query-arguments) *id*
          (gethash "info_hash" query-arguments) info-hash

          (gethash "t" query-dict) transaction-id
          (gethash "y" query-dict) "q"
          (gethash "q" query-dict) "get_peers"
          (gethash "a" query-dict) query-arguments

          (gethash transaction-id *transactions*) info-hash)
    (send-bencoded-data socket query-dict)))

(defun announce-peer (socket info-hash transaction-id)
  "Announces peer status under INFO-HASH."
  (let* ((query-dict (make-hash-table))
         (query-arguments (make-hash-table))
         (token (recall-token info-hash)))
    (setf (gethash "id" query-arguments) *id*
          (gethash "implied_port" query-arguments) (if *use-implied-port-p* 1 0)
          (gethash "info_hash" query-arguments) info-hash
          (gethash "port" query-arguments) *default-port*
          (gethash "token" query-arguments) token

          (gethash "t" query-dict) transaction-id
          (gethash "y" query-dict) "q"
          (gethash "q" query-dict) "announce_peer"
          (gethash "a" query-dict) query-arguments

          (gethash transaction-id *transactions*) t)
    (send-bencoded-data socket query-dict)))

(defun send-message (type ip port transaction-id &key id info-hash)
  "Sends NODE a TYPE message. TYPE should be a keyword like
:PING or :FIND_NODE."
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
                                                   transaction-id)))
      (simple-error () (invoke-restart :continue)))))

;;; Responses to queries

(defun respond-to-ping (socket dict node)
  "Responds to a ping query."
  (let ((response-dict (make-hash-table))
        (response-arguments (make-hash-table)))
    (setf (gethash "id" response-arguments) *id*

          (gethash "t" response-dict) (gethash "t" dict)
          (gethash "y" response-dict) "r"
          (gethash "r" response-dict) response-arguments)
    (send-bencoded-data socket response-dict)))

(defun respond-to-find-node (socket dict node)
  "Responds to a find_node query."
  (let ((response-dict (make-hash-table))
        (response-arguments (make-hash-table)))
    (setf (gethash "id" response-arguments) *id*
          (gethash "nodes" response-arguments)
          (let* ((target (gethash "target" dict))
                 (have-target-p (member target *node-list*
                                        :key #'node-id
                                        :test #'string=)))
            (if have-target-p
                ;; MEMBER returns a list
                (compact-node-info (first have-target-p))
                (with-output-to-string (str)
                  (mapc (lambda (peer) (princ (compact-node-info peer) str))
                        (find-closest-nodes target)))))

          (gethash "t" response-dict) (gethash "t" dict)
          (gethash "y" response-dict) "r"
          (gethash "r" response-dict) response-arguments)
    (send-bencoded-data socket response-dict)))

(defun respond-to-get-peers (socket dict node)
  "Responds to a get_peers query."
  (let* ((arguments-dict (gethash "a" dict))
         (hash (gethash "info_hash" arguments-dict))
         (peers (have-peers hash))
         (response-dict (make-hash-table))
         (response-arguments (make-hash-table)))
    (setf (gethash "id" response-arguments) *id*
          (gethash "token" response-arguments) (invent-token hash node)

          (gethash "t" response-dict) (gethash "t" dict)
          (gethash "y" response-dict) "r"
          (gethash "r" response-dict) response-arguments)
    (if peers
        (setf (gethash "values" response-arguments)
              (mapcar #'compact-peer-info peers))
        (setf (gethash "nodes" response-arguments)
              (with-output-to-string (str)
                (mapc (lambda (node) (princ (compact-node-info node) str))
                      (find-closest-nodes hash)))))
    (send-bencoded-data socket response-dict)))

(defun respond-to-announce-peer (socket dict node source-port)
  "Responds to an announce_peer query. If the received token isn't valid,
sends a protocol error message."
  (let* ((response-dict (make-hash-table))
         (response-arguments (make-hash-table))
         (argument-dict (gethash "a" dict))
         (id (gethash "id" argument-dict))
         (implied-port-p (gethash "implied_port" argument-dict))
         (info-hash (gethash "info_hash" argument-dict))
         (port (if (and implied-port-p (= implied-port-p 1))
                   ;; if implied_port is 1, use the source port
                   source-port
                   ;; otherwise use the supplied port
                   (gethash "port" argument-dict)))
         (token (gethash "token" argument-dict)))
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
  (let ((error-dict (make-hash-table)))
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
    (with-connected-socket
        (target-socket (node-socket node))
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
        (simple-error () (invoke-restart :continue))))))