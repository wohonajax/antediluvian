(in-package #:dhticl)

(defvar *default-port* 6881)

(defvar *use-implied-port-p* nil)

(defun ping-node (client-socket-stream)
  "Sends the node connected to via the socket that CLIENT-SOCKET-STREAM
is linked to a ping query."
  (usocket:with-socket-listener
      (listening-socket nil nil
                        :protocol :datagram
                        :element-type '(unsigned-byte 8)
                        :timeout 5
                        :local-host usocket:*wildcard-host*
                        :local-port *default-port*)
    (usocket:with-connected-socket
        (connection (usocket:socket-accept listening-socket))
      (let ((query-dict (make-hash-table))
            (query-body-dict (make-hash-table)))
        (setf (gethash "id" query-body-dict) +my-id+

              (gethash "t" query-dict) (generate-transaction-id)
              (gethash "y" query-dict) "q"
              (gethash "q" query-dict "ping")
              (gethash "a" query-dict) query-body-dict)
        (bencode:encode query-dict client-socket-stream
                        :external-format '(unsigned-byte 8))
        (force-output client-socket-stream))
      (bencode:decode connection :external-format '(unsigned-byte 8)))))

(defun find-node (client-socket-stream node-id)
  "Sends the node connected to via the socket that CLIENT-SOCKET-STREAM
is linked to a find_node query for NODE-ID."
  (usocket:with-socket-listener
      (listening-socket nil nil
                        :protocol :datagram
                        :element-type '(unsigned-byte 8)
                        :timeout 5
                        :local-host usocket:*wildcard-host*
                        :local-port *default-port*)
    (usocket:with-connected-socket
        (connection (usocket:socket-accept listening-socket))
      (let ((query-dict (make-hash-table))
            (query-body-dict (make-hash-table)))
        (setf (gethash "id" query-body-dict) +my-id+
              (gethash "target" query-body-dict) node-id

              (gethash "t" query-dict) (generate-transaction-id)
              (gethash "y" query-dict) "q"
              (gethash "q" query-dict) "find_node"
              (gethash "a" query-dict) query-body-dict)
        (bencode:encode query-dict client-socket-stream
                        :external-format '(unsigned-byte 8))
        (force-output client-socket-stream))
      (bencode:decode connection :external-format '(unsigned-byte 8)))))

(defun get-peers (client-socket-stream info-hash)
  "Sends the node connected to via the socket that CLIENT-SOCKET-STREAM
is linked to a get_peers query using INFO-HASH."
  (usocket:with-socket-listener
      (listening-socket nil nil
                        :protocol :datagram
                        :element-type '(unsigned-byte 8)
                        :timeout 5
                        :local-host usocket:*wildcard-host*
                        :local-port *default-port*)
    (usocket:with-connected-socket
        (connection (usocket:socket-accept listening-socket))
      (let ((query-dict (make-hash-table))
            (query-body-dict (make-hash-table)))
        (setf (gethash "id" query-body-dict) +my-id+
              (gethash "info_hash" query-body-dict) (ensure-hash info-hash)

              (gethash "t" query-dict) (generate-transaction-id)
              (gethash "y" query-dict) "q"
              (gethash "q" query-dict) "get_peers"
              (gethash "a" query-dict) query-body-dict)
        (bencode:encode query-dict client-socket-stream
                        :external-format '(unsigned-byte 8))
        (force-output client-socket-stream))
      (bencode:decode connection :external-format '(unsigned-byte 8)))))

(defun announce-peer (client-socket-stream info-hash)
  "Sends the node connected to via the socket that CLIENT-SOCKET-STREAM
is linked to an announce_peer query using INFO-HASH."
  (usocket:with-socket-listener
      (listening-socket nil nil
                        :protocol :datagram
                        :element-type '(unsigned-byte 8)
                        :timeout 5
                        :local-host usocket:*wildcard-host*
                        :local-port *default-port*)
    (usocket:with-connected-socket
        (connection (usocket:socket-accept listening-socket))
      (let* ((query-dict (make-hash-table))
             (query-body-dict (make-hash-table))
             (surely-hash (ensure-hash info-hash))
             (token (recall-token sure-hash)))
        (setf (gethash "id" query-body-dict) +my-id+
              (gethash "implied_port" query-body-dict) (if *use-implied-port-p* 1 0)
              (gethash "info_hash" query-body-dict) surely-hash
              (gethash "port" query-body-dict) *default-port*
              (gethash "token" query-body-dict) token

              (gethash "t" query-dict) (generate-transaction-id)
              (gethash "y" query-dict) "q"
              (gethash "q" query-dict) "announce_peer"
              (gethash "a" query-dict) query-body-dict)
        (bencode:encode query-dict client-socket-stream
                        :external-format '(unsigned-byte 8))
        (force-output client-socket-stream))
      (bencode:decode connection :external-format '(unsigned-byte 8)))))

(defun send-message (type node &key info-hash)
  (multiple-value-bind (ip port)
      (parse-node-ip (node-ip node))
    (usocket:with-client-socket
        (target-socket target-stream ip port
                       :protocol :datagram
                       :element-type '(unsigned-byte 8)
                       :timeout 5)
      (handler-case (ecase type
                      (:ping (ping-node target-stream))
                      (:store)
                      (:find_node (find-node target-stream (node-id node)))
                      (:find_value)
                      (:get_peers (get-peers target-stream info-hash))
                      (:announce_peer (announce-peer target-stream info-hash)))
        (simple-error () (invoke-restart :continue))))))

(defun send-response (type))

;;;; Everything below this point is being rewritten

(defmacro define-message (name arglist &body message)
  (with-gensyms (target)
    `(defun ,name ,arglist
       (let ((,target (multiple-value-call (lambda (ip port)
                                             (usocket:socket-connect
                                              ip port :protocol :datagram))
                        (parse-node-ip (node-ip node)))))
         (handler-bind ((simple-error (lambda (c)
                                        (declare (ignore c))
                                        (invoke-restart :continue))))
           (bencode:encode
            (de-bencode ,@message)
            ,target))))))

(defmacro define-query (name torrent &body body)
  `(define-message ,name ,(if torrent (list 'node 'hash) (list 'node))
     (concatenate 'string
                  ;; { "t" : *t-id* ,
                  "d" "1:t" "2:" ,(generate-transaction-id)
                  ;; "y" : "q" ,
                  "1:y" "1:q"
                  ;; "q" : *name*
                  "1:q" (substitute #\_ #\- (string-downcase ',name))
                  ;; "a" : { "id" : *id*
                  "1:a" "d" "2:id20:" +my-id+
                  ;; ((body)) } }
                  ,@body "e" "e")))

(defmacro define-response (name &body body)
  `(define-message ,(symbolicate (string-upcase
                                  (format nil "respond-to-~A" name)))
       ,(list 'node 'query)
     (let* ((dict (de-bencode query))
            (transaction-id (gethash "t" dict)))
       (concatenate 'string
                    ;; { "t" : *t-id* ,
                    "d" "1:t" "2:" transaction-id
                    ;; "y" : "r"
                    "1:y" "1:r"
                    ;; "r" : { "id" : *id* ,
                    "1:r" "d" "2:id" "20:" +my-id+
                    ;; ((body)) } }
                    ,@body "e" "e"))))

(define-message dht-error (node transaction-id type)
  (concatenate 'string
               ;; { "t" : *t-id* ,
               "d" "1:t" "2:" transaction-id
               ;; "y" : "e"
               "1:y" "1:e"
               ;; "e" : [ #, string
               "1:e" "l"
               (case type
                 (:generic (concatenate 'string "i201e"
                                        "13:Generic Error"))
                 (:server (concatenate 'string "i202e"
                                       "12:Server Error"))
                 (:protocol (concatenate 'string "i203e"
                                         "14:Protocol Error"))
                 (:method (concatenate 'string "i204e"
                                       "14:Method Unknown")))
               ;; ] }
               "e" "e"))

(defun format-nodes (list-of-nodes)
  "Formats a list of nodes appropriately for bencoding."
  (with-output-to-string (x)
    (princ "l" x)
    (mapc (lambda (a)
            (princ "26:" x)
            (princ (node-id a) x)
            (princ (node-ip a) x))
          list-of-nodes)
    (princ "e" x)))

(define-response ping nil)

(define-response find-node
  (concatenate 'string "5:nodes"))

(define-response get-peers
  (let* ((hash (gethash "info_hash" (gethash "a" dict)))
         (peers (have-peers hash)))
    (concatenate 'string "5:token" (invent-token hash (node-ip node))
                 (if peers
                     (concatenate 'string "6:values" (format-nodes peers))
                     (concatenate 'string "5:nodes"
                                  (format-nodes (find-closest-nodes
                                                 (node-id node))))))))

(define-response announce-peer nil)
