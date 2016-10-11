(in-package #:dhticl)

(defvar *default-port* 6881)

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

(define-query ping nil nil)

(define-query find-node nil
  (concatenate 'string "6:target" "20:" (node-id node)))

(define-query get-peers t
  (concatenate 'string "9:info_hash" "20:" (ensure-hash hash)))

(define-query announce-peer t
  (let* ((sure-hash (ensure-hash hash))
         (token (recall-token sure-hash)))
    (concatenate 'string
                 "9:info_hash" "20:" sure-hash
                 "4:port" *default-port*
                 "5:token" (format nil "~D" (length token)) ":" token)))

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
