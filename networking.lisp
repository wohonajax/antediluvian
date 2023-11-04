(in-package #:dhticl)

(defvar *transactions* (make-hash-table :test #'equal))

(defun calculate-elapsed-inactivity (node)
  "Returns the time in minutes since NODE's last seen activity."
  (and (node-last-activity node) (minutes-since last-activity)))

(defun calculate-node-health (node)
  "Returns the node's health as a keyword, either :GOOD, :QUESTIONABLE,
or :BAD."
  (let ((time-inactive (calculate-elapsed-inactivity node)))
    (cond ((null time-inactive) :questionable)
          ((< time-inactive 15) :good)
          ((ping-then-listen node) :good)
          (t :bad))))

(defun ping-old-nodes (bucket)
  "Pings the nodes in a bucket from oldest to newest."
  (sort-bucket-by-age bucket)
  (iterate-bucket bucket
                  (lambda (node)
                    (send-message :ping (node-ip node) (node-port node))))
  (sort-bucket-by-distance bucket)
  (update-bucket bucket))

(defun purge-bad-nodes (bucket)
  "Removes all nodes of bad health from BUCKET."
  (map-into (bucket-nodes bucket)
            (lambda (node)
              (unless (eql :bad (node-health node))
                node))
            (bucket-nodes bucket))
  (sort-bucket-by-distance bucket)
  (update-bucket bucket))
;;; FIXME
(defun handle-questionable-node (node)
  "Checks the health of NODE."
  (setf (node-health node)
        (cond ((ping-then-listen node) :good)
              ((ping-then-listen node) :good)
              (t :bad)))
  (update-bucket (correct-bucket (node-id node))))

(defun handle-questionable-nodes (bucket)
  "Handles all nodes in BUCKET that are of questionable health."
  (iterate-bucket bucket
                  (lambda (node)
                    (when (eql :questionable (node-health node))
                      (handle-questionable-node node))))
  (update-bucket bucket))

(defun parse-query (dict ip port)
  "Parses a Bencoded query dictionary."
  (let* ((now (get-universal-time))
         (arguments (gethash "a" dict))
         (id (gethash "id" arguments))
         (distance (calculate-distance (convert-id-to-int +my-id+)
                                       (convert-id-to-int id)))
         (maybe-node (find-node-in-table id)))
    (if maybe-node
        (setf (node-last-activity maybe-node) now
              (node-health maybe-node) :good)
        (let ((node (create-node :id id :ip ip :port port
                                 :distance distance
                                 :last-activity now
                                 :health :good)))
          (push node *node-list*)
          (add-to-bucket node)))
    (alexandria:switch ((gethash "q" dict) :test #'string=)
      ("ping" (send-response :ping node dict))
      ("find_node" (send-response :find_node node dict))
      ("get_peers" (send-response :get_peers node dict))
      ("announce_peer" (send-response :announce_peer node dict port)))))

(defun parse-response (dict ip port)
  "Parses a Bencoded response dictionary."
  (flet ((parse-nodes (str)
           (let (nodes
                 (len (/ (length str) 6)))
             (handler-case
                 (dotimes (i len nodes)
                   (let ((index (* i 6)))
                     (multiple-value-bind (parsed-ip parsed-port)
                         (parse-node-ip (subseq str index (+ index 6)))
                       (push (cons parsed-ip parsed-port) nodes))))
               ;; TODO: error handling for out-of-bounds
               ;; (node's health is bad)
               (error () (return-from parse-nodes nodes)))))
         (ping-nodes (node-list)
           (mapc (lambda (pair) (send-message :ping (car pair) (cdr pair)))
                 node-list)))
    (let* ((transaction-id (gethash "t" dict))
           (arguments (gethash "a" dict))
           (id (gethash "id" arguments))
           ;; TOKEN comes from a get_peers response, needed for announce_peer
           (token (gethash "token" arguments))
           ;; NODES comes from a find_node or get_peers response
           (nodes (gethash "nodes" arguments))
           ;; VALUES is a list of strings which are compact node info
           ;; Comes from a get_peers response
           (values (gethash "values" arguments))
           (implied-port (gethash "implied_port" arguments))
           (peer-port (gethash "port" arguments))
           (node (car (member id *node-list* :key #'node-id :test #'string=))))
      ;; handle bookkeeping of the node
      (if node
          (progn (setf (node-last-activity node) (get-universal-time)
                       (node-health node) :good)
                 (pushnew (gethash "info_hash" arguments)
                          (node-hashes node)
                          :test #'equalp)
                 (cond ((and implied-port (= implied-port 1))
                        (setf (node-port node) port))
                       (peer-port (setf (node-port node) peer-port))))
          (push (create-node :id id :ip ip :port port
                             :distance
                             (calculate-distance (convert-id-to-int id)
                                                 (convert-id-to-int +my-id+))
                             :last-activity (get-universal-time)
                             :health :good)
                *node-list*))
      (when nodes
        (ping-nodes (parse-nodes nodes)))
      (when values
        (ping-nodes (parse-nodes values)))
      ;; TODO: associate with info-hash instead of with node
      (when token ;; CURRENT-INFO-HASH-VALUE will be (node . token)
        (let ((current-info-hash-value (gethash (gethash "info_hash" arguments)
                                                *hashmap*)))
          (unless (equalp (cdr current-info-hash-value) token)
            (setf (gethash (gethash "info_hash" arguments) *hashmap*)
                  (cons node token))))))))
