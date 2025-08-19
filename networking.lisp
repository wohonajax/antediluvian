;;;; Code related to communicating
;;;; with nodes over the network

(in-package #:dhticl)

(defvar *listening-socket*)

(defvar *results-list* (list)
  "A list containing nodes received from find_node queries.")

(defvar *previous-best-results* (list))

(defvar *best-results* (list)
  "A list containing the k best results from a find_node lookup.")

(defvar *active-lookups* (make-hash-table :test #'equal)
  "A hash table containing currently active lookups.")

(defconstant +alpha+ 3
  "The number of simultaneous lookups to perform.")

(defun ping-old-nodes (bucket)
  "Pings the nodes in a bucket from oldest to newest."
  (sort-bucket-by-age bucket)
  (iterate-bucket bucket
                  (lambda (node)
                    (send-message :ping (node-ip node) (node-port node)
                                  (generate-transaction-id))))
  (update-bucket bucket)
  (sort-bucket-by-distance bucket *my-id*))

(defun purge-bad-nodes (bucket)
  "Removes all nodes of bad health from BUCKET."
  (map-into (bucket-nodes bucket)
            (lambda (node)
              (unless (eql :bad (node-health node))
                node))
            (bucket-nodes bucket))
  (update-bucket bucket)
  (sort-bucket-by-distance bucket *my-id*))
;;; TODO: can this be done better?
(defun handle-questionable-node (node)
  "Checks the health of NODE."
  (send-message :ping (node-ip node) (node-port node)
                (generate-transaction-id)))

(defun handle-questionable-nodes (bucket)
  "Handles all nodes in BUCKET that are of questionable health."
  (iterate-bucket bucket
                  (lambda (node)
                    (when (eql :questionable (node-health node))
                      (handle-questionable-node node))))
  (update-bucket bucket))

(defun ping-oldest-node (bucket)
  "Pings the oldest node in BUCKET."
  (sort-bucket-by-age bucket)
  (let ((node (svref bucket 0)))
    (send-message :ping (node-ip node) (node-port node)
                  (generate-transaction-id)))
  (sort-bucket-by-distance bucket *my-id*))

(defun lookup (node)
  "Begins a lookup of NODE."
  (let ((transaction-id (generate-transaction-id)))
    (send-message :ping (node-ip node) (node-port node) transaction-id)
    (setf (gethash transaction-id *active-lookups*) node)))

(defun ping-results! ()
  "Begins looking up nodes in the intermediary results list."
  (loop for node in *results-list*
        while (< (hash-table-count *active-lookups*) +alpha+)
        do (lookup node)
           (pop *results-list*)))

(defun push-to-best-results (target)
  (insert node *best-results* #'<
          :key (lambda (node)
                 (calculate-node-distance node target))))

(defun parse-query (dict ip port)
  "Parses a Bencoded query dictionary."
  (let* ((now (get-universal-time))
         (arguments (gethash "a" dict))
         (id (gethash "id" arguments))
         (info-hash (gethash "info_hash" arguments))
         (token (gethash "token" arguments))
         (node (find-node-in-table id)))
    (if node
        (setf (node-last-activity node) now
              (node-health node) :good)
        (progn (setf node (create-node :id id :ip ip :port port
                                       :last-activity now
                                       :health :good))
               (push node *node-list*)
               (add-to-bucket node)
               (setf (gethash (gethash "t" dict) *transactions*) t)))
    (alexandria:switch ((gethash "q" dict) :test #'string=)
      ("ping" (send-response :ping node dict))
      ("find_node" (send-response :find_node node dict))
      ("get_peers" (send-response :get_peers node dict))
      ("announce_peer" (cond ((member token (recall-token info-hash)
                                      :test #'equalp)
                              (push node (gethash info-hash *peer-list*))
                              (send-response :announce_peer node dict
                                             :source-port port))
                             (t (send-response :dht_error node dict
                                               :error-type :protocol)))))))

(defun parse-nodes (string)
  "Parses a list of nodes out of a string of compact node info substrings."
  (let (nodes)
    (handler-case
        (do ((i 0 (+ i 26))) ; operate on compact node info substrings
            ((= i (length string)) nodes)
          (multiple-value-bind (parsed-ip parsed-port)
              (parse-node-ip (subseq string (+ i 20) (+ i 26)))
            ;; (list node-id node-ip node-port)
            (push (list (subseq string i (+ i 20)) parsed-ip parsed-port)
                  nodes)))
      ;; TODO: error handling for out-of-bounds
      ;; (node's health may be bad--malformed response sent?)
      (error () (return-from parse-nodes nodes)))))

(defun parse-peers (string)
  "Parses a list of peers out of a string of compact peer info substrings."
  (let (peers)
    (handler-case
        (do ((i 0 (+ i 6))) ; operate on compact peer info substrings
            ((= i (length string)) peers)
          (multiple-value-bind (parsed-ip parsed-port)
              (parse-node-ip (subseq string i (+ i 6)))
            (push (cons parsed-ip parsed-port) peers)))
      ;; TODO: error handling for out-of-bounds
      ;; (node's health may be bad--malformed response sent?)
      (error () (return-from parse-peers peers)))))

(defun handle-node-bookkeeping (node time implied-port peer-port id ip port)
  "Either adjust NODE's settings or create a node based on those settings.
Returns the node object."
  (cond (node (setf (node-last-activity node) time
                    (node-health node) :good)
              (cond ((and implied-port (= implied-port 1))
                     (setf (node-port node) port))
                    (peer-port (setf (node-port node) peer-port))))
        (t (setf node (create-node :id id :ip ip :port port
                                   :last-activity time
                                   :health :good))
           (add-to-bucket node)))
  node)

(defun handle-nodes-response (nodes)
  "Handle a nodes response from a find_node or get_peers query by pinging every
node in the response."
  (when nodes
    (loop with node-list = (parse-nodes nodes)
          for (node-id node-ip node-port) in node-list
          for node = (create-node :id node-id :ip node-ip :port node-port)
          do (push node *results-list*))
    (ping-results!)))

(defun handle-values-response (peers)
  "Handle a list of peers that have been searched for."
  ;; TODO: add to *PEER-LIST*
  (when peers
    (loop with peer-list = (parse-peers peers)
          for (peer-ip . peer-port) in peer-list
          do (send-message :ping peer-ip peer-port
                           (generate-transaction-id)))))

(defun handle-lookup-response (transaction-id node target)
  "Handles a find_node response. Recursively calls find_node until the best
results are the same as the previous best results."
  (when (gethash transaction-id *active-lookups*)
    (cond ((= (length *best-results*) +k+)
           ;; we only want the k closest nodes
           ;; TODO: use insertion sort to avoid calling SORT on the whole list
           (unless (< (node-distance (first *best-results*))
                      (node-distance node))
             (push-to-best-results node)))
          (t (push-to-best-results node)))
    (remhash transaction-id *active-lookups*)
    (cond (*results-list* ; if *RESULTS-LIST* isn't empty
           (lookup (first *results-list*))
           (pop *results-list*))
          ((equalp *best-results* *previous-best-results*)) ; stop recursion
          (t (setf *previous-best-results* *best-results*)
             (mapc (lambda (node)
                     (send-message :find_node
                                   (node-ip node)
                                   (node-port node)
                                   (generate-transaction-id)))
                   *best-results*)))))

(defun store-received-token (token time transaction-id node)
  "Handles bookkeeping for a given TOKEN."
  (when token
    (setf (gethash token *token-births*) time

          (gethash (gethash transaction-id *transactions*) ; info_hash
                   *token-hashes*)
          token

          (gethash token *token-nodes*) node)))

(defun parse-response (dict ip port)
  "Parses a Bencoded response dictionary."
  (let* ((now (get-universal-time))
         (transaction-id (gethash "t" dict))
         (arguments (gethash "a" dict))
         (id (gethash "id" arguments))
         (info-hash (gethash "info_hash" arguments))
         ;; TOKEN comes from a get_peers response, needed for announce_peer
         (token (gethash "token" arguments))
         ;; NODES comes from a find_node or get_peers response
         (nodes (gethash "nodes" arguments))
         ;; VALUES is a list of strings which are compact peer info
         ;; Comes from a get_peers response
         (values (gethash "values" arguments))
         (implied-port (gethash "implied_port" arguments))
         (peer-port (gethash "port" arguments))
         (node (first (member id *node-list* :key #'node-id :test #'string=))))
    (setf node
          (handle-node-bookkeeping node now implied-port peer-port id ip port))
    ;; bad transaction ID
    (unless (gethash transaction-id *transactions*)
      (send-response :dht_error node dict :error-type :protocol)
      (setf (node-health node) :bad)
      (return-from parse-response))
    (handle-nodes-response nodes)
    (handle-values-response values)
    (handle-lookup-response transaction-id node info-hash)
    (store-received-token token now transaction-id node)
    (remhash transaction-id *transactions*)))

(defun parse-message ()
  "Parses a KRPC message."
  (multiple-value-bind (data size host port)
      (receive-data)
    (let* ((packet (subseq data 0 size))
           (dict (bencode:decode packet)))
      (alexandria:eswitch ((gethash "y" dict) :test #'string=)
        ("q" (parse-query dict host port))
        ("r" (parse-response dict host port))
        ("e" ;; TODO: handle errors
         )))))