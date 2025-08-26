;;;; Code related to communicating
;;;; with nodes over the network

(in-package #:dhticl)

(defvar *listening-socket*
        (socket-connect nil nil
                        :protocol :datagram
                        :local-port *default-port*)
  "A UDP socket listening on *DEFAULT-PORT*.")

(defvar *results-list* (list)
  "A list containing nodes received from find_node queries.")

(defvar *previous-best-results* (list))

(defvar *best-results* (list)
  "A list containing the k best results from a find_node lookup.")

(defvar *active-lookups* (make-hash-table :test #'equal)
  "A hash table containing currently active lookups.")

(defconstant +alpha+ 3
  "The number of simultaneous lookups to perform.")

(defun receive-data ()
  "Receive data from the listening socket."
  (socket-receive
   *listening-socket*
   nil ;; FIXME: this just hangs waiting for a huge response
   +max-datagram-packet-size+))

(defun generate-transaction-id ()
  "Creates a transaction ID and returns it as a byte-vector."
  (random-data 2))

(defun ping-old-nodes (bucket)
  "Pings the nodes in a bucket from oldest to newest."
  (sort-bucket-by-age bucket)
  (iterate-bucket bucket
                  (lambda (node)
                    (send-message :ping (node-ip node) (node-port node)
                                  (generate-transaction-id)))))

(defmacro replace-bucket (bucket test)
  "Replaces nodes in BUCKET with NIL when TEST isn't satisfied. The variable
NODE is bound in the test form."
  `(progn (map-into (bucket-nodes ,bucket)
                    (lambda (node)
                      (when node
                        (if ,test
                            (progn (kill-node node)
                                   nil)
                            node)))
                    (bucket-nodes ,bucket))
          (sort-bucket-by-age ,bucket)))

(defun purge-stale-nodes (bucket)
  "Removes all stale nodes from BUCKET."
  (replace-bucket bucket (node-stale-p node)))

(defun purge-bad-nodes (bucket)
  "Removes all nodes of bad health from BUCKET and from the list of nodes."
  (replace-bucket bucket (eql :bad (node-health node))))
;;; TODO: can this be done better?
(defun handle-questionable-node (node)
  "Checks the health of NODE."
  (let ((inactivity (calculate-elapsed-inactivity node)))
    (when (and inactivity (> inactivity 15))
      (setf (node-health node) :bad)
      (return-from handle-questionable-node)))
  (send-message :ping (node-ip node) (node-port node)
                (generate-transaction-id)))

(defun handle-questionable-nodes (bucket)
  "Handles all nodes in BUCKET that are of questionable health."
  (iterate-bucket bucket
                  (lambda (node)
                    (when (eql :questionable (node-health node))
                      (handle-questionable-node node)))))

(defun ping-oldest-node (bucket)
  "Pings the oldest node in BUCKET."
  (sort-bucket-by-age bucket)
  (let ((node (svref bucket 0)))
    (send-message :ping (node-ip node) (node-port node)
                  (generate-transaction-id))))

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

(defun push-to-best-results (node target)
  (insert node *best-results* #'<
          :key (lambda (node)
                 (calculate-node-distance node target))))

(defun handle-node-bookkeeping (node time implied-port peer-port id ip port)
  "Either adjust NODE's settings or create a node based on those settings.
Returns the node object."
  (cond (node (setf (node-last-activity node) time
                    (node-health node) :good)
              ;; when implied_port is 1, use the source port of the UDP packet
              (cond ((and implied-port (= implied-port 1))
                     (setf (node-port node) port))
                    (peer-port (setf (node-port node) peer-port))))
        (t (setf node (create-node :id id :ip ip :port port
                                   :last-activity time
                                   :health :good))
           (add-to-bucket node)))
  node)

(defun store-received-token (token time transaction-id node)
  "Handles bookkeeping for a given TOKEN."
  (let ((info-hash (gethash transaction-id *transactions*)))
    (setf (gethash token *token-births*) time

          (gethash info-hash *token-hashes*) token
      
          (gethash (node-ip node) *token-ips*) token)))

(defun parse-query (dict ip port)
  "Parses a Bencoded query dictionary."
  (let* ((now (get-universal-time))
         (transaction-id (gethash "t" dict))
         (arguments (gethash "a" dict))
         (id (gethash "id" arguments))
         (info-hash (gethash "info_hash" arguments))
         (token (gethash "token" arguments))
         (implied-port (gethash "implied_port" arguments))
         (peer-port (gethash "port" arguments))
         (node (find-node-in-table id)))
    (setf (gethash transaction-id *transactions*) (or info-hash t))
    (setf node
          (handle-node-bookkeeping node now implied-port peer-port id ip port))
    (when token
      (store-received-token token now transaction-id node))
    (switch ((gethash "q" dict) :test #'string=)
      ("ping" (send-response :ping node dict))
      ("find_node" (send-response :find_node node dict))
      ("get_peers" (send-response :get_peers node dict))
      ("announce_peer" (cond ((member token (recall-tokens info-hash)
                                      :test #'equalp)
                              (push node (gethash info-hash *peer-list*))
                              (send-response :announce_peer node dict
                                             :source-port port))
                             (t (send-response :dht_error node dict
                                               :error-type :protocol)))))))

(defun parse-nodes (byte-vector)
  "Parses a list of nodes out of a byte-vector of compact node info
substrings."
  (let (nodes)
    (handler-case
        (do ((i 0 (+ i 26))) ; operate on compact node info "substrings"
            ((= i (length byte-vector)) nodes)
          (multiple-value-bind (parsed-ip parsed-port)
              (parse-node-ip (subseq byte-vector (+ i 20) (+ i 26)))
            ;; (list node-id node-ip node-port)
            (push (list (subseq byte-vector i (+ i 20)) parsed-ip parsed-port)
                  nodes)))
      ;; TODO: error handling for out-of-bounds
      ;; (node's health may be bad--malformed response sent?)
      (error () (return-from parse-nodes nodes)))))

(defun parse-peers (peers-list)
  "Parses a list of peers out of a list of compact peer info byte-vectors."
  (handler-case
      (mapcar (lambda (byte-vector)
                (multiple-value-call #'cons (parse-node-ip byte-vector)))
              peers-list)
    ;; TODO: error handling for out-of-bounds
    ;; (node's health may be bad--malformed response sent?)
    (error () (return-from parse-peers))))

(defun handle-nodes-response (nodes)
  "Handle a nodes response from a find_node or get_peers query by pinging every
node in the response."
  (loop with node-list = (parse-nodes nodes)
        for (node-id node-ip node-port) in node-list
        for node = (create-node :id node-id :ip node-ip :port node-port)
        do (push node *results-list*))
  (ping-results!))

(defun handle-values-response (peers)
  "Handle a list of peers that have been searched for."
  ;; TODO: add to *PEER-LIST*
  (loop with peer-list = (parse-peers peers)
        for (peer-ip . peer-port) in peer-list
        do (send-message :ping peer-ip peer-port
                         (generate-transaction-id))))

(defun handle-lookup-response (transaction-id node target)
  "Handles a find_node response. Recursively calls find_node until the best
results are the same as the previous best results."
  (when (gethash transaction-id *active-lookups*)
    (cond ((= (length *best-results*) +k+)
           ;; we only want the k closest nodes
           ;; TODO: use insertion sort to avoid calling SORT on the whole list
           (unless (< (calculate-node-distance (first *best-results*) target)
                      (calculate-node-distance node target))
             (push-to-best-results node target)
             ;; TODO: handle this in PUSH-TO-BEST-RESULTS?
             (setf *best-results* (subseq *best-results* 0 +k+))))
          (t (push-to-best-results node target)))
    (remhash transaction-id *active-lookups*)
    (cond (*results-list* ; if *RESULTS-LIST* isn't empty
           (lookup (first *results-list*))
           (pop *results-list*))
          ((equalp *best-results* *previous-best-results*)) ; stop recursion
          ;; FIXME: make sure we receive the K closest best results
          (t (setf *previous-best-results* *best-results*)
             (mapc (lambda (node)
                     (send-message :find_node
                                   (node-ip node)
                                   (node-port node)
                                   (generate-transaction-id)
                                   :id target))
                   *best-results*)))))

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
         (node (find-node-in-table id)))
    ;; no implied_port or port arguments in responses so they're both nil here
    (setf node (handle-node-bookkeeping node now nil nil id ip port))
    ;; bad transaction ID
    (unless (gethash transaction-id *transactions*)
      (send-response :dht_error node dict :error-type :protocol)
      (setf (node-health node) :bad)
      (return-from parse-response))
    ;; we're getting a response, so the node isn't stale
    (setf (node-failed-rpcs node) 0)
    (when nodes
      (handle-nodes-response nodes))
    (when values
      (handle-values-response values))
    (when info-hash
      (handle-lookup-response transaction-id node info-hash))
    (when token
      (store-received-token token now transaction-id node))
    (remhash transaction-id *transactions*)))

(defun parse-message ()
  "Parses a KRPC message."
  (multiple-value-bind (data size host port)
      (receive-data)
    (let* ((packet (subseq data 0 size))
           (dict (bencode:decode packet)))
      (eswitch ((gethash "y" dict) :test #'string=)
        ("q" (parse-query dict host port))
        ("r" (parse-response dict host port))
        ("e" ;; TODO: handle errors
         )))))