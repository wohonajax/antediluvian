;;;; Code related to communicating
;;;; with nodes over the network

(in-package #:dhticl)

(defvar *listening-dht-socket*
        (socket-connect nil nil
                        :protocol :datagram
                        :local-port *default-port*)
  "A UDP socket listening on *DEFAULT-PORT* for DHT communication.")

(defvar *replacement-cache* (list)
  "A list of nodes to potentially add to the routing table, should a bucket
contain nodes that go stale.")

(defvar *replacement-candidates* (make-hash-table :test #'equalp)
  "A hash table mapping transaction IDs to a cons whose car is a promise and
whose cdr is the node to add to the bucket.")

(defun receive-data ()
  "Receive data from the listening socket."
  (socket-receive *listening-socket* nil +max-datagram-packet-size+))

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

(defun node-replacement-check (deletion-candidate replacement-candidate)
  "Initiates a check for whether to replace DELETION-CANDIDATE with
REPLACEMENT-CANDIDATE."
  (let ((transaction-id (generate-transaction-id))
        (promise (promise)))
    (setf (gethash transaction-id *replacement-candidates*)
          (cons promise replacement-candidate))
    ;; if the promise isn't fulfilled after 10 seconds, consider it failed
    (make-thread (lambda () (sleep 10) (fulfill promise 'timeout)))
    (send-message :ping (node-ip deletion-candidate)
                  (node-port deletion-candidate)
                  transaction-id)))

(defun maybe-add-to-table (node)
  "Adds NODE to the routing table if there's an empty slot in the appropriate
bucket, otherwise checks whether to replace the least-recently-active node in
that bucket."
  (let ((bucket (correct-bucket node)))
    (sort-bucket-by-age bucket)
    (when-let (empty-slot-index (first-empty-slot bucket))
      (setf (svref bucket empty-slot-index) node)
      ;; we've added the node to the bucket; we're done
      (return-from maybe-add-to-table))
    ;; check whether to replace the least-recently-active
    ;; node in the bucket with the new node we're handling
    (node-replacement-check (svref bucket 0) node)))

(defun bucket-split-candidate-p (node bucket)
  "Tests whether BUCKET fits the criteria for being split or not. In order to
be split, a bucket's range must encapsulate both the node's ID and our ID, and
NODE must be closer to us than the kth closest node in the routing table."
  (when (first-empty-slot bucket)
    ;; bucket isn't full; don't split it
    (return-from bucket-split-candidate-p))
  (labels ((node-distance-from-us (node)
             (calculate-node-distance node *id*))
           (kth-closest-node-to-us ()
             (extremum (find-closest-nodes *id*) #'>
                       :key #'node-distance-from-us)))
    (let ((id (node-id node))
          (lower-bound (bucket-min bucket))
          (upper-bound (bucket-max bucket)))
      (and (within (convert-id-to-int id) lower-bound upper-bound)
           (within (convert-id-to-int *id*) lower-bound upper-bound)
           (< (calculate-distance id *id*)
              (node-distance-from-us (kth-closest-node-to-us)))))))

(defun maybe-add-node (node)
  "Does nothing if NODE is already in the routing table. If it isn't, splits
the bucket NODE fits into if it's a candidate for splitting. Initiates the
procedure for potentially adding a node to a bucket."
  (let ((bucket (correct-bucket node)))
    (when (contains node bucket :test #'eq)
      (return-from maybe-add-node))
    (when (bucket-split-candidate-p node bucket)
      (split-bucket bucket))
    (maybe-add-to-table node)))

(defun check-replacement-candidates ()
  "Checks whether to replace potentially stale nodes with replacement
candidates or add those candidates to the replacement cache."
  (maphash (lambda (transaction-id promise-node-cons)
             (let ((promise (car promise-node-cons))
                   (node (cdr promise-node-cons)))
               (when (fulfilledp promise)
                 (case (force promise)
                   (timeout (setf (svref (correct-bucket node) 0) node))
                   (response (push node *replacement-cache*)))
                 (remhash transaction-id *replacement-candidates*))))
           *replacement-candidates*))
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

(defun handle-node-bookkeeping (node time implied-port peer-port id ip port)
  "Either adjust NODE's settings or create a node based on those settings,
then tries to add it to the routing table. Returns the node object."
  (cond (node (setf (node-last-activity node) time
                    (node-health node) :good)
              ;; when implied_port is 1, use the source port of the UDP packet
              (cond ((and implied-port (= implied-port 1))
                     (setf (node-port node) port))
                    (peer-port (setf (node-port node) peer-port))))
        (t (setf node (create-node :id id :ip ip :port port
                                   :last-activity time
                                   :health :good))))
  (maybe-add-node node)
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
  (ping-lookup-results))

(defun handle-values-response (peers)
  "Handle a list of peers that have been searched for."
  ;; TODO: add to *PEER-LIST*
  (loop with peer-list = (parse-peers peers)
        for (peer-ip . peer-port) in peer-list
        do (send-message :ping peer-ip peer-port
                         (generate-transaction-id))))

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
    (check-replacement-candidates)
    ;; if this is a response to a test for a candidate's replacement,
    ;; fulfill the promise/indicate that we got a response
    (when-let (promise-node-cons (gethash transaction-id
                                          *replacement-candidates*))
      (fulfill (car promise-node-cons) 'response))
    (when nodes
      (handle-nodes-response nodes))
    (when values
      (handle-values-response values))
    ;; FIXME: *active-lookups* currently maps transaction IDs to node objects.
    ;; we need to maintain the target of our lookups, which will be a node ID
    (when-let* ((info-hash (gethash transaction-id *transactions*))
                (info-hash-p (not (eql t info-hash))))
      (handle-lookup-response transaction-id node info-hash))
    (when token
      (store-received-token token now transaction-id node))
    (remhash transaction-id *transactions*)))

(defun binary-key-test (keys)
  "Tests whether KEYS suits decoding as a byte-vector rather than a string."
  (or (equal keys '("t"))
      (equal keys '("id" "a"))
      (equal keys '("target" "a"))
      (equal keys '("nodes" "a"))
      (equal keys '("values" "a"))
      (equal keys '("info_hash" "a"))
      (equal keys '("token" "a"))))

(defun parse-message ()
  "Parses a KRPC message."
  (multiple-value-bind (data size host port)
      (receive-data)
    (let* ((packet (subseq data 0 size))
           (*binary-key-p* #'binary-key-test)
           (dict (decode packet)))
      (eswitch ((gethash "y" dict) :test #'string=)
        ("q" (parse-query dict host port))
        ("r" (parse-response dict host port))
        ("e" ;; TODO: handle errors
         )))))