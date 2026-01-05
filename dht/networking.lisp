;;;; Code related to communicating
;;;; with nodes over the network

(in-package #:antediluvian)

(defun receive-data ()
  "Receive data from the listening socket."
  (socket-receive *listening-dht-socket* nil +max-datagram-packet-size+))

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
                        (if ,test nil node)))
                    (bucket-nodes ,bucket))
          (sort-bucket-by-age ,bucket)))

(defun purge-stale-nodes (bucket)
  "Removes all stale nodes from BUCKET."
  (replace-bucket bucket (node-stale-p node)))

(defun purge-bad-nodes (bucket)
  "Removes all nodes of bad health from BUCKET and from the list of nodes."
  (replace-bucket bucket (eq (node-health node) :bad)))
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
                    (when (eq (node-health node) :questionable)
                      (handle-questionable-node node)))))

(defun ping-oldest-node (bucket)
  "Pings the oldest node in BUCKET."
  (let ((node (oldest-node-in-bucket bucket)))
    (send-message :ping (node-ip node) (node-port node)
                  (generate-transaction-id))))

(defun set-node-port (node implied-port given-port port)
  "Sets NODE's port slot based on the potential IMPLIED-PORT setting. A value
of NIL or 0 means to use the GIVEN-PORT. A value of 1 means to use the source
PORT of the UDP packet."
  (setf (node-port node)
        (cond ((and implied-port (= implied-port 1))
               port)
              (given-port given-port)
              ;; nothing else to work with
              ;; use the originating port
              (t port))))

(defun handle-node-bookkeeping (node time implied-port given-port id ip port)
  "Either adjusts NODE's settings or creates a node based on those settings,
then tries to add it to the routing table. Returns the node object."
  (if node
      (setf (node-last-activity node) time
            (node-health node) :good)
      (setf node (create-node :id id :ip ip :port port
                              :last-activity time
                              :health :good)))
  (set-node-port node implied-port given-port port)
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
         (given-port (gethash "port" arguments))
         (node (find-node-in-table id)))
    (setf (gethash transaction-id *transactions*) (or info-hash t))
    (setf node
          (handle-node-bookkeeping node now implied-port given-port id ip port))
    (when token
      (store-received-token token now transaction-id node))
    (switch ((gethash "q" dict) :test #'string=)
      ("ping" (send-response :ping node dict))
      ("find_node" (send-response :find_node node dict))
      ("get_peers" (send-response :get_peers node dict))
      ("announce_peer"
       (cond ((member token (recall-tokens info-hash) :test #'equalp)
              (when (member info-hash *torrents* :key #'torrent-info-hash :test #'equalp)
                (let ((peer (make-peer ip port info-hash)))
                  (with-lock-held (*peer-list-lock*)
                    (unless (member (peer-ip peer) *peer-list* :key #'peer-ip :test #'equalp)
                      (push peer *peer-list*)
                      (initiate-peer-connection peer)))))
              (send-response :announce_peer node dict :source-port port))
             (t (send-response :dht_error node dict :error-type :protocol)))))))

(defun parse-nodes (byte-vector)
  "Parses a list of nodes out of a byte vector of compact node info
substrings."
  (let (nodes)
    (handler-case
        (do ((i 0 (+ i 26))) ; operate on compact node info "substrings"
            ((= i (length byte-vector)) nodes)
          (multiple-value-bind (parsed-ip parsed-port)
              (parse-node-ip (subseq byte-vector (+ i 20) (+ i 26)))
            ;; (list node-id node-ip node-port)
            (push (create-node :id (subseq byte-vector i (+ i 20))
                               :ip parsed-ip :port parsed-port)
                  nodes)))
      ;; TODO: error handling for out-of-bounds
      ;; (node's health may be bad--malformed response sent?)
      ;; also possibly sending IPv6 addresses
      (error () (return-from parse-nodes nodes)))
    nodes))

(defun parse-peers (peers-list)
  "Parses a list of peers out of a list of compact peer info byte vectors."
  (let (parsed-peers)
    (handler-case
        (mapc (lambda (byte-vector)
                (push (multiple-value-call #'cons (parse-node-ip byte-vector))
                      parsed-peers))
              peers-list)
      ;; TODO: error handling for out-of-bounds
      ;; (node's health may be bad--malformed response sent?)
      ;; also possibly sending IPv6 addresses
      (error () (return-from parse-peers parsed-peers)))
    parsed-peers))

(defun handle-nodes-response (nodes target)
  "Handle a nodes response from a find_node or get_peers query by pinging every
node in the response."
  (loop for node in (parse-nodes nodes)
        do (push node (gethash target *lookup-results-lists*)))
  ;; don't recurse until all lookups for the target have returned
  (unless (gethash target *active-lookups*)
    (recurse-on-lookup-results target)))

(defun handle-values-response (peers target)
  "Handle a list of peers that have been searched for."
  (with-lock-held (*peer-list-lock*)
    (loop for (ip . port) in (parse-peers peers)
          for peer = (make-peer ip port target)
          ;; TODO: remove duplicate peers when there's
          ;; a peer with a failed socket connection
          unless (member ip *peer-list* :key #'peer-ip :test #'equalp)
            do (push peer *peer-list*)
               (initiate-peer-connection peer))))

(defun parse-response (dict ip port)
  "Parses a Bencoded response dictionary."
  (let* ((now (get-universal-time))
         (transaction-id (gethash "t" dict))
         (response-arguments (gethash "r" dict))
         (id (gethash "id" response-arguments))
         ;; TOKEN comes from a get_peers response, needed for announce_peer
         (token (gethash "token" response-arguments))
         ;; NODES comes from a find_node or get_peers response
         (nodes (gethash "nodes" response-arguments))
         ;; VALUES is a list of byte vectors which are compact peer info
         ;; Comes from a get_peers response
         (values (gethash "values" response-arguments))
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
    (when-let (promise-node-cons (gethash transaction-id *replacement-candidates*))
      (fulfill (car promise-node-cons) 'response))
    (when-let* ((target (gethash transaction-id *transactions*))
                ;; target is an info hash or node ID
                (targetp (not (eql target t))))
      (when nodes
        (handle-nodes-response nodes target))
      (when values
        (handle-values-response values target))
      (handle-lookup-response transaction-id target))
    (when token
      (store-received-token token now transaction-id node))
    (remhash transaction-id *transactions*)))

(defun binary-key-test (keys)
  "Tests whether KEYS suits decoding as a byte vector rather than a string."
  (or (equal keys '("t"))
      (equal keys '("id" "a"))
      (equal keys '("target" "a"))
      (equal keys '("info_hash" "a"))
      (equal keys '("token" "a"))
      (equal keys '("id" "r"))
      (equal keys '("token" "r"))
      (equal keys '("nodes" "r"))
      (equal keys '("values" "r"))
      ;; extensions
      (equal keys '("ip"))
      (equal keys '("v"))))

(defun parse-message ()
  "Parses a KRPC message."
  (multiple-value-bind (data size host port)
      (receive-data)
    (let* ((packet (subseq data 0 size))
           (bencode:*binary-key-p* #'binary-key-test)
           ;; we need this handler-bind
           ;; because libtorrent puts "y"
           ;; keys before "v" keys
           ;; (non-alphabetical sorting)
           (dict (handler-bind ((error (lambda (e)
                                         (declare (ignore e))
                                         (invoke-restart 'bencode::continue))))
                   (bencode:decode packet))))
      (eswitch ((gethash "y" dict) :test #'string=)
        ("q" (parse-query dict host port))
        ("r" (parse-response dict host port))
        ("e" ;; TODO: handle errors
         )))))