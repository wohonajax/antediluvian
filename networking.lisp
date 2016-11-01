(in-package #:dhticl)

(defun listen-closely ()
  "Creates a temporary listening socket to receive responses."
  (usocket:with-connected-socket
      (socket (usocket:socket-connect
               usocket:*wildcard-host* *default-port*
               :protocol :datagram
               :element-type '(unsigned-byte 8)
               :timeout 5))
    (usocket:socket-receive socket nil 2048)))

;;;; TODO: make another layer of abstraction
(defun poke-node (node)
  "Pings NODE and waits a short length of time for a response. Returns the
response if we get one, otherwise returns NIL."
  (ping node)
  (listen-closely))

(defun hit-on-node (node)
  "Asks NODE for its contact information."
  (find-node node)
  (listen-closely))

(defun beg-node (node torrent)
  "Asks NODE for peers of TORRENT."
  (get-peers torrent node)
  (listen-closely))

(defun divulge-to-node (node torrent)
  "Tells NODE we're entering the swarm for TORRENT."
  (announce-peer torrent node)
  (listen-closely))

(defun calculate-elapsed-inactivity (node)
  "Returns the time in minutes since NODE's last seen activity."
  (let ((last-activity (node-last-activity node)))
    (and last-activity (minutes-since last-activity))))

(defun calculate-last-activity (node)
  "Returns the universal timestamp of NODE's last seen activity."
  (let ((time-inactive (calculate-elapsed-inactivity node)))
    (cond (time-inactive time-inactive)
          ((poke-node node) (get-universal-time))
          (t nil))))

(defun calculate-node-health (node)
  "Returns the node's health as a keyword, either :GOOD, :QUESTIONABLE, or :BAD."
  (let ((time-inactive (calculate-elapsed-inactivity node)))
    (cond ((null time-inactive) :questionable)
          ((< time-inactive 15) :good)
          ((poke-node node) :good)
          (t :bad))))

(defun update-node (node)
  "Recalculates the time since NODE's last activity and updates its health
accordingly."
  (setf (node-last-activity node) (calculate-last-activity node)
        (node-health node) (calculate-node-health node)))

(defun ping-old-nodes (bucket)
  "Pings the nodes in a bucket from oldest to newest."
  (sort-bucket-by-age bucket)
  (iterate-bucket bucket #'poke-node)
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

(defun handle-questionable-node (node)
  "Elucidates the health of NODE."
  (setf (node-health node)
        (cond ((poke-node node) :good)
              ((poke-node node) :good)
              (t :bad)))
  (update-bucket (correct-bucket (node-id node))))

(defun handle-questionable-nodes (bucket)
  "Handles all nodes in BUCKET that are of questionable health."
  (iterate-bucket bucket
                  (lambda (node)
                    (when (eql :questionable (node-health node))
                      (handle-questionable-node node))))
  (update-bucket bucket))
