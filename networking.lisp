(in-package #:dhticl)

(defun listen-closely ()
  "Creates a temporary listening socket to receive responses."
  (usocket:with-connected-socket
      (socket (usocket:socket-connect
               nil nil
               :protocol :datagram
               :element-type '(unsigned-byte 8)
               :timeout 5
               :local-host usocket:*wildcard-host*
               :local-port *default-port*))
    (usocket:socket-receive socket nil 2048)))

(defun ping-then-listen (node)
  (progn (send-message :ping node)
         (listen-closely)))

;;;; TODO: make another layer of abstraction
(defun calculate-elapsed-inactivity (node)
  "Returns the time in minutes since NODE's last seen activity."
  (let ((last-activity (node-last-activity node)))
    (and last-activity (minutes-since last-activity))))

(defun calculate-last-activity (node)
  "Returns the universal timestamp of NODE's last seen activity."
  (let ((time-inactive (calculate-elapsed-inactivity node)))
    (cond (time-inactive time-inactive)
          ((ping-then-listen node) (get-universal-time))
          (t nil))))

(defun calculate-node-health (node)
  "Returns the node's health as a keyword, either :GOOD, :QUESTIONABLE, or :BAD."
  (let ((time-inactive (calculate-elapsed-inactivity node)))
    (cond ((null time-inactive) :questionable)
          ((< time-inactive 15) :good)
          ((ping-then-listen node) :good)
          (t :bad))))

(defun update-node (node)
  "Recalculates the time since NODE's last activity and updates its health
accordingly."
  (setf (node-last-activity node) (calculate-last-activity node)
        (node-health node) (calculate-node-health node)))

(defun ping-old-nodes (bucket)
  "Pings the nodes in a bucket from oldest to newest."
  (sort-bucket-by-age bucket)
  (iterate-bucket bucket (lambda (node) (ping-then-listen node)))
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
