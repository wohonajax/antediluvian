;;;; Code related to nodes

(in-package #:dhticl)

(defvar *id* (make-hash (random-data 20)))

(defstruct node
  (id nil :read-only t)
  (ip)
  (port)
  (socket)
  (last-activity nil :type fixnum)
  (health)
  (failed-rpcs 0 :type fixnum))

(defun create-node (&key id ip port last-activity (health :questionable))
  "Creates a node object with the specified attributes."
  (make-node :id id :ip ip :port port
             :socket (socket-connect ip port :protocol :datagram
                                     :element-type '(unsigned-byte 8))
             :last-activity last-activity :health health))

(defun calculate-node-distance (node target)
  "Returns the distance between NODE and TARGET."
  (calculate-distance (convert-id-to-int target)
                      (convert-id-to-int (node-id node))))

(defun calculate-elapsed-inactivity (node)
  "Returns the time in minutes since NODE's last seen activity."
  (let ((last-activity (node-last-activity node)))
    (and last-activity (minutes-since last-activity))))

(defun calculate-node-health (node)
  "Returns the node's health as a keyword, either :GOOD, :QUESTIONABLE,
or :BAD."
  (let ((time-inactive (calculate-elapsed-inactivity node)))
    (cond ((null time-inactive) :questionable)
          ((< time-inactive 15) (node-health node))
          (t :bad))))

(defun node-stale-p (node)
  "Returns T if NODE is stale, NIL otherwise."
  (>= (node-failed-rpcs node) 5))

(defun node-closer-p (goal node1 node2)
  "Returns T if NODE1 is closer to GOAL than NODE2, NIL otherwise."
  (< (calculate-node-distance node1 goal)
     (calculate-node-distance node2 goal)))

(defun compact-peer-info (node)
  "Translates NODE's IP and port into compact format."
  (let ((port-vec (make-array 2 :element-type '(unsigned-byte 8))))
    (concat-vec (node-ip node)
                (port-to-octet-buffer (node-port node) port-vec))))

(defun compact-node-info (node)
  "Translate's NODE's ID, IP, and port into compact peer format."
  (concat-vec (node-id node)
              (compact-peer-info node)))

(defun kill-node (node)
  "Closes the socket associated with NODE."
  (socket-close (node-socket node)))