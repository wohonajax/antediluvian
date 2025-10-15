;;;; Code related to nodes

(in-package #:antediluvian)

(defvar *id* (digest-sequence :sha1 (random-data 20))
  "The node ID for this session.")

(defclass node ()
  ((id :initarg :id :accessor node-id)
   (ip :initarg :ip :accessor node-ip)
   (port :initarg :port :accessor node-port)
   (last-activity :initarg :last-activity :accessor node-last-activity)
   (health :initarg :health :accessor node-health)
   (failed-rpcs :initform 0 :accessor node-failed-rpcs)))

(defun create-node (&key id ip port last-activity (health :questionable))
  "Creates a node object with the specified attributes."
  (make-instance 'node :id id :ip ip :port port
                 :last-activity last-activity :health health))

(defun calculate-node-distance (node target)
  "Returns the distance between NODE and TARGET."
  (calculate-distance (convert-id-to-int target)
                      (convert-id-to-int (node-id node))))

(defun calculate-elapsed-inactivity (node)
  "Returns the time in minutes since NODE's last seen activity."
  ;; make sure we don't try to calculate the time since nil,
  ;; just in case the last-activity slot hasn't been updated
  (when-let (last-activity (node-last-activity node))
    (minutes-since last-activity)))

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

(defun compact-node-info (node)
  "Translate's NODE's ID, IP, and port into compact peer format."
  (concat-vec (node-id node)
              (compact-peer-info (node-ip node) (node-port node))))
