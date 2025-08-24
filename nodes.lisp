;;;; Code related to nodes

(in-package #:dhticl)

(defvar *id*
  (let ((array (make-array 20 :element-type '(unsigned-byte 8))))
    (dotimes (i 20 array)
      (setf (aref array i) (random 256)))))

(defvar *node-list* (list))

(defstruct node
  (id nil :read-only t)
  (ip)
  (port)
  (socket)
  (last-activity nil :type fixnum)
  (health))

(defun create-node (&key id ip port last-activity (health :questionable))
  "Creates a node object with the specified attributes and adds it
to *NODE-LIST*."
  (let ((node (make-node :id id
                         :ip ip
                         :port port
                         :socket (socket-connect
                                  ip port
                                  :protocol :datagram
                                  :element-type '(unsigned-byte 8))
                         :last-activity last-activity
                         :health health)))
    (push node *node-list*)
    node))

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
          ((< time-inactive 15) :good)
          (t :bad))))

(defun node-closer-p (goal node1 node2)
  "Returns T if NODE1 is closer to GOAL than NODE2, NIL otherwise."
  (let ((id1 (node-id node1))
        (id2 (node-id node2)))
    (< (calculate-distance (convert-id-to-int id1) goal)
       (calculate-distance (convert-id-to-int id2) goal))))

(defun compact-peer-info (node)
  "Translates NODE's IP and port into compact format."
  (concatenate '(vector (unsigned-byte 8))
               (node-ip node)
               (integer-to-octets (node-port node))))

(defun compact-node-info (node)
  "Translate's NODE's ID, IP, and port into compact peer format."
  (concatenate '(vector (unsigned-byte 8))
               (node-id node)
               (compact-peer-info node)))

(defun kill-node (node)
  "Closes the socket associated with NODE and removes it from the node list."
  (socket-close (node-socket node))
  (setf *node-list*
        (remove node *node-list* :test #'eq :count 1)))