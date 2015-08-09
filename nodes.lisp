(in-package #:dhticl-nodes)

(defun calculate-distance (a b)
  "Returns the distance between A and B."
  (logxor a b))

(defun convert-id-to-int (id)
  "Converts a node ID from a hexadecimal string to a decimal integer."
  (parse-integer id :radix 16))

(defun convert-id-to-hex (id)
  "Converts a node ID from a decimal integer to a hexadecimal string."
  (string-downcase (format nil "~X" id)))

(defconstant +my-id+
  (convert-id-to-hex (random (expt 2 160))))

(defstruct node
  (id nil :type string
      :read-only t)
  (ip nil :read-only t)
  (distance nil :read-only t)
  (last-activity nil :type fixnum)
  (health))

(defun create-node (&key (id nil) (ip nil) (distance nil)
		      (last-activity nil)
		      (health :questionable))
  "Creates a node object with the specified attributes."
  (make-node :id id
	     :ip ip
	     :distance distance
	     :last-activity last-activity
	     :health health))

(defun calculate-node-distance (node)
  "Returns the distance between NODE and us."
  (calculate-distance (convert-id-to-int +my-id+)
		      (node-id node)))

(defun calculate-elapsed-inactivity (node)
  "Returns the time in minutes since NODE's last seen activity."
  (let ((last-activity (node-last-activity node)))
    (and last-activity (minutes-since last-activity))))

(defun calculate-last-activity (node)
  "Returns the universal timestamp of NODE's last seen activity."
  (let ((time-inactive (calculate-elapsed-inactivity node)))
    (cond (time-inactive time-inactive)
	  ((ping-node) (get-universal-time))
	  (t nil))))

(defun calculate-node-health (node)
  "Returns the node's health as a keyword, either :GOOD, :QUESTIONABLE, or :BAD."
  (let ((time-inactive (calculate-elapsed-inactivity node)))
    (cond ((null time-inactive) :questionable)
	  ((< time-inactive 15) :good)
	  ((ping-node node) :good)
	  (t :bad))))

(defun update-node (node)
  "Recalculates the time since NODE's last activity and updates its health
  accordingly."
  (setf (node-last-activity node) (calculate-last-activity node)
	(node-health node) (calculate-node-health node)))
#|
(defun make-node-hash (node)
  "Constructs an infohash using NODE."
  (make-hash (ironclad:integer-to-octets (ip node))))
|#
