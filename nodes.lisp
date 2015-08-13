(in-package #:dhticl-nodes)

(defvar +my-id+ ;; TODO: don't forget to make this a constant
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
