(in-package #:dhticl)

(define-constant +my-id+
  (make-string-from-bytes
   (map '(vector (unsigned-byte 8))
        #'random-byte
        (make-array 20)))
  :test #'equal)

(defstruct node
  (id nil :type string
      :read-only t)
  (ip nil :read-only t)
  (distance nil :read-only t)
  (last-activity nil :type fixnum)
  (health)
  (hashes nil :type list))

(defun create-node (&key (id nil) (ip nil) (distance nil)
		      (last-activity nil)
		      (health :questionable)
                      (hashes nil))
  "Creates a node object with the specified attributes."
  (make-node :id id
	     :ip ip
	     :distance distance
	     :last-activity last-activity
	     :health health
             :hashes hashes))

(defun calculate-node-distance (node)
  "Returns the distance between NODE and us."
  (calculate-distance (convert-id-to-int +my-id+)
		      (node-id node)))
