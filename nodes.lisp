(in-package #:dhticl)

(alexandria:define-constant +my-id+
  (make-string-from-bytes
   (map '(vector (unsigned-byte 8))
        #'random-byte
        (make-array 20)))
  :test #'equal)

(defvar *node-list* (list))

(defstruct node
  (id nil :type string
          :read-only t)
  (ip nil :read-only t)
  (port nil :read-only t)
  (distance nil :read-only t)
  (last-activity nil :type fixnum)
  (health)
  (hashes nil :type list))

(defun create-node (&key (id nil) (ip nil) (port nil) (distance nil)
                      (last-activity nil)
                      (health :questionable)
                      (hashes nil))
  "Creates a node object with the specified attributes."
  (let ((node (make-node :id id
                         :ip ip
                         :port port
                         :distance distance
                         :last-activity last-activity
                         :health health
                         :hashes hashes)))
    (push node *node-list*)
    node))

(defun calculate-node-distance (node)
  "Returns the distance between NODE and us."
  (calculate-distance (convert-id-to-int +my-id+)
                      (convert-id-to-int (node-id node))))

(defun compact-node-info (node)
  "Translates NODE's IP and port into compact format."
  ;; FIXME: translate port to byte-string
  (format nil "~a~a"
          (node-ip node)
          (map 'string #'code-char
               (ironclad:integer-to-octets (node-port node)))))
