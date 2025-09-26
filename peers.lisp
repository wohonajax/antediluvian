;;;; Code related to peers

(defvar *peer-list* (make-hash-table :test #'equalp)
  "A hash table containing info_hashes as keys and hash tables mapping IP
addresses to socket objects as values.")

(defvar *listening-peer-socket* nil
  "A TCP socket listening for connections from peers.")

(defun have-peers (info-hash)
  "Returns a list of peers for INFO-HASH."
  (let ((peer-list (gethash info-hash *peer-list*)))
    (unless peer-list
      (remhash info-hash *peer-list*))
    peer-list))