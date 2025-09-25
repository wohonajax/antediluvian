;;;; Code related to peers

(defstruct peer ip port socket)

(defvar *peer-list* (make-hash-table :test #'equalp)
  "A hash table containing info_hashes as keys and a list of peer objects as
values.")

(defvar *listening-peer-socket* nil
  "A TCP socket listening for connections from peers.")

(defun kill-peer (peer)
  "Closes the socket associated with PEER and removes the peer from the peer
list."
  (maphash (lambda (info-hash peer-list)
             (when-let* ((member (member peer peer-list :test #'equalp))
                         (actual-member (first member)))
               (socket-close (peer-socket actual-member))
               (setf (gethash info-hash *peer-list*)
                     (remove actual-member peer-list :count 1))))
           *peer-list*))

(defun have-peers (info-hash)
  "Returns a list of peers for INFO-HASH."
  (let ((peer-list (gethash info-hash *peer-list*)))
    (unless peer-list
      (remhash info-hash *peer-list*))
    peer-list))