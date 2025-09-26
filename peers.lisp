;;;; Code related to peers

(in-package #:dhticl)

(defvar *peer-list* (make-hash-table :test #'equalp)
  "A hash table containing info_hashes as keys and hash tables mapping IP
addresses to socket object futures as values. These futures will have NIL
values if the socket connection failed.")

(defvar *listening-peer-socket* nil
  "A TCP socket listening for connections from peers.")

(defun have-peers (info-hash)
  "Returns a list of peers for INFO-HASH."
  (let ((peer-list (gethash info-hash *peer-list*)))
    (unless peer-list
      (remhash info-hash *peer-list*))
    peer-list))

(defun get-peer-socket (ip info-hash)
  "Returns the socket object associated with IP for a peer under INFO-HASH, or
NIL if the connection failed. Returns NIL if there is no such peer or if the
connection failed. Will block if the connection attempt is still in progress."
  (when-let (peers (gethash info-hash *peer-list*))
    (force (gethash ip peers))))