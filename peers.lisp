;;;; Code related to peers

(in-package #:dhticl)

(defvar *peer-list* (make-hash-table :test #'equalp)
  "A hash table containing info_hashes as keys and hash tables mapping IP
addresses to socket object futures as values. These futures will have NIL
values if the socket connection failed.")

(defvar *listening-peer-socket* nil
  "A TCP socket listening for connections from peers.")

(defun have-peers (info-hash)
  "Returns an alist of peer addresses and ports for INFO-HASH."
  (let ((peers (gethash info-hash *peer-list*)))
    (unless peers
      (remhash info-hash *peer-list*)
      (return-from have-peers))
    (loop for socket-future being the hash-values of peers
            using (hash-key ip)
          for socket = (force socket-future)
          when socket ; if the connection failed, don't include that peer
            collect (cons ip (get-peer-port socket)))))

(defun get-peer-socket (ip info-hash)
  "Returns the socket object associated with IP for a peer under INFO-HASH.
Returns NIL if there is no such peer or if the connection failed. Will block if
the connection attempt is still in progress."
  (when-let (peers (gethash info-hash *peer-list*))
    (force (gethash ip peers))))