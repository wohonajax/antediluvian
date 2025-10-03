;;;; Code related to peers

(in-package #:antediluvian)

(defvar *peer-list* (make-hash-table :test #'equalp)
  "A hash table containing info_hashes as keys and hash tables mapping IP
addresses to socket object futures as values. These futures will have NIL
values if the socket connection failed.")

(defun compact-peer-info (ip port)
  "Translates a peer's IP address and PORT into compact peer format."
  (let ((port-vec (make-array 2 :element-type '(unsigned-byte 8))))
    (concat-vec ip (port-to-octet-buffer port port-vec))))

(defun pack-values-response (info-hash)
  "Returns a list of address/port byte-vectors for peers under INFO-HASH in
compact peer format."
  (when-let (peers (gethash info-hash *peer-list*))
    (loop for socket-future being the hash-values of peers
            using (hash-key ip)
          for socket = (force socket-future)
          when socket ; if the connection failed, don't include that peer
            collect (compact-peer-info ip (get-peer-port socket)))))
