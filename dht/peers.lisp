;;;; Code related to peers

(in-package #:antediluvian)

(defun compact-peer-info (ip port)
  "Translates a peer's IP address and PORT into compact peer format."
  (concat-vec ip (port-to-octet-buffer port (make-octets 2))))

(defun pack-values-response (info-hash)
  "Returns a list of address/port byte vectors for peers under INFO-HASH in
compact peer format."
  (with-lock-held (*peer-list-lock*)
    (loop for peer in *peer-list*
          when (equalp info-hash (torrent-info-hash (peer-torrent peer)))
            collect (compact-peer-info (peer-ip peer)
                                       (peer-port peer)))))