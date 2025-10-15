;;;; Code related to peers

(in-package #:antediluvian)

(defun compact-peer-info (ip port)
  "Translates a peer's IP address and PORT into compact peer format."
  (concat-vec ip (port-to-octet-buffer port (make-octets 2))))

(defun pack-values-response (info-hash)
  "Returns a list of address/port byte vectors for peers under INFO-HASH in
compact peer format."
  (loop for peer in *peer-list*
        for socket = (force (peer-socket peer))
        when (and socket ; if the connection failed, don't include that peer
                  (member info-hash (peer-torrents peer)
                          :key #'torrent-info-hash
                          :test #'equalp))
          collect (compact-peer-info (get-peer-address socket)
                                     (get-peer-port socket))))