;;;; Code related to trackers

(in-package #:antediluvian)

(defun build-tracker-announce-url (base-url info-hash)
  "Builds a GET request URL as described at
<https://wiki.theory.org/BitTorrentSpecification#Tracker_Request_Parameters>."
  (with-output-to-string (str)
    (princ base-url str)
    (princ "?info_hash=" str)
    (princ (urlencode info-hash) str)
    (princ "&peer_id=" str)
    (princ (urlencode *peer-id*) str)
    (princ "&port=" str)
    (princ *default-port* str)
    (princ "&compact=1" str)))

(defun announce-to-tracker (torrent announce-url)
  "Sends an announce GET request for TORRENT to ANNOUNCE-URL. Returns NIL if
the request fails."
  (handler-case (dex:get (build-tracker-announce-url announce-url
                                                     (torrent-info-hash torrent))
                         :keep-alive nil)
    (error ())))

(defun torrent-announce (torrent)
  "Announces peer status for TORRENT to its associated tracker(s)."
  (let ((metadata (torrent-info torrent)))
    (if-let (announce-list (gethash "announce-list" metadata))
      (dolist (tier announce-list)
        ;; TODO: keep track of which URLs are responsive
        ;; and move them to the front of the list
        (mapc (lambda (url)
                (when (announce-to-tracker torrent url)
                  (return)))
              tier))
      (announce-to-tracker torrent (gethash "announce" metadata)))))