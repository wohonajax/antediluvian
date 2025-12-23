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

(defun announce-to-tracker (torrent url)
  "Sends an announce GET request for TORRENT to URL."
  (let ((hash (torrent-info-hash torrent)))
    (dex:get (build-tracker-announce-url hash url))))

(defun torrent-announce (torrent)
  "Announces peer status for TORRENT to its associated tracker(s)."
  (let ((metadata (torrent-info torrent)))
    (if-let (announce-list (gethash "announce-list" metadata))
      (dolist (tier-or-url announce-list)
        (cond ((listp tier-or-url)
               ;; FIXME: announce to one URL at a time and
               ;; RETURN if the announce is successful
               (mapc (lambda (url) (announce-to-tracker torrent url))
                     tier-or-url))
              (t (announce-to-tracker torrent tier-or-url))))
      (announce-to-tracker torrent (gethash "announce" metadata)))))