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
  "Sends announce GET requests for TORRENT to its associated trackers."
  (let ((metadata (torrent-info torrent)))
    (if-let (announce-list (gethash "announce-list" metadata))
      (let ((current-tier 0)
            (current-tracker 0))
        (dolist (tier announce-list)
          (mapc (lambda (url)
                  (when (announce-to-tracker torrent url)
                    (return))
                  (incf current-tracker))
                tier)
          (incf current-tier)
          (setf current-tracker 0))
        (unless (= current-tracker 0)
          (let* ((operative-tier (nth current-tier announce-list))
                 (responsive-tracker (nth current-tracker operative-tier)))
            (setf (nth current-tracker operative-tier)
                  (cons responsive-tracker
                        (remove responsive-tracker
                                operative-tier
                                :test #'string=))))))
      (announce-to-tracker torrent (gethash "announce" metadata)))))