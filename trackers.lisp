;;;; Code related to trackers

(in-package #:antediluvian)

(defun build-tracker-announce-url (base-url info-hash &key (event :started))
  "Builds a GET request URL as described at
<https://wiki.theory.org/BitTorrentSpecification#Tracker_Request_Parameters>.
The EVENT keyword argument should be one of :STARTED, :STOPPED, or :COMPLETED."
  (with-output-to-string (str)
    (princ base-url str)
    (princ "?info_hash=" str)
    (princ (urlencode-binary-data info-hash) str)
    (princ "&peer_id=" str)
    (princ (urlencode-binary-data *peer-id*) str)
    (princ "&port=" str)
    (princ *port* str)
    ;; TODO: keep track of upload/download/left
    (princ "&uploaded=0" str)
    (princ "&downloaded=0" str)
    (princ "&left=0" str)
    (princ "&compact=1" str)
    (princ "&no_peer_id=1" str)
    ;; TODO: determine whether to send started, stopped, or completed
    (princ "&event=" str)
    (case event
      (:started (princ "started" str))
      (:stopped (princ "stopped" str))
      (:completed (princ "completed" str)))))

(defun parse-announce-response (response-vector info-hash)
  "Parses RESPONSE-VECTOR, a Bencoded byte vector result from an announce GET
request. Returns a list of peers."
  (let* ((bencode:*binary-key-p* (curry #'equal '("peers")))
         (peers-vector (gethash "peers" (bencode:decode response-vector)))
         (total-length (length peers-vector))
         peers)
    (do ((i 0 (+ i 6)))
        ((= i total-length))
      (multiple-value-bind (ip port)
          (parse-node-ip (subseq peers-vector i (+ i 6)))
        (let ((peer (make-peer ip port info-hash)))
          (with-lock-held (*peer-list-lock*)
            (unless (member (peer-ip peer) *peer-list* :key #'peer-ip :test #'equalp)
              (push peer peers)
              (push peer *peer-list*)
              (initiate-peer-connection peer))))))
    peers))

(defun announce-to-tracker (torrent announce-url)
  "Sends an announce GET request for TORRENT to ANNOUNCE-URL. Returns NIL if
the request fails."
  (let ((info-hash (torrent-info-hash torrent)))
    (handler-case (parse-announce-response
                   (dex:get (build-tracker-announce-url announce-url info-hash)
                            :keep-alive nil
                            :force-binary t)
                   info-hash)
      (error ()))))

(defun torrent-announce (torrent)
  "Sends announce GET requests for TORRENT to its associated trackers."
  (let ((metadata (torrent-info torrent)))
    (if-let (announce-list (gethash "announce-list" metadata))
      (let ((current-tier 0)
            (current-tracker 0)
            peers)
        (dolist (tier announce-list)
          (mapc (lambda (url)
                  (when-let (intermediate-list (announce-to-tracker torrent url))
                    (setf peers intermediate-list)
                    (return))
                  (incf current-tracker))
                tier)
          (incf current-tier)
          (setf current-tracker 0))
        (unless (= current-tracker 0)
          (let ((operative-tier (nth current-tier announce-list)))
            (rotatef (first operative-tier)
                     (nth current-tracker operative-tier))))
        ;; return the list of peers after populating their torrent slots
        (mapc (lambda (peer) (setf (peer-torrent peer) torrent))
              peers))
      (announce-to-tracker torrent (gethash "announce" metadata)))))