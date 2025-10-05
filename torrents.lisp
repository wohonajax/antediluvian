;;;; Code related to torrents

(in-package #:antediluvian)

(defclass torrent ()
  ((info-hash :initarg :info-hash :accessor torrent-info-hash)
   (name :initarg :name :accessor torrent-name)
   (destination-path :initarg :destination :accessor torrent-destination)))

(defun magnet-link-p (item)
  "Tests whether ITEM is a magnet link."
  (and (stringp item) (starts-with-p "magnet:?" item)))

(defun extract-sha1-from-parsed-magnet-link (hash-table)
  "Extracts a BitTorrent info hash from HASH-TABLE, a parsed magnet link."
  (loop for (protocol hash)
          in (magnet:get-all-exact-topics hash-table)
        when (eq protocol :btih)
          return hash))

(defun bdecode-torrent-file (filespec)
  "Bdecodes the torrent file referred to by FILESPEC."
  (with-open-file (stream filespec :element-type '(unsigned-byte 8))
    (bencode:decode stream)))

(defun get-info-hash-from-bdecoded-torrent-file (hash-table)
  "Gets the SHA1 info hash from HASH-TABLE, a decoded torrent file."
  (let ((dict (make-hash-table :test #'equal)))
    ;; we need a dictionary of only the "info" part of the torrent file
    (setf (gethash "info" dict) (gethash "info" hash-table))
    (digest-sequence :sha1 (bencode:encode dict nil))))

(defun make-download-pathname (filename)
  "Creates a download destination pathname from FILENAME."
  (uiop:truenamize (merge-pathnames filename *default-download-directory*)))

(defun parse-source (source)
  "Parses SOURCE into a torrent object. SOURCE should be a magnet link,
a filespec to a torrent file, or a SHA1 hash."
  (let* ((parsed-source (cond ((magnet-link-p source)
                               (magnet:parse source))
                              ((filespecp source)
                               (bdecode-torrent-file (uiop:truenamize source)))
                              (t source)))
         (hash (cond ((magnet-link-p source)
                      (extract-sha1-from-parsed-magnet-link parsed-source))
                     ((filespecp source)
                      (get-info-hash-from-bdecoded-torrent-file parsed-source))
                     (t source)))
         (name (cond ((magnet-link-p source)
                      ;; all parameters in a parsed
                      ;; magnet link have list values
                      (first (gethash :dn parsed-source)))
                     ((filespecp source)
                      (gethash "name" (gethash "info" parsed-source))))))
    (make-instance 'torrent :info-hash hash :name name
                            :destination (make-download-pathname name))))

(defun parse-sources (list-of-sources)
  "Converts every source in LIST-OF-SOURCES to a SHA1 hash."
  (mapcar #'parse-source list-of-sources))

(defun add-torrent (source)
  "Adds a torrent from SOURCE, which should be a magnet link, a filespec to a
torrent file, or a SHA1 hash."
  (add-hash (torrent-info-hash (parse-source source))))