;;;; Code related to torrents

(in-package #:antediluvian)

(defun magnet-link-p (item)
  "Tests whether ITEM is a magnet link."
  (and (stringp item) (starts-with-p "magnet:?" item)))

(defun extract-sha1-from-magnet-link (magnet-link)
  "Extracts a BitTorrent info hash from MAGNET-LINK."
  (loop for (protocol hash)
          in (magnet:get-all-exact-topics (magnet:parse magnet-link))
        when (eq protocol :btih)
          return hash))

(defun bdecode-torrent-file (filespec)
  "Bdecodes the torrent file referred to by FILESPEC."
  (with-open-file (stream filespec :element-type '(unsigned-byte 8))
    (bencode:decode stream)))

(defun get-info-hash-from-torrent-file (filespec)
  "Gets the SHA1 info hash from a torrent file referred to by FILESPEC."
  (let ((dict (make-hash-table :test #'equal))
        (torrent-file-dict (bdecode-torrent-file filespec)))
    ;; we need a dictionary of only the "info" part of the torrent file
    (setf (gethash "info" dict) (gethash "info" torrent-file-dict))
    (digest-sequence :sha1 (bencode:encode dict nil))))

(defun parse-source (source)
  "Parses SOURCE into a SHA1 hash. SOURCE should be a magnet link, filespec to
a torrent file, or a SHA1 hash."
  (cond ((magnet-link-p source)
         (extract-sha1-from-magnet-link source))
        ((or (stringp source) (pathnamep source))
         (get-info-hash-from-torrent-file (truenamize source)))
        (t source)))

(defun parse-sources (list-of-sources)
  "Converts every source in LIST-OF-SOURCES to a SHA1 hash."
  (mapcar #'parse-source list-of-sources))

(defun add-torrent (source)
  "Adds a torrent from SOURCE, which should be a magnet link, a filespec to a
torrent file, or a SHA1 hash."
  (add-hash (parse-source source)))