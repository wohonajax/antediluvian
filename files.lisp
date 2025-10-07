;;;; Code related to files

(in-package #:antediluvian)

(defun have-piece-p (piece-index torrent)
  "Returns T if we have the piece number PIECE-INDEX of TORRENT. Returns NIL if
we don't have the piece, or if PIECE-INDEX is out of bounds."
  (let* ((metainfo (torrent-info torrent))
         (info-dictionary (gethash "info" metainfo))
         (pieces (gethash "pieces" info-dictionary))
         (piece-length (gethash "piece length" info-dictionary))
         (sha1-index (* piece-index 20))
         (sha1-hash (subseq pieces sha1-index (+ sha1-index 20)))
         (file-destination-path (torrent-destination torrent)))
    (with-open-file (file-stream file-destination-path :element-type '(unsigned-byte 8))
      (file-position file-stream (* piece-index piece-length))
      (let ((result (make-array piece-length :element-type '(unsigned-byte 8))))
        ;; if we get an end-of-file error, we're out of bounds. return nil
        (handler-case (read-sequence result file-stream)
          (end-of-file () (return-from have-piece-p)))
        (equalp sha1-hash (digest-sequence :sha1 result))))))