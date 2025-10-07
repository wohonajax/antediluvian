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
         ;; if the piece is out of bounds, return nil
         (sha1-hash (handler-case (subseq pieces sha1-index (+ sha1-index 20))
                      (error () (return-from have-piece-p))))
         (file-destination-path (torrent-destination torrent)))
    (with-open-file (file-stream file-destination-path :element-type '(unsigned-byte 8))
      (file-position file-stream (* piece-index piece-length))
      (let ((result (make-array piece-length :element-type '(unsigned-byte 8))))
        (read-sequence result file-stream)
        (equalp sha1-hash (digest-sequence :sha1 result))))))

(defun write-piece (piece piece-index torrent)
  "Writes the given PIECE-INDEXth PIECE of TORRENT to its file."
  (let* ((file-destination-path (torrent-destination torrent))
         (info-dictionary (gethash "info" (torrent-info torrent)))
         (piece-length (gethash "piece length" info-dictionary)))
    (with-open-file (file-stream file-destination-path
                                 :direction :output
                                 :if-exists :overwrite
                                 :if-does-not-exist :create
                                 :element-type '(unsigned-byte 8))
      (file-position file-stream (* piece-index piece-length))
      (write-sequence piece file-stream))))