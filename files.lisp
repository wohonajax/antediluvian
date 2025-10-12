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
         (pieces (gethash "pieces" info-dictionary))
         (sha1-index (* piece-index 20))
         (piece-length (gethash "piece length" info-dictionary)))
    (unless (equalp (digest-sequence :sha1 piece)
                    (subseq pieces sha1-index (+ sha1-index 20)))
      (return-from write-piece))
    (with-open-file (file-stream file-destination-path
                                 :direction :output
                                 :if-exists :overwrite
                                 :if-does-not-exist :create
                                 :element-type '(unsigned-byte 8))
      (file-position file-stream (* piece-index piece-length))
      (write-sequence piece file-stream))))

(defun write-block (block piece-index begin torrent)
  "Writes a BLOCK starting at a BEGIN offset within the PIECE-INDEXth piece of
TORRENT to the appropriate file."
  (let* ((file-destination-path (torrent-destination torrent))
         (info-dictionary (gethash "info" (torrent-info torrent)))
         (piece-length (gethash "piece-length" info-dictionary)))
    (with-open-file (file-stream file-destination-path
                                 :direction :output
                                 :if-exists :overwrite
                                 :if-does-not-exist :create
                                 :element-type '(unsigned-byte 8))
      (file-position file-stream (+ (* piece-index piece-length)
                                    begin))
      (write-sequence block file-stream))))

(defstruct write-instruction torrent block piece-index byte-offset length)

(defvar *write-instructions-channel* (make-instance 'chanl:channel))

(defun write-instruction-values (write-instruction)
  "Returns a block to write, its piece index location, its byte offset from
that piece index, and the torrent the instruction corresponds to as multiple
values."
  (values (write-instruction-block write-instruction)
          (write-instruction-piece-index write-instruction)
          (write-instruction-byte-offset write-instruction)
          (write-instruction-torrent write-instruction)))

(defun start-file-writer-thread ()
  "Starts a thread that manages writing files to disk."
  (make-thread
   (lambda ()
     (loop (multiple-value-call #'write-block
                                (write-instruction-values
                                 (chanl:recv *write-instructions-channel*)))))
   :name "file-writer"))

(defvar *file-writer-thread* nil
  "A thread managing file write operations.")