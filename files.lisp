;;;; Code related to files

(in-package #:antediluvian)

(defun file-number-and-offset (byte-index file-list)
  "Returns N and FILE-INDEX as multiple values, where the Nth file in FILE-LIST
is indexed by BYTE-INDEX and FILE-INDEX is how far into that file BYTE-INDEX
indicates."
  (loop with index-so-far = 0
        for nth-file upfrom 0
        for dict in file-list
        for file-size = (gethash "length" dict)
        for file-start = index-so-far then (+ file-start file-size)
        for file-end = (+ file-start file-size)
        when (<= file-start byte-index (1- file-end))
          return (values nth-file (- byte-index file-start))
        do (setf index-so-far file-end)))

(defun have-piece-p (torrent piece-index)
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
      (let ((result (make-octets piece-length)))
        (read-sequence result file-stream)
        (equalp sha1-hash (digest-sequence :sha1 result))))))

(defun write-piece (torrent piece piece-index)
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

(defun write-block (torrent block piece-index begin)
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

(defun read-block (torrent piece-index byte-offset block-length)
  "Reads a block from TORRENT indicated by PIECE-INDEX, BYTE-OFFSET, and
BLOCK-LENGTH and returns it as a byte vetor."
  (let* ((metainfo (torrent-info torrent))
         (info-dictionary (gethash "info" metainfo))
         (piece-length (gethash "piece length" info-dictionary))
         (true-index (+ (* piece-index piece-length)
                        byte-offset))
         (file-dict-list (gethash "files" info-dictionary))
         (file-list (torrent-file-list torrent)))
    (multiple-value-bind (indexed-file-number offset-into-file)
        (file-number-and-offset true-index file-dict-list)
      (loop with bytes-read-so-far = 0
            with block = (make-octets block-length)
            for current-file-number-to-read from indexed-file-number
            initially (with-open-file (file-stream (nth current-file-number-to-read
                                                        file-list)
                                                   :element-type '(unsigned-byte 8))
                        (file-position file-stream offset-into-file)
                        (setf bytes-read-so-far
                              (read-sequence block file-stream))
                        (incf current-file-number-to-read))
            until (= bytes-read-so-far block-length)
            do (with-open-file (file-stream (nth current-file-number-to-read
                                                 file-list)
                                            :element-type '(unsigned-byte 8))
                 (setf bytes-read-so-far
                       (read-sequence block file-stream :start bytes-read-so-far)))
            finally (return block)))))

(defstruct block-request piece-index byte-offset block-length)

(defstruct write-instruction torrent block piece-index byte-offset)

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