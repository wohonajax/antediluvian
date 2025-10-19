;;;; Code related to files

(in-package #:antediluvian)

(defun file-number-and-offset (byte-index file-list)
  "Returns N and FILE-INDEX as multiple values, where the Nth file in FILE-LIST
is indexed by BYTE-INDEX and FILE-INDEX is how far into that file BYTE-INDEX
indicates."
  (loop with index-so-far = 0
        for nth-file from 0
        for dict in file-list
        for file-size = (gethash "length" dict)
        for next-file-start = (+ index-so-far file-size)
        when (<= index-so-far byte-index (1- next-file-start))
          return (values nth-file (- byte-index index-so-far))
        do (setf index-so-far next-file-start)))

(defmacro with-chunk ((chunk-var &optional chunk) torrent piece-index byte-offset
                      chunk-length initially-form loop-body-form return-form)
  "Performs a chunk operation with the CHUNK (read from disk if not supplied)
bound to CHUNK-VAR. There will be a variable BYTES-SO-FAR initialized to 0.
There will be a variable OFFSET-INTO-FILE initialized to the file position in
the first file to read from or write to in the torrent's file list. There will
be a variable named CURRENT-FILE-NUMBER initialized to the index of the current
file to be read from or written to in the file list."
  (with-unique-names (info-dictionary file-dict-list piece-length byte-index
                       file-list indexed-file-number)
    `(let* ((,info-dictionary (gethash "info" (torrent-info ,torrent)))
            (,file-dict-list (gethash "files" ,info-dictionary))
            (,piece-length (gethash "piece length" ,info-dictionary))
            (,byte-index (+ (* ,piece-index ,piece-length)
                            ,byte-offset))
            (,file-list (torrent-file-list ,torrent)))
       (multiple-value-bind (,indexed-file-number offset-into-file)
           (file-number-and-offset ,byte-index ,file-dict-list)
         (loop with ,chunk-var = (or ,chunk (make-octets ,chunk-length))
               with bytes-so-far = 0
               for current-file-number from ,indexed-file-number
               initially ,initially-form
               until (= bytes-so-far ,chunk-length)
               do ,loop-body-form
               finally (return ,return-form))))))

(defmacro read-chunk (torrent piece-index byte-offset chunk-length chunk-var
                      &body return-form)
  "Reads a chunk of TORRENT indicated by PIECE-INDEX, BYTE-OFFSET, and CHUNK-LENGTH.
Binds the chunk to CHUNK-VAR and returns the result of RETURN-FORM."
  (with-unique-names (file-stream)
    `(with-chunk (,chunk-var)
       ,torrent ,piece-index ,byte-offset ,chunk-length
       ;; initially form
       (with-open-file (,file-stream
                        (nth current-file-number file-list)
                        :element-type '(unsigned-byte 8))
         (file-position ,file-stream offset-into-file)
         (setf bytes-so-far (read-sequence ,chunk-var ,file-stream))
         (incf current-file-number))
       ;; loop body form
       (with-open-file (,file-stream
                        (nth current-file-number file-list)
                        :element-type '(unsigned-byte 8))
         (setf bytes-so-far
               (read-sequence ,chunk-var ,file-stream :start bytes-so-far)))
       ;; return form
       ,@return-form)))

(defun have-piece-p (torrent piece-index)
  "Returns T if we have the piece number PIECE-INDEX of TORRENT. Returns NIL if
we don't have the piece, or if PIECE-INDEX is out of bounds."
  (let* ((metainfo (torrent-info torrent))
         (info-dictionary (gethash "info" metainfo))
         (pieces (gethash "pieces" info-dictionary))
         (piece-length (gethash "piece length" info-dictionary))
         (byte-index (* piece-index piece-length))
         (sha1-index (* piece-index 20))
         ;; if the piece is out of bounds, return nil
         (sha1-hash (handler-case (subseq pieces sha1-index (+ sha1-index 20))
                      (error () (return-from have-piece-p))))
         (file-dict-list (gethash "files" info-dictionary))
         (file-list (torrent-file-list torrent)))
    (multiple-value-bind (indexed-file-number offset-into-file)
        (file-number-and-offset byte-index file-dict-list)
      (loop with piece = (make-octets piece-length)
            with bytes-into-piece = 0
            for current-file-number from indexed-file-number
            initially (with-open-file (file-stream (nth current-file-number
                                                        file-list)
                                                   :element-type '(unsigned-byte 8))
                        (file-position file-stream offset-into-file)
                        (setf bytes-into-piece
                              (read-sequence piece file-stream))
                        (incf current-file-number))
            until (= bytes-into-piece piece-length)
            do (with-open-file (file-stream (nth current-file-number file-list)
                                            :element-type '(unsigned-byte 8))
                 (setf bytes-into-piece
                       (read-sequence piece file-stream :start bytes-into-piece)))
            finally (return (equalp sha1-hash (digest-sequence :sha1 piece)))))))

(defun write-chunk (torrent piece-index byte-offset chunk)
  "Writes a CHUNK indicated by a BYTE-OFFSET into the PIECE-INDEXth piece of
TORRENT to disk."
  (let* ((info-dictionary (gethash "info" (torrent-info torrent)))
         (pieces (gethash "pieces" info-dictionary))
         (piece-length (gethash "piece length" info-dictionary))
         (byte-index (+ (* piece-index piece-length)
                        byte-offset))
         (file-dict-list (gethash "files" info-dictionary))
         (file-list (torrent-file-list torrent)))
    (multiple-value-bind (indexed-file-number offset-into-file)
        (file-number-and-offset byte-index file-dict-list)
      (loop with bytes-written-so-far = 0
            with chunk-length = (length chunk)
            for current-file-number from indexed-file-number
            initially (let* ((current-file-length
                              (gethash "length" (nth current-file-number
                                                     file-dict-list)))
                             (bytes-to-write (- current-file-length
                                                offset-into-file)))
                        (with-open-file (file-stream
                                         (nth current-file-number file-list)
                                         :direction :output
                                         :element-type '(unsigned-byte 8)
                                         :if-exists :overwrite
                                         :if-does-not-exist :create)
                          (file-position file-stream offset-into-file)
                          (write-sequence chunk file-stream :end bytes-to-write)
                          (setf bytes-written-so-far bytes-to-write)
                          (incf current-file-number)))
            until (= bytes-written-so-far chunk-length)
            do (with-open-file (file-stream
                                (nth current-file-number file-list)
                                :direction :output
                                :element-type '(unsigned-byte 8)
                                :if-exists :overwrite
                                :if-does-not-exist :create)
                 (let* ((current-file-length
                         (gethash "length" (nth current-file-number
                                                file-dict-list)))
                        (ending-index (+ bytes-written-so-far
                                         current-file-length))
                        (bytes-to-write (- chunk-length
                                           ending-index)))
                   (write-sequence chunk file-stream
                                   :start bytes-written-so-far
                                   :end ending-index)
                   (incf bytes-written-so-far (- current-file-length
                                                 bytes-to-write))))))))

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