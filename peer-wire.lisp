;;;; Code related to the BitTorrent peer protocol

(in-package #:antediluvian)

(defun generate-peer-id ()
  "Generates a 20-byte peer ID for this session. Uses \"AD\" for the client
header (for AnteDiluvian). Uses an Azureus-style client ID string."
  (concat-vec (ascii-string-to-byte-array "-AD0000-")
              ;; 2 hyphens + 2 client ID characters
              ;; + 4 version number characters = 8
              ;; 20 - 8 = 12
              (random-data 12)))

(defvar *peer-id* (generate-peer-id)
  "The peer ID for this session.")

;;;; Peer wire protocol

(defconstant +length-offset+ (expt 2 14)
  "The length offset to use for request and cancel messages.")

(defun write-handshake-header (stream)
  "Writes the BitTorrent handshake header to STREAM."
  (let ((protocol-vector (ascii-string-to-byte-array "BitTorrent protocol")))
    (write-byte (length protocol-vector) stream) ; 19
    (write-sequence protocol-vector stream)))

(defun write-handshake-header-reserved-bytes (stream)
  "Writes the reserved bytes of the handshake that indicate protocol
extensions to STREAM."
  (let ((reserved-bytes (make-octets 8 :initial-element 0)))
    (flet ((set-bit (nth-bit nth-byte)
             ;; nth-bit is "big-endian"/left-justified; a value of 7 means
             ;; the 8th bit from left to right (i.e., setting bit 7 will
             ;; result in 1 and setting bit 0 will result in 128)
             (setf (ldb (byte 8 (- 7 nth-bit))
                        (aref reserved-bytes nth-byte))
                   1)))
      (set-bit 7 7) ; DHT extension (the last bit in the last reserved byte)
      (write-sequence reserved-bytes stream))))

(defun parse-extensions-for-peer (peer extension-bytes-vector)
  "Parses the reserved bits set in EXTENSION-BYTES-VECTOR and sets the
appropriate slots in PEER."
  (flet ((bit-setp (bit byte)
           ;; BIT of bytes[BYTE]
           (= 1 (ldb (byte 8 bit) (aref extension-bytes-vector byte)))))
    (when (bit-setp 7 0)
      (setf (supports-azureus-messaging-protocol-p peer) t))
    (when (bit-setp 3 2)
      (setf (supports-bittorrent-location-aware-protocol-p peer) t))
    (when (bit-setp 4 5)
      (setf (supports-libtorrent-extension-protocol-p peer) t))
    (when (bit-setp 0 5)
      (setf (supports-extension-negotiation-protocol-p peer) t))
    (when (bit-setp 1 5)
      (setf (supports-extension-negotiation-protocol-p peer) t))
    ;; peers are assumed to support DHT
    (when (not (bit-setp 0 7))
      (setf (supports-bittorrent-dht-p peer) nil))
    (when (bit-setp 1 7)
      (setf (supports-peer-exchange-p peer) t))
    (when (bit-setp 2 7)
      (setf (supports-fast-extension-p peer) t))
    (when (bit-setp 3 7)
      (setf (supports-nat-traversal-p peer) t))))

(defun receive-handshake (socket)
  "Receives a BitTorrent protocol handshake from a peer connected to SOCKET.
Returns the peer object, or NIL if the handshake failed."
  (macrolet ((close-unless (test)
               `(unless ,test
                  (socket-close socket)
                  (return-from receive-handshake nil))))
    (lret* ((stream (socket-stream socket))
            (protocol-vector (make-octets 19))
            (extensions-vector (make-octets 8))
            (hash (make-octets 20))
            (peer-id (make-octets 20))
            (peer (make-instance 'peer :ip (get-peer-address socket)
                                 :port (get-peer-port socket)
                                 :socket socket
                                 :id peer-id)))
      (close-unless (= (read-byte stream) 19))
      (read-sequence protocol-vector stream)
      (close-unless (string= (byte-array-to-ascii-string protocol-vector)
                             "BitTorrent protocol"))
      (read-sequence extensions-vector stream)
      (parse-extensions-for-peer peer extensions-vector)
      (read-sequence hash stream)
      (close-unless (gethash hash *torrent-hashes*))
      ;; we've checked that the hash is among our active
      ;; torrents, so just write the hash back to confirm
      (write-sequence hash stream)
      (finish-output stream)
      (setf (peer-torrent peer) (gethash hash *torrent-hashes*))
      (read-sequence peer-id stream)
      (with-lock-held (*peer-list-lock*)
        (push peer *peer-list*)))))

(defvar *message-id-to-message-type-alist*
        '((0 . :choke)
          (1 . :unchoke)
          (2 . :interested)
          (3 . :not-interested)
          (4 . :have)
          (5 . :bitfield)
          (6 . :request)
          (7 . :piece)
          (8 . :cancel)
          (9 . :port))
  "Alist mapping message byte IDs to message type keywords.")

(defun read-4-bytes-to-integer (stream)
  "Reads 4 big-endian bytes from STREAM and converts the result to an integer."
  (let ((vector (make-octets 4)))
    (read-sequence vector stream)
    (octets-to-integer vector)))

(defun read-peer-wire-length-header (stream)
  "Reads a 4-byte peer wire message length header from STREAM."
  (read-4-bytes-to-integer stream))

(defun message-id-to-message-type (id)
  "Translates a message ID byte to a keyword denoting the message type."
  (cdr (assoc id *message-id-to-message-type-alist*)))

(defun parse-bitfield (bitfield-vector)
  "Parses a bitfield out of BITFIELD-VECTOR. Bits are read from left to right."
  (loop with had-pieces = '()
        with piece-index = 0
        for byte across bitfield-vector
        do (loop for bit below 8
                 do (when (= 1 (ldb (byte 1 (- 7 bit)) byte))
                      (push piece-index had-pieces))
                    (incf piece-index))
        finally (return (nreverse had-pieces))))

(defun perform-handshake (peer)
  "Performs a BitTorrent protocol handshake with PEER. Returns T if successful,
NIL if not."
  (let* ((socket (peer-socket peer))
         (stream (socket-stream socket))
         (hash (torrent-info-hash (peer-torrent peer))))
    (write-handshake-header stream) ; protocol length prefix and string
    (write-handshake-header-reserved-bytes stream)
    (write-sequence hash stream)
    (write-sequence *peer-id* stream)
    (finish-output stream)
    ;; if we don't get the same hash back as
    ;; the one we send, sever the connection
    (let ((peer-hash (make-octets 20)))
      (read-sequence peer-hash stream)
      (unless (equalp hash peer-hash)
        (with-lock-held ((peer-lock peer))
          (socket-close socket))
        (with-lock-held (*peer-list-lock*)
          (removef *peer-list* peer :count 1))
        (return-from perform-handshake nil)))
    ;; return t if the handshake was successful
    t))

(defun message-id-for-message-type (message-type)
  "Returns the byte to be sent to a peer for a MESSAGE-TYPE message.
MESSAGE-TYPE must be a keyword."
  (car (rassoc message-type *message-id-to-message-type-alist*)))

(defun pad-integer-to-octets (integer total-length)
  "Converts INTEGER to a big-endian vector of octets of length TOTAL-LENGTH.
Pads with zeros from the left."
  (let ((integer-vector (integer-to-octets integer)))
    (concat-vec (make-array (- total-length (length integer-vector))
                            :initial-element 0)
                integer-vector)))

(defmacro with-socket-stream ((stream-var socket) &body body)
  "Binds STREAM-VAR to the socket stream of SOCKET, executes BODY with
STREAM-VAR bound, and finally calls FINISH-OUTPUT on the stream."
  `(let ((,stream-var (socket-stream ,socket)))
     ,@body
     (finish-output ,stream-var)))

(defun send-peer-message-length-header (message-length socket)
  "Sends a peer wire protocol message length header to the peer connected to
SOCKET."
  (with-socket-stream (stream socket)
    (write-sequence (pad-integer-to-octets message-length 4) stream)))

(defun send-keep-alive-message (socket)
  "Sends a keep-alive message to the peer connected to SOCKET."
  (send-peer-message-length-header 0 socket))

(defun send-choke-message (socket)
  "Sends a choke message to the peer connected to SOCKET."
  (send-peer-message-length-header 1 socket)
  (with-socket-stream (stream socket)
    (write-byte (message-id-for-message-type :choke) stream)))

(defun send-unchoke-message (socket)
  "Sends an unchoke message to the peer connected to SOCKET."
  (send-peer-message-length-header 1 socket)
  (with-socket-stream (stream socket)
    (write-byte (message-id-for-message-type :unchoke) stream)))

(defun send-interested-message (socket)
  "Sends an interested message to the peer connected to SOCKET."
  (send-peer-message-length-header 1 socket)
  (with-socket-stream (stream socket)
    (write-byte (message-id-for-message-type :interested) stream)))

(defun send-not-interested-message (socket)
  "Sends a not interested message to the peer connected to SOCKET."
  (send-peer-message-length-header 1 socket)
  (with-socket-stream (stream socket)
    (write-byte (message-id-for-message-type :not-interested) stream)))

(defun send-have-message (piece-index socket)
  "Sends a have message to the peer connected to SOCKET stating that we have
the PIECE-INDEXth piece of a torrent."
  (send-peer-message-length-header 5 socket)
  (with-socket-stream (stream socket)
    (write-byte (message-id-for-message-type :have) stream)
    (write-sequence (pad-integer-to-octets piece-index 4) stream)))

(defun send-bitfield-message (torrent socket)
  "Sends a bitfield message to the peer connected to SOCKET regarding TORRENT.
Bitfield messages essentially communicate which pieces of a torrent we already
have."
  (with-socket-stream (stream socket)
    (let* ((number-of-pieces (number-of-pieces torrent))
           ;; we want the ceiling so we don't lose pieces.
           ;; extra bits are zeros
           (pieces-length (ceiling number-of-pieces 8))
           (bitfield-vector (make-octets pieces-length :initial-element 0)))
      (if-let (had-pieces (with-lock-held ((torrent-lock torrent))
                            (had-pieces torrent)))
        (loop with piece-index = 0
              for vector-index below pieces-length
              do (loop with bitfield = 0
                       for i from 7 downto 0
                       when (member piece-index had-pieces)
                         do (setf (ldb (byte 1 i) bitfield) 1)
                       do (incf piece-index)
                       finally (setf (svref bitfield-vector vector-index) bitfield)))
        (loop with piece-index = 0
              for vector-index below pieces-length
              do (loop with bitfield = 0
                       for i from 7 downto 0
                       if (have-piece-p torrent piece-index)
                         do (with-lock-held ((torrent-lock torrent))
                              (push piece-index (had-pieces torrent)))
                       else do (with-lock-held ((torrent-lock torrent))
                                 (push piece-index (needed-pieces torrent)))
                       do (incf piece-index)
                       finally (setf (svref bitfield-vector vector-index) bitfield))))
      (send-peer-message-length-header (1+ number-of-pieces) socket)
      (write-byte (message-id-for-message-type :bitfield) stream)
      (write-sequence bitfield-vector stream))))

(defun send-request-message (piece-index byte-offset block-length socket)
  "Sends a request message for PIECE-INDEX, with a BYTE-OFFSET byte offset
within the piece and a BLOCK-LENGTH byte offset from BYTE-OFFSET, to the peer
connected to SOCKET."
  (with-socket-stream (stream socket)
    (send-peer-message-length-header 13 socket)
    (write-byte (message-id-for-message-type :request) stream)
    (write-sequence (pad-integer-to-octets piece-index 4) stream)
    (write-sequence (pad-integer-to-octets byte-offset 4) stream)
    (write-sequence (pad-integer-to-octets block-length 4) stream)))

(defun send-piece-message (piece-index byte-offset block socket)
  "Sends a piece message where PIECE-INDEX is the piece index, BYTE-OFFSET is
the byte offset within the piece, and BLOCK is a chunk of the PIECE-INDEXth
piece."
  (with-socket-stream (stream socket)
    (let ((block-length (length block)))
      (send-peer-message-length-header (+ 9 block-length) socket)
      (write-byte (message-id-for-message-type :piece) stream)
      (write-sequence (pad-integer-to-octets piece-index 4) stream)
      (write-sequence (pad-integer-to-octets byte-offset 4) stream)
      (write-sequence block stream))))

(defun send-cancel-message (piece-index byte-offset block-length socket)
  "Sends a cancel message where PIECE-INDEX is the piece index, BYTE-OFFSET is
the byte offset within the piece, and BLOCK-LENGTH is the length of the block
to cancel in bytes, to the peer connected to SOCKET."
  (with-socket-stream (stream socket)
    (send-peer-message-length-header 13 socket)
    (write-byte (message-id-for-message-type :cancel) stream)
    (write-sequence (pad-integer-to-octets piece-index 4) stream)
    (write-sequence (pad-integer-to-octets byte-offset 4) stream)
    (write-sequence (pad-integer-to-octets block-length 4) stream)))

(defun send-port-message (socket)
  "Sends a port message to the peer connected to SOCKET, indicating the port
this DHT node is listening on."
  (with-socket-stream (stream socket)
    (send-peer-message-length-header 3 socket)
    (write-byte (message-id-for-message-type :port) stream)
    (write-sequence (port-to-octet-buffer *port* (make-octets 2)) stream)))

(defun request-piece (torrent piece-index socket)
  "Sends request messages for the PIECE-INDEXth piece of TORRENT to the peer
connected to SOCKET."
  (loop with piece-length = (gethash "piece length"
                                     (gethash "info" (torrent-info torrent)))
        with bytes-requested-so-far = 0
        with current-request-length = +length-offset+
        until (= bytes-requested-so-far piece-length)
        do (send-request-message piece-index
                                 bytes-requested-so-far
                                 current-request-length
                                 socket)
           (incf bytes-requested-so-far current-request-length)
        when (> (+ bytes-requested-so-far current-request-length) piece-length)
          do (setf current-request-length (- piece-length bytes-requested-so-far))))