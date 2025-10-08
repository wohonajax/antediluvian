;;;; Code related to the BitTorrent peer protocol

(in-package #:antediluvian)

(defun generate-peer-id ()
  "Generates a 20-byte peer ID for this session. Uses \"AD\" for the client
header (for AnteDiluvian)."
  (concat-vec (ascii-string-to-byte-array "AD") (random-data 18)))

(defvar *peer-id* (generate-peer-id)
  "The peer ID for this session.")

(defvar *listening-peer-socket* nil
  "A TCP socket listening for connections from peers.")

(defvar *accepted-connections* (list)
  "A list of sockets accepted using SOCKET-ACCEPT on the
*LISTENING-PEER-SOCKET*.")

(defun start-listener-thread ()
  "Starts a thread that will listen for incoming connections on the listening
peer socket."
  (make-thread
   (lambda ()
     ;; FIXME: wait for input in a separate thread
     (loop for socket in (wait-for-input *listening-peer-socket*
                                         :ready-only t)
           do (push (socket-accept socket) *accepted-connections*)))))

(defvar *peer-listener-thread* nil
  "A thread listening for incoming TCP connections.")

(defun get-peer-socket (ip info-hash)
  "Returns the socket object associated with IP for a peer under INFO-HASH.
Returns NIL if there is no such peer or if the connection failed. Will block if
the connection attempt is still in progress."
  (when-let (peers (gethash info-hash *peer-list*))
    (force (gethash ip peers))))

;;;; Peer wire protocol

(defconstant +length-offset+ (expt 2 14)
  "The length offset to use for request and cancel messages.")

(defun write-handshake-header (stream)
  "Writes the BitTorrent handshake header to STREAM."
  (write-byte 19 stream) ; length prefix
  (write-sequence (ascii-string-to-byte-array "BitTorrent protocol") stream))

(defun write-handshake-header-reserved-bytes (stream)
  "Writes the reserved bytes of the handshake that indicate protocol
extensions to STREAM."
  (let ((vector (make-array 8 :element-type '(unsigned-byte 8)
                            :initial-element 0)))
    (write-sequence vector stream)))

(defun perform-handshake (hash socket)
  "Performs a BitTorrent protocol handshake with the peer under HASH connected
to SOCKET."
  (let ((stream (socket-stream socket)))
    (write-handshake-header stream)
    (write-handshake-header-reserved-bytes stream)
    (write-sequence hash stream)
    (finish-output stream)
    ;; if we don't get the same hash back as
    ;; the one we send, sever the connection
    (let ((peer-hash (make-array 20 :element-type '(unsigned-byte 8))))
      (read-sequence peer-hash stream)
      (unless (equalp hash peer-hash)
        (let ((ip (get-peer-address socket)))
          (socket-close socket)
          (remhash ip (gethash hash *peer-list*)))
        (return-from perform-handshake)))
    (write-sequence *peer-id* stream)
    (finish-output stream)))

(defun byte-for-message-type (type)
  "Returns the byte to be sent to a peer for a TYPE message. TYPE must be a
keyword."
  (case type
    ;; no payload
    (:choke 0)
    (:unchoke 1)
    (:interested 2)
    (:not-interested 3)
    ;; payload
    (:have 4)
    (:bitfield 5)
    (:request 6)
    (:piece 7)
    (:cancel 8)))

(defun pad-integer-to-octets (integer length)
  "Converts INTEGER to a big-endian vector of octets of length LENGTH. Pads
with zeros from the left."
  (let ((integer-vector (integer-to-octets integer)))
    (concat-vec (make-array (- (length integer-vector) length)
                            :initial-element 0)
                integer-vector)))

(defmacro with-socket-stream ((stream-var socket) &body body)
  "Binds STREAM-VAR to the socket stream of SOCKET, executes BODY with
STREAM-VAR bound, and finally calls FINISH-OUTPUT on the stream."
  `(let ((,stream-var (socket-stream ,socket)))
     ,@body
     (finish-output ,stream-var)))

(defun send-peer-message-length-header (length socket)
  "Sends a peer wire protocol message length header to the peer connected to
SOCKET."
  (with-socket-stream (stream socket)
    (write-sequence (pad-integer-to-octets length 4) stream)))

(defun send-keep-alive-message (socket)
  "Sends a keep-alive message to the peer connected to SOCKET."
  (send-peer-message-length-header 0 socket))

(defun send-choke-message (socket)
  "Sends a choke message to the peer connected to SOCKET."
  (send-peer-message-length-header 1 socket)
  (with-socket-stream (stream socket)
    (write-byte (byte-for-message-type :choke) stream)))

(defun send-unchoke-message (socket)
  "Sends an unchoke message to the peer connected to SOCKET."
  (send-peer-message-length-header 1 socket)
  (with-socket-stream (stream socket)
    (write-byte (byte-for-message-type :unchoke) stream)))

(defun send-interested-message (socket)
  "Sends an interested message to the peer connected to SOCKET."
  (send-peer-message-length-header 1 socket)
  (with-socket-stream (stream socket)
    (write-byte (byte-for-message-type :interested) stream)))

(defun send-not-interested-message (socket)
  "Sends a not interested message to the peer connected to SOCKET."
  (send-peer-message-length-header 1 socket)
  (with-socket-stream (stream socket)
    (write-byte (byte-for-message-type :not-interested) stream)))

(defun send-have-message (piece-index socket)
  "Sends a have message to the peer connected to SOCKET stating that we have
the PIECE-INDEXth piece of a torrent."
  (send-peer-message-length-header 5 socket)
  (with-socket-stream (stream socket)
    (write-byte (byte-for-message-type :have) stream)
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
           (bitfield-vector (make-array pieces-length :initial-element 0)))
      (loop with piece-index = 0
            for vector-index below pieces-length
            do (loop with bitfield = 0
                     for i from 7 downto 0
                     do (when (have-piece-p piece-index torrent)
                          (setf (ldb (byte 1 i) bitfield) 1))
                       (incf piece-index)
                     finally (setf (svref bitfield-vector vector-index)
                                   bitfield)))
      (send-peer-message-length-header (1+ number-of-pieces) socket)
      (write-byte (byte-for-message-type :bitfield) stream)
      (write-sequence bitfield-vector stream))))