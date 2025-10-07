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

(defun send-message-with-no-payload (type socket)
  "Sends a TYPE message to the peer connected to SOCKET."
  (write-byte (byte-for-message-type type) (socket-stream socket)))

(defun send-bitfield-message (socket)
  "Sends a bitfield message to the peer connected to SOCKET."
  (let ((stream (socket-stream socket))
        (bitfield 0))
    (write-byte (byte-for-message-type :bitfield) stream)
    ;;TODO: figure out how to calculate the bitfield to send
    ;; (it's based on the pieces we already have)
    ;; we'll need to send many bytes, so we need to figure out
    ;; how to construct a list of bitfields, then do
    ;; (mapc (rcurry #'write-byte stream) list-of-bitfields)
    ;; contstructing the list could be done via something like
    ;; (loop for i below (ceiling (/ file-length 8))
    ;;       collect (let ((bitfield 0))
    ;;                 (setf (ldb (byte 1 x) bitfield) 1)
    ;;                 bitfield))
    (write-byte bitfield stream)))