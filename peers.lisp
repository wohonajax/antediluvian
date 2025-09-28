;;;; Code related to peers

(in-package #:dhticl)

(defvar *peer-list* (make-hash-table :test #'equalp)
  "A hash table containing info_hashes as keys and hash tables mapping IP
addresses to socket object futures as values. These futures will have NIL
values if the socket connection failed.")

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
     (loop for sockets = (wait-for-input *listening-peer-socket*
                                         :ready-only t)
           do (mapc (lambda (socket)
                      (push (socket-accept socket)
                            *accepted-connections*))
                    sockets)))))

(defvar *peer-listener-thread* nil
  "A thread listening for incoming TCP connections.")

(defun compact-peer-info (ip port)
  "Translates a peer's IP address and PORT into compact peer format."
  (let ((port-vec (make-array 2 :element-type '(unsigned-byte 8))))
    (concat-vec ip (port-to-octet-buffer port port-vec))))

(defun pack-values-response (info-hash)
  "Returns a list of address/port byte-vectors for peers under INFO-HASH in
compact peer format."
  (when-let (peers (gethash info-hash *peer-list*))
    (loop for socket-future being the hash-values of peers
            using (hash-key ip)
          for socket = (force socket-future)
          when socket ; if the connection failed, don't include that peer
            collect (compact-peer-info ip (get-peer-port socket)))))

(defun generate-peer-id ()
  "Generates a 20-byte peer ID for this session. Uses \"CL\" for the client
header."
  (concat-vec (map '(vector (unsigned-byte 8)) #'char-code "CL")
              (random-data 18)))

(defvar *peer-id* (generate-peer-id)
  "The peer ID for this session.")

(defun get-peer-socket (ip info-hash)
  "Returns the socket object associated with IP for a peer under INFO-HASH.
Returns NIL if there is no such peer or if the connection failed. Will block if
the connection attempt is still in progress."
  (when-let (peers (gethash info-hash *peer-list*))
    (force (gethash ip peers))))

(defun write-handshake-header (stream)
  "Writes the BitTorrent handshake header to STREAM."
  (write-byte 19 stream) ; length prefix
  (map nil (lambda (char) (write-byte (char-code char) stream))
       "BitTorrent protocol"))

(defun write-handshake-header-reserved-bytes (stream)
  "Writes the reserved bytes of the handshake that indicate protocol
extensions to STREAM."
  (dotimes (i 8)
    (write-byte 0 stream)))

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
          (remhash ip (gethash hash *peer-list*)))))
    (write-sequence *my-peer-id* stream)
    (finish-output stream)))