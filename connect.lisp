;;;; Code related to peer connections over TCP

(in-package #:antediluvian)

(defvar *listening-peer-socket* nil
  "A TCP socket listening for connections from peers.")

(defvar *peer-connection-threads* (list)
  "A list of threads communicating with peer sockets.")

(defun start-listener-thread ()
  "Starts a thread that will listen for incoming connections on the listening
peer socket."
  (make-thread
   (lambda ()
     (loop for socket-list = (wait-for-input *listening-peer-socket* :ready-only t)
           do (mapc #'accept-peer-connection socket-list)))
   :name "Peer socket listener thread"))

(defvar *peer-listener-thread* nil
  "A thread listening for incoming TCP connections.")

(defun read-peer-wire-message (peer stream)
  "Reads PEER's peer wire protocol message from STREAM."
  (let* ((torrent (peer-torrent peer))
         (length (read-peer-wire-length-header stream))
         (message-bytes (make-octets length)))
    (read-sequence message-bytes stream)
    (case (message-id-to-message-type (aref message-bytes 0))
      (:choke (setf (choking-us-p peer) t))
      (:unchoke (setf (choking-us-p peer) nil))
      (:interested (setf (interested-in-us-p peer) t))
      (:not-interested (setf (interested-in-us-p peer) nil))
      (:have (pushnew (octets-to-integer (subseq message-bytes 1))
                      (had-pieces peer) :test #'=))
      ;; bitfield messages come first, so don't
      ;; worry about overwriting anything
      (:bitfield (setf (had-pieces peer)
                       (parse-bitfield (subseq message-bytes 1))))
      (:request (let* ((message-content (subseq message-bytes 1))
                       (piece-index (octets-to-integer
                                     (subseq message-content 0 4)))
                       (byte-offset (octets-to-integer
                                     (subseq message-content 4 8)))
                       (block-length (octets-to-integer
                                      (subseq message-content 8))))
                  (pushnew (make-block-request :piece-index piece-index
                                               :byte-offset byte-offset
                                               :block-length block-length)
                           (requested-pieces peer)
                           :test #'equalp)))
      (:piece (let* ((message-content (subseq message-bytes 1))
                     (piece-index (octets-to-integer
                                   (subseq message-content 0 4)))
                     (byte-offset (octets-to-integer
                                   (subseq message-content 4 8)))
                     (block (subseq message-content 8)))
                (chanl:send *write-instructions-channel*
                            (make-write-instruction :torrent torrent
                                                    :block block
                                                    :block-length (length block)
                                                    :piece-index piece-index
                                                    :byte-offset byte-offset))))
      (:cancel (let* ((message-content (subseq message-bytes 1))
                      (piece-index (octets-to-integer
                                    (subseq message-content 0 4)))
                      (byte-offset (octets-to-integer
                                    (subseq message-content 4 8)))
                      (block-length (octets-to-integer
                                     (subseq message-content 8))))
                 (removef (requested-pieces peer)
                          (make-block-request :piece-index piece-index
                                              :byte-offset byte-offset
                                              :block-length block-length)
                          :test #'equalp)))
      (:port (send-message :ping (peer-ip peer)
                           (port-from-octet-buffer (subseq message-bytes 1))
                           (generate-transaction-id))))))

(defun send-piece-to-peer (peer)
  "Sends a requested piece to PEER."
  (when-let* ((request (with-lock-held ((peer-lock peer))
                         (pop (requested-pieces peer))))
              (piece-index (block-request-piece-index request))
              (byte-offset (block-request-byte-offset request))
              (block-length (block-request-block-length request))
              (requested-block (read-chunk (peer-torrent peer)
                                           piece-index
                                           byte-offset
                                           block-length
                                           block
                                           block)))
    (send-piece-message piece-index
                        byte-offset
                        requested-block
                        (peer-socket peer))))

(defun request-had-piece (peer)
  "Requests a piece that PEER has."
  (when-let* ((torrent (peer-torrent peer))
              (piece-to-request
               (with-lock-held ((torrent-lock torrent))
                 (first (needed-pieces torrent))))
              (peer-has-piece-p
               (with-lock-held ((peer-lock peer))
                 (member piece-to-request (had-pieces peer)))))
    (request-piece torrent piece-to-request (peer-socket peer))))

(defun accept-peer-connection (socket)
  "Accepts a peer connection from a SOCKET and listens in a new thread."
  (when-let* ((accepted-socket (socket-accept socket))
              (peer (receive-handshake accepted-socket)))
    (push (make-thread (lambda ()
                         (loop with stream = (socket-stream accepted-socket)
                               with torrent = (peer-torrent peer)
                               initially (send-bitfield-message torrent accepted-socket)
                                         (when (supports-bittorrent-dht-p peer)
                                           (send-port-message accepted-socket))
                                         (send-unchoke-message accepted-socket)
                                         (setf (am-choking-p peer) nil)
                               ;; read protocol messages
                               unless (am-choking-p peer)
                                 do (wait-for-input accepted-socket)
                                    (read-peer-wire-message peer stream)
                               ;; send protocol messages
                               unless (choking-us-p peer)
                                 do (send-piece-to-peer peer)
                                    (request-had-piece peer)
                               ;; (loop-finish) takes us here
                               finally (with-lock-held ((peer-lock peer))
                                         (socket-close accepted-socket))
                                       (with-lock-held (*peer-list-lock*)
                                         (removef *peer-list* peer :count 1)))))
          *peer-connection-threads*)))

(defun initiate-peer-connection (peer)
  "Initiates a TCP socket connection with PEER."
  (macrolet ((try-socket-connection ()
               `(handler-case (socket-connect (peer-ip peer)
                                              (peer-port peer)
                                              :element-type '(unsigned-byte 8)
                                              :timeout 10)
                  ;; if the connection fails, abandon the peer
                  (connection-refused-error () (return-from thread-block))
                  (timeout-error () (return-from thread-block)))))
    (push (make-thread (lambda ()
                         (block thread-block
                           (with-lock-held ((peer-lock peer))
                             (setf (peer-socket peer) (try-socket-connection)))
                           (loop with socket = (peer-socket peer)
                                 with stream = (socket-stream socket)
                                 with torrent = (peer-torrent peer)
                                 initially (unless (perform-handshake peer)
                                             ;; perform-handshake closes the
                                             ;; socket and removes the peer
                                             ;; from the peer list if the
                                             ;; handshake fails, so in that
                                             ;; case just exit the whole block
                                             (return-from thread-block))
                                           (send-bitfield-message torrent socket)
                                           (when (supports-bittorrent-dht-p peer)
                                             (send-port-message socket))
                                           (send-unchoke-message socket)
                                           (with-lock-held ((peer-lock peer))
                                             (setf (am-choking-p peer) nil))
                                 ;; read protocol messages
                                 unless (am-choking-p peer)
                                   do (wait-for-input socket)
                                      (read-peer-wire-message peer stream)
                                 ;; send protocol messages
                                 unless (choking-us-p peer)
                                   do (request-had-piece peer)
                                      (send-piece-to-peer peer)
                                 ;; (loop-finish) takes us here
                                 finally (with-lock-held ((peer-lock peer))
                                           (socket-close socket))))
                         ;; (return-from thread-block) takes us here
                         (with-lock-held (*peer-list-lock*)
                           (removef *peer-list* peer :count 1))))
          *peer-connection-threads*)))