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
     (loop for socket in (wait-for-input *listening-peer-socket* :ready-only t)
           do (accept-peer-connection socket)))))

(defvar *peer-listener-thread* nil
  "A thread listening for incoming TCP connections.")

(defun accept-peer-connection (socket)
  "Accepts a peer connection from a SOCKET and listens in a new thread."
  (when-let* ((accepted-socket (socket-accept socket))
              (peer (receive-handshake accepted-socket)))
    (push (make-thread (lambda ()
                         (loop with stream = (socket-stream accepted-socket)
                               initially (send-unchoke-message accepted-socket)
                                         (setf (am-choking-p peer) nil)
                               ;; TODO: send pieces from the
                               ;; had-pieces slot of the peer
                               do (wait-for-input accepted-socket)
                                  (read-peer-wire-message peer stream))))
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
                           (setf (peer-socket peer) (try-socket-connection))
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
                                 initially (send-bitfield-message torrent socket)
                                 initially (when (supports-bittorrent-dht-p peer)
                                             (send-port-message socket))
                                 initially (send-unchoke-message socket)
                                           (setf (am-choking-p peer) nil)
                                 ;; read protocol messages
                                 unless (am-choking-p peer)
                                   do (wait-for-input socket)
                                      (read-peer-wire-message peer stream)
                                 ;; send protocol messages
                                 unless (choking-us-p peer)
                                   ;; requesting pieces
                                   when (needed-pieces torrent)
                                     do (request-piece torrent
                                                       (first (needed-pieces torrent))
                                                       socket)
                                   ;; seeding
                                   do (when-let* ((request (pop (requested-pieces peer)))
                                                  (piece-index (block-request-piece-index request))
                                                  (byte-offset (block-request-byte-offset request))
                                                  (block-length (block-request-block-length request))
                                                  (requested-block (read-chunk torrent
                                                                               piece-index
                                                                               byte-offset
                                                                               block-length
                                                                               block
                                                                               block)))
                                        (send-piece-message piece-index
                                                            byte-offset
                                                            requested-block
                                                            socket))
                                 ;; (loop-finish) takes us here
                                 finally (socket-close socket)))
                         ;; (return-from thread-block) takes us here
                         (with-lock-held (*peer-list-lock*)
                           (removef *peer-list* peer :count 1))))
          *peer-connection-threads*)))