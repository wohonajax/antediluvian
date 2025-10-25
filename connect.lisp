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
                  (connection-refused-error ()
                    (with-lock-held (*peer-list-lock*)
                      (setf *peer-list* (remove peer *peer-list* :count 1)))
                    (return-from thread-block))
                  (timeout-error ()
                    (with-lock-held (*peer-list-lock*)
                      (setf *peer-list* (remove peer *peer-list* :count 1)))
                    (return-from thread-block)))))
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
                                 unless (am-choking-p peer)
                                   do (wait-for-input socket)
                                      (read-peer-wire-message peer stream)
                                 ;; abandon the peer
                                 finally (with-lock-held (*peer-list-lock*)
                                           (socket-close socket)
                                           (setf *peer-list*
                                                 (remove peer *peer-list* :count 1)))))))
          *peer-connection-threads*)))