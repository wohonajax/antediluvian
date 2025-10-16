;;;; Code related to peers

(in-package #:antediluvian)

(defclass peer ()
  ((ip :initarg :ip :accessor peer-ip)
   (port :initarg :port :accessor peer-port)
   ;; stream socket object for the connection to this peer
   (socket :initarg :socket :accessor peer-socket)
   ;; this peer's ID
   (id :initform nil :initarg :id :accessor peer-id)
   ;; torrent this peer is associated with
   (torrent :initform nil :accessor peer-torrent)
   ;; list of pieces this peer already has
   (had-pieces :initform '() :accessor had-pieces)
   ;; list of piece indices this peer has requested
   (requested-pieces :initform '() :accessor requested-pieces)
   ;; whether we are choking this peer
   (am-choking-p :initform t :accessor am-choking-p)
   ;; whether this peer has choked us
   (choking-us-p :initform t :accessor choking-us-p)
   ;; whether we're interested in something this peer has
   (am-interested-p :initform nil :accessor am-interested-p)
   ;; whether this peer is interested in something we have
   ;; if true then requests will be incoming when we unchoke
   (interested-in-us-p :initform nil :accessor interested-in-us-p)))

(defvar *peer-list* (list)
  "A list of peer objects corresponding to peers for torrents we're downloading
or uploading.")

(defvar *peer-list-lock* (make-lock "peer list lock")
  "A lock for modifying *PEER-LIST* from multiple threads.")

(defun make-peer-socket-future (ip port)
  "Creates a future containing a socket connected to IP and PORT, or NIL if the
connection fails or times out."
  (future (handler-case (socket-connect ip port
                                        :element-type '(unsigned-byte 8)
                                        :timeout 5)
            ;; if we can't connect to the peer,
            ;; just have the future contain nil
            (connection-refused-error ())
            (timeout-error ()))))

(defun make-peer (ip port hash)
  "Creates a peer object with a socket future that attempts to connect to IP
and PORT."
  (make-instance 'peer :socket (make-peer-socket-future ip port)
                 :torrent (gethash hash *torrent-hashes*)))

(defun add-peer-to-peer-list (hash socket id)
  "Adds a peer with the given SOCKET and ID to the peer list under HASH."
  (pushnew (make-instance 'peer :socket socket :id id
                          :torrent (gethash hash *torrent-hashes*))
           *peer-list*
           :key #'peer-id :test #'equalp))

(defun clear-peer-list ()
  "Removes any peer whose socket connection failed."
  (loop for peer in *peer-list*
        unless (force (peer-socket peer))
          do (setf *peer-list* (remove peer *peer-list* :count 1))))