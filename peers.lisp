;;;; Code related to peers

(in-package #:antediluvian)

(defclass peer ()
  ;; stream socket object for the connection to this peer
  ;; wrapped in a future (need to use (force (peer-socket peer)))
  ((socket :initarg :socket :accessor peer-socket)
   ;; this peer's ID
   (id :initform nil :initarg :id :accessor peer-id)
   ;; list of torrents this peer is associated with
   (torrents :initform '() :accessor peer-torrents)
   ;; list of pieces this peer already has
   (had-pieces :initform '() :accessor had-pieces)
   ;; list of piece indices this peer has requested
   (requested-pieces :initform '() :accessor requested-pieces)
   ;; whether we have choked this peer
   (chokedp :initform t :accessor chokedp)
   ;; whether this peer has choked us
   (choked-us-p :initform t :accessor choked-us-p)
   ;; whether we're interested in something this peer has
   (interestedp :initform nil :accessor interestedp)
   ;; whether this peer is interested in something we have
   ;; if true then requests will be incoming when we unchoke
   (interested-in-us-p :initform nil :accessor interested-in-us-p)))

(defvar *peer-list* (make-hash-table :test #'equalp)
  "A hash table containing info_hashes as keys and hash tables mapping IP
addresses to peer objects as values. Peer object socket slots contain futures.
These futures will have NIL values if the socket connection failed.")

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

(defun make-peer (ip port)
  "Creates a peer object with a socket future that attempts to connect to IP
and PORT."
  (make-instance 'peer :socket (make-peer-socket-future ip port)))

(defun add-peer-to-peer-list (hash socket id)
  "Adds a peer with the given SOCKET and ID to the peer list under HASH."
  (setf (gethash (get-peer-address socket) (gethash hash *peer-list*))
        (make-instance 'peer :socket socket :id id)))

(defun clear-peer-list ()
  "Removes any peer whose socket connection failed."
  (loop for peer-table being the hash-values of *peer-list*
        do (loop for peer being the hash-values of peer-table
                   using (hash-key ip)
                 unless (force (peer-socket peer))
                   do (remhash ip peer-table))))