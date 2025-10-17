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
   (interested-in-us-p :initform nil :accessor interested-in-us-p)
   ;;; supported protocol extensions indicated
   ;;; by the reserved bytes in the handshake header
   (supports-azureus-messaging-protocol-p
    :initform nil
    :accessor supports-azureus-messaging-protocol-p)
   (supports-bittorrent-location-aware-protocol-p
    :initform nil
    :accessor supports-bittorrent-location-aware-protocol-p)
   (supports-libtorrent-extension-protocol-p
    :initform nil
    :accessor supports-libtorrent-extension-protocol-p)
   (supports-extension-negotiation-protocol-p
    :initform nil
    :accessor supports-extension-negotiation-protocol-p)
   (supports-bittorrent-dht-p
    :initform nil
    :accessor supports-bittorrent-dht-p)
   (supports-peer-exchange-p :initform nil :accessor supports-peer-exchange-p)
   (supports-fast-extension-p
    :initform nil
    :accessor supports-fast-extension-p)
   (supports-nat-traversal-p :initform nil :accessor supports-nat-traversal-p)))

(defvar *peer-list* (list)
  "A list of peer objects corresponding to peers for torrents we're downloading
or uploading.")

(defvar *peer-list-lock* (make-lock "peer list lock")
  "A lock for modifying *PEER-LIST* from multiple threads.")

(defun make-peer (ip port hash)
  "Creates a peer object with an IP address and PORT associated with a torrent
indicated by HASH."
  (make-instance 'peer :ip ip :port port
                 :torrent (gethash hash *torrent-hashes*)))

(defun add-peer-to-peer-list (hash socket id)
  "Adds a peer with the given SOCKET and ID to the peer list under HASH."
  (with-lock-held (*peer-list-lock*)
    (if (member id *peer-list* :key #'peer-id :test #'equalp)
        (socket-close socket)
        (push (make-instance 'peer :ip (get-peer-address socket)
                             :port (get-peer-port socket)
                             :socket socket :id id
                             :torrent (gethash hash *torrent-hashes*))
              *peer-list*))))