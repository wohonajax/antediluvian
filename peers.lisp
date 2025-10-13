;;;; Code related to peers

(in-package #:antediluvian)

(defclass peer ()
  ;; stream socket object for the connection to this peer
  ;; wrapped in a future (need to use (force (peer-socket peer)))
  (socket :initarg :socket :accessor peer-socket)
  ;; list of pieces this peer already has
  (had-pieces :initarg :had-pieces :accessor had-pieces)
  ;; list of piece indices this peer has requested
  (requested-pieces :initarg :requested-pieces :accessor requested-pieces)
  ;; whether we have choked this peer
  (chokedp :initform t :accessor chokedp)
  ;; whether this peer has choked us
  (choked-us-p :initform t :accessor choked-us-p)
  ;; whether we're interested in something this peer has
  (interestedp :initform nil :accessor interestedp)
  ;; whether this peer is interested in something we have
  ;; if true then requests will be incoming when we unchoke
  (interested-in-us-p :initform nil :accessor interested-in-us-p))