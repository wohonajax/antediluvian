;;;; Code related to peers

(in-package #:antediluvian)

(defclass peer ()
  (socket :initarg :socket :accessor peer-socket)
  (had-pieces :initarg :had-pieces :accessor had-pieces)
  (requested-pieces :initarg :requested-pieces :accessor requested-pieces)
  (chokedp :initarg :chokedp :accessor chokedp))