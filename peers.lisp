;;;; Code related to peers

(in-package #:antediluvian)

(defclass peer ()
  (socket :accessor peer-socket)
  (had-pieces :accessor had-pieces)
  (requested-pieces :accessor requested-pieces)
  (chokedp :accessor chokedp))