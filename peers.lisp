;;;; Code related to peers

(in-package #:antediluvian)

(defclass peer ()
  (socket :accessor socket))