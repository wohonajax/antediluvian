(in-package #:antediluvian)

(defconstant +k+ 8
  "Replication parameter. Used for bucket size limits, how many nodes to return
or search for, and more.")

(defconstant +alpha+ 3
  "The number of simultaneous lookups to perform.")