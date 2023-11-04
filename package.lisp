(cl:in-package #:cl-user)

(defpackage #:dhticl
  (:use #:cl)
  (:export #:dht
           #:+my-id+
           #:*default-port*
           #:*routing-table*
           #:*routing-table-location*
           #:*settings-location*
           #:*hashes*))
