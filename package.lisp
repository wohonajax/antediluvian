(in-package #:cl-user)

(defpackage #:dhticl
  (:use #:cl)
  (:export #:dht
           #:kill
           #:+my-id+
           #:*parsed-id*
           #:*default-port*
           #:*routing-table*
           #:*routing-table-location*
           #:*settings-location*
           #:*ipv6p*))
