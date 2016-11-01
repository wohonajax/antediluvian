(in-package #:cl-user)

(defpackage #:dhticl
  (:use #:cl)
  (:import-from #:alexandria
                #:with-gensyms
                #:clamp
                #:switch
                #:symbolicate
                #:define-constant)
  (:export #:dht
           #:kill
           #:+my-id+
           #:*parsed-id*
           #:*default-port*
           #:*routing-table*
           #:*routing-table-location*
           #:*settings-location*
           #:*ipv6p*))
