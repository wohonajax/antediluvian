(cl:in-package #:cl-user)

(defpackage #:dhticl
  (:use #:cl)
  (:import-from #:alexandria
                #:clamp
                #:lastcar
                #:switch
                #:eswitch)
  (:import-from #:arrows #:->>)
  (:import-from #:ironclad
                #:ascii-string-to-byte-array
                #:octets-to-integer
                #:integer-to-octets
                #:digest-sequence)
  (:export #:dht
           #:*my-id*
           #:*default-port*
           #:*use-implied-port-p*
           #:*routing-table*
           #:*routing-table-location*
           #:*settings-location*
           #:*hashes*))