(cl:in-package #:cl-user)

(defpackage #:dhticl
  (:use #:cl)
  (:import-from #:alexandria
                #:clamp
                #:switch
                #:eswitch)
  (:import-from #:clj-arrows #:->>)
  (:import-from #:ironclad
                #:ascii-string-to-byte-array
                #:octets-to-integer
                #:integer-to-octets
                #:digest-sequence)
  (:import-from #:usocket
                #:socket-connect
                #:with-connected-socket
                #:port-from-octet-buffer
                #:socket-receive
                #:socket-send
                #:+max-datagram-packet-size+)
  (:export #:dht
           #:*my-id*
           #:*default-port*
           #:*use-implied-port-p*
           #:*routing-table*
           #:*routing-table-location*
           #:*settings-location*
           #:*hashes*))