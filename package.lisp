(cl:in-package #:cl-user)

(defpackage #:dhticl
  (:use #:cl)
  (:import-from #:alexandria
                #:clamp
                #:extremum
                #:lastcar
                #:switch
                #:eswitch)
  (:import-from #:anaphora
                #:awhen)
  (:import-from #:bordeaux-threads
                #:make-thread
                #:destroy-thread)
  (:import-from #:clj-arrows #:->>)
  (:import-from #:ironclad
                #:octets-to-integer
                #:digest-sequence
                #:random-data)
  (:import-from #:uiop
                #:xdg-config-home)
  (:import-from #:usocket
                #:socket-connect
                #:socket-close
                #:port-from-octet-buffer
                #:port-to-octet-buffer
                #:socket-receive
                #:socket-send
                #:+max-datagram-packet-size+
                #:with-connected-socket)
  (:export #:dht
           #:*id*
           #:*default-port*
           #:*use-implied-port-p*
           #:*routing-table*
           #:*hashes*))