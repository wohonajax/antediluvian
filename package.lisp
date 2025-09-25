(cl:in-package #:cl-user)

(defpackage #:dhticl
  (:use #:cl)
  (:import-from #:alexandria
                #:clamp
                #:eswitch
                #:extremum
                #:lastcar
                #:rcurry
                #:switch
                #:when-let
                #:when-let*)
  (:import-from #:bencode
                #:*binary-key-p*
                #:decode
                #:encode)
  (:import-from #:bordeaux-threads
                #:destroy-thread
                #:make-thread)
  (:import-from #:ironclad
                #:digest-sequence
                #:octets-to-integer
                #:random-data)
  (:import-from #:lparallel
                #:force
                #:fulfill
                #:fulfilledp
                #:promise)
  (:import-from #:serapeum
                #:firstn)
  (:import-from #:uiop
                #:xdg-config-home)
  (:import-from #:usocket
                #:+max-datagram-packet-size+
                #:port-from-octet-buffer
                #:port-to-octet-buffer
                #:socket-accept
                #:socket-close
                #:socket-connect
                #:socket-listen
                #:socket-receive
                #:socket-send
                #:socket-stream)
  (:export #:dht
           #:*id*
           #:*default-port*
           #:*use-implied-port-p*
           #:*routing-table*
           #:*hashes*))