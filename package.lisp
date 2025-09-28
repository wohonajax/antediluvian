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
  (:import-from #:babel
                #:string-to-octets)
  (:import-from #:bencode
                #:*binary-key-p*
                #:decode
                #:encode)
  (:import-from #:bordeaux-threads
                ;; threads
                #:destroy-thread
                #:make-thread
                ;; locks (mutexes)
                #:make-lock
                #:with-lock-held)
  (:import-from #:ironclad
                #:digest-sequence
                #:octets-to-integer
                #:random-data)
  (:import-from #:lparallel
                #:force
                #:fulfill
                #:fulfilledp
                #:future
                #:promise)
  (:import-from #:serapeum
                #:firstn)
  (:import-from #:uiop
                #:xdg-config-home)
  (:import-from #:usocket
                #:*wildcard-host*
                #:+max-datagram-packet-size+
                #:get-peer-address
                #:get-peer-port
                #:port-from-octet-buffer
                #:port-to-octet-buffer
                #:socket-connect
                #:socket-close
                ;; stream sockets (TCP)
                #:socket-accept
                #:socket-listen
                #:socket-stream
                #:wait-for-input
                ;; datagram sockets (UDP)
                #:socket-receive
                #:socket-send
                ;; conditions
                #:connection-refused-error
                #:timeout-error)
  (:export #:dht
           #:*id*
           #:*peer-id*
           #:*default-port*
           #:*use-implied-port-p*
           #:*routing-table*
           #:*hashes*))