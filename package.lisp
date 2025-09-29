(cl:in-package #:cl-user)

(defpackage #:dhticl
  (:use #:cl)
  (:import-from #:alexandria
                #:clamp
                #:curry
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
                ;; threads
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
                #:future
                #:promise)
  (:import-from #:serapeum
                #:firstn)
  (:import-from #:uiop
                #:xdg-config-home)
  (:import-from #:usocket
                #:+max-datagram-packet-size+
                #:get-local-port
                #:get-peer-port
                #:port-from-octet-buffer
                #:port-to-octet-buffer
                #:socket-connect
                #:socket-close
                ;; datagram sockets (UDP)
                #:socket-receive
                #:socket-send
                ;; conditions
                #:connection-refused-error
                #:timeout-error)
  (:export #:add-hash
           #:dht
           ;; variables
           #:*id*
           #:*peer-id*
           #:*default-port*
           #:*use-implied-port-p*
           #:*routing-table*
           #:*peer-list*
           #:*hashes*
           ;; util
           #:concat-vec))