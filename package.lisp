(cl:in-package #:cl-user)

(defpackage #:antediluvian
  (:use #:cl)
  (:import-from #:alexandria
                #:curry
                #:eswitch
                #:extremum
                #:lastcar
                #:rcurry
                #:switch
                #:when-let
                #:when-let*)
  (:import-from #:bordeaux-threads
                ;; threads
                #:destroy-thread
                #:make-thread)
  (:import-from #:ironclad
                #:ascii-string-to-byte-array
                #:digest-sequence
                #:integer-to-octets
                #:octets-to-integer
                #-mezzano #:random-data)
  (:import-from #:lparallel
                ;; kernel
                #:end-kernel
                #:make-kernel
                ;; futures and promises
                #:force
                #:fulfill
                #:fulfilledp
                #:future
                #:promise)
  (:import-from #:serapeum
                #:firstn
                #:lret*)
  (:import-from #:str
                #:join
                #:starts-with-p)
  (:import-from #:uiop
                #:truenamize
                #:xdg-config-home)
  (:import-from #:usocket
                #:*wildcard-host*
                #:+max-datagram-packet-size+
                #:get-local-port
                #:get-peer-address
                #:get-peer-port
                #:port-from-octet-buffer
                #:port-to-octet-buffer
                #:socket-connect
                #:socket-close
                ;; datagram sockets (UDP)
                #:socket-receive
                #:socket-send
                ;; stream sockets (TCP)
                #:socket-accept
                #:socket-listen
                #:socket-stream
                #:wait-for-input
                ;; conditions
                #:connection-refused-error
                #:timeout-error)
  (:export #:add-torrent
           #:start))