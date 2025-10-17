(cl:in-package #:cl-user)

(defpackage #:antediluvian
  (:use #:cl)
  (:import-from #:alexandria
                ;; general utilities
                #:extremum
                #:lastcar
                ;; higher order functions
                #:compose
                #:curry
                #:rcurry
                ;; switch
                #:eswitch
                #:switch
                ;; when-let
                #:when-let
                #:when-let*)
  (:import-from #:bordeaux-threads
                ;; threads
                #:destroy-thread
                #:make-thread
                ;; locks
                #:make-lock
                #:with-lock-held)
  (:import-from #:ironclad
                ;; digests (i.e. hash functions)
                #:digest-sequence
                ;; general utilities
                #:ascii-string-to-byte-array
                #:integer-to-octets
                #:octets-to-integer
                ;; ironclad doesn't support mezzano
                ;; we define our own random-data for it in util.lisp
                #-mezzano #:random-data)
  (:import-from #:lparallel
                ;; promises
                #:force
                #:fulfill
                #:fulfilledp
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
                ;; variables and constants
                #:*wildcard-host*
                #:+max-datagram-packet-size+
                ;; port translation utilities
                #:port-from-octet-buffer
                #:port-to-octet-buffer
                ;; general socket functions
                #:get-local-port
                #:get-peer-address
                #:get-peer-port
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