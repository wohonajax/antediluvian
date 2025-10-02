(asdf:defsystem #:dhticl
  :name "DHTiCL"
  :author "Hunter Chandler <wohonajax@gmail.com>"
  :version "20251002"
  :maintainer "Hunter Chandler <wohonajax@gmail.com>"
  :license "MIT"
  :description "DHTiCL"
  :long-description "Mainline DHT in Common Lisp"
  :components ((:file "package")
               (:file "util")
               (:file "nodes")
               (:file "tokens")
               (:file "buckets")
               (:file "peers")
               (:file "krpc")
               (:file "routing-table")
               (:file "lookups")
               (:file "networking")
               (:file "dhticl"))
  :serial t
  :depends-on (alexandria
               bencode
               bordeaux-threads
               ironclad/core
               ironclad/digests
               lparallel
               serapeum
               uiop
               usocket))