(asdf:defsystem #:dhticl
  :name "DHTiCL"
  :author "Hunter Chandler <wohonajax@gmail.com>"
  :version "20250828"
  :maintainer "Hunter Chandler <wohonajax@gmail.com>"
  :license "MIT"
  :description "DHTiCL"
  :long-description "Mainline DHT in Common Lisp"
  :components ((:file "package")
               (:file "util")
               (:file "nodes")
               (:file "tokens")
               (:file "buckets")
               (:file "krpc")
               (:file "lookups")
               (:file "networking")
               (:file "dhticl"))
  :serial t
  :depends-on (alexandria
               bencode
               bordeaux-threads
               clj-arrows
               ironclad/core
               ironclad/digests
               lparallel
               uiop
               usocket))