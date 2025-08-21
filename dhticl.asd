(asdf:defsystem #:dhticl
  :name "DHTiCL"
  :author "Hunter Chandler <wohonajax@gmail.com>"
  :version "20250820"
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
               (:file "networking")
               (:file "dhticl"))
  :serial t
  :depends-on (usocket
               bencode
               alexandria
               ironclad/core
               ironclad/digests
               clj-arrows))