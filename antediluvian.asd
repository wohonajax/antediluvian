(asdf:defsystem #:antediluvian
  :name "antediluvian"
  :version "20251002"
  :description "antediluvian"
  :long-description "Torrent client in Common Lisp"
  :author "Hunter Chandler <wohonajax@gmail.com>"
  :maintainer "Hunter Chandler <wohonajax@gmail.com>"
  :license "MIT"
  :components ((:file "package")
               (:file "util")
               (:module "dht"
                        :serial t
                        :components ((:file "peers")
                                     (:file "nodes")
                                     (:file "tokens")
                                     (:file "buckets")
                                     (:file "krpc")
                                     (:file "routing-table")
                                     (:file "lookups")
                                     (:file "networking")
                                     (:file "dhticl")))
               (:file "peers")
               (:file "antediluvian"))
  :serial t
  :depends-on (alexandria
               babel
               bencode
               bordeaux-threads
               ironclad/core
               ironclad/digests
               lparallel
               magnet
               serapeum
               str
               uiop
               usocket))