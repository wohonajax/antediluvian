(asdf:defsystem #:antediluvian
  :name "antediluvian"
  :version "20251231"
  :description "antediluvian"
  :long-description "Torrent client in Common Lisp"
  :author "Hunter Chandler <wohonajax@gmail.com>"
  :maintainer "Hunter Chandler <wohonajax@gmail.com>"
  :license "MIT"
  :components ((:file "package")
               (:file "settings")
               (:file "util")
               (:file "torrents")
               (:file "peers")
               (:file "files")
               (:file "peer-wire")
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
                                     (:file "dht")))
               (:file "connect")
               (:file "trackers")
               (:file "antediluvian"))
  :serial t
  :depends-on (alexandria
               bencode
               bordeaux-threads
               chanl
               dexador
               do-urlencode
               ironclad/core
               ironclad/digests
               lparallel
               magnet
               serapeum
               str
               uiop
               usocket))