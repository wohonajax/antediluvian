(asdf:defsystem #:antediluvian
  :name "antediluvian"
  :version "20260118"
  :description "antediluvian"
  :long-description "Torrent client in Common Lisp"
  :author "Hunter Chandler <wohonajax@gmail.com>"
  :maintainer "Hunter Chandler <wohonajax@gmail.com>"
  :license "MIT"
  :serial t
  :components ((:file "package")
               (:file "settings")
               (:file "util")
               (:file "torrents")
               (:file "peers")
               (:file "files")
               (:file "peer-wire")
               (:module "dht"
                        :serial t
                        :components ((:file "global")
                                     (:file "peers")
                                     (:file "nodes")
                                     (:file "tokens")
                                     (:file "red-black-trees")
                                     (:file "buckets")
                                     (:file "krpc")
                                     (:file "routing-table")
                                     (:file "lookups")
                                     (:file "networking")
                                     (:file "dht")))
               (:file "connect")
               (:file "trackers")
               (:file "antediluvian"))
  :depends-on (alexandria
               bencode
               bordeaux-threads
               chanl
               dexador
               do-urlencode
               filepaths
               ironclad/core
               ironclad/digests
               lparallel
               magnet
               red-black-tree
               serapeum
               uiop
               usocket))