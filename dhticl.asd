(in-package #:cl-user)

(defpackage #:dhticl-asd
  (:use #:cl #:asdf))

(in-package #:dhticl-asd)

;;;;; TODO: krpc.lisp, routing.lisp dhticl.lisp
(defsystem #:dhticl
  :name "DHTiCL"
  :author "Hunter Chandler <wohonajax@gmail.com>"
  :version "20150813"
  :maintainer "Hunter Chandler <wohonajax@gmail.com>"
  :license "MIT"
  :description "DHTiCL"
  :long-description "DHT in Common Lisp"
  :components ((:file "packages")
	       (:file "util" :depends-on ("packages"))
	       (:file "nodes" :depends-on ("packages" "util"))
	       (:file "krpc" :depends-on ("packages" "util" "nodes"))
	       (:file "routing" :depends-on ("packages" "util" "nodes" "krpc"))
	       (:file "dhticl" :depends-on ("packages" "util" "nodes" "krpc" "routing")))
  :depends-on (usocket bencode alexandria ironclad))
