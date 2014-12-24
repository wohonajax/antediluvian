(in-package #:cl-user)

(defpackage #:dhticl-asd
  (:use #:cl #:asdf))

(in-package #:dhticl-asd)

(defsystem #:dhticl
  :name "DHTiCL"
  :author "Hunter Chandler <wohonajax@gmail.com>"
  :version "0.0.0"
  :maintainer "Hunter Chandler <wohonajax@gmail.com>"
  :license "MIT"
  :description "DHTiCL"
  :long-description "DHT in Common Lisp"
  :components ((:file "packages")
	       (:file "util" :depends-on ("packages")))
  :depends-on (usocket ironclad))
