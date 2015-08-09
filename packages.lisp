(in-package #:cl-user)

(defpackage #:dhticl-util
  (:use #:cl)
  (:export #:within
	   #:minutes-since
	   #:awhen
	   #:it))

(defpackage #:dhticl-krpc
  (:use #:cl
	#:dhticl-util)
  (:export #:*default-port*
	   #:poke-node
	   #:hit-on-node
	   #:ask-node-for-peers
	   #:inform-node))

(defpackage #:dhticl-nodes
  (:use #:cl
	#:dhticl-util
	#:dhticl-krpc)
  (:export #:+my-id+
	   #:convert-id-to-int
	   #:convert-id-to-hex
	   #:node-id
	   #:node-ip
	   #:node-distance
	   #:node-last-activity
	   #:node-health
	   #:create-node
	   #:update-node))

(in-package #:dhticl-krpc)
(use-package '#:dhticl-nodes)
(in-package #:cl-user)

(defpackage #:dhticl-routing
  (:use #:cl
	#:dhticl-util
	#:dhticl-nodes
	#:dhticl-krpc)
  (:export #:*routing-table*
	   #:*routing-table-location*
	   #:save-table
	   #:load-table))

(defpackage #:dhticl
  (:use #:cl
	#:dhticl-util
	#:dhticl-nodes
	#:dhticl-krpc
	#:dhticl-routing)
  (:export #:dht
	   #:kill))
