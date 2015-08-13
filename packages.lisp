(in-package #:cl-user)

(defpackage #:dhticl-util
  (:use #:cl)
  (:export #:within
	   #:minutes-since
	   #:awhen
	   #:it
	   #:calculate-distance
	   #:convert-id-to-int
	   #:convert-id-to-hex))

(defpackage #:dhticl-nodes
  (:use #:cl
	#:dhticl-util)
  (:export #:+my-id+
	   #:node-id
	   #:node-ip
	   #:node-distance
	   #:node-last-activity
	   #:node-health
	   #:create-node))

(defpackage #:dhticl-krpc
  (:use #:cl
	#:dhticl-util
	#:dhticl-nodes)
  (:export #:*default-port*
	   #:poke-node
	   #:hit-on-node
	   #:beg-node
	   #:divulge-to-node
	   #:update-node))

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
