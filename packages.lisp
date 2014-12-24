(in-package #:cl-user)

(defpackage #:dhticl-util
  (:use #:cl)
  (:export #:make-key
           #:dlambda))

(defpackages #:dhticl-nodes
  (:use #:cl
        #:dhticl-util)
  (:export #:node-distance))
