(in-package #:dhticl-util)

(defun make-key (seq)
  (ironclad:digest-sequence :sha1 seq))
