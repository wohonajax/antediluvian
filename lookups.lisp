;;;; Code related to find_node lookups
;;;; When a find_node query is sent, we get a response of either the target's
;;;; contact information or the k closest nodes. If we get k nodes, we send
;;;; find_node queries to each of those nodes, continuing until the k closest
;;;; nodes are no closer than the previous k closest nodes.

(in-package #:dhticl)

(defconstant +alpha+ 3
  "The number of simultaneous lookups to perform.")

(defvar *results-list* (make-hash-table :test #'equalp)
  "A hash table mapping lookup targets to lists containing nodes received from
find_node queries.")

(defvar *previous-best-results* (make-hash-table :test #'equalp))

(defvar *best-results* (make-hash-table :test #'equalp)
  "A hash table mapping lookup targets to lists containing the k best results
from find_node lookups.")

(defvar *active-lookups* (make-hash-table :test #'equalp)
  "A hash table mapping transaction IDs to currently active lookup targets.")

(defun lookup (target node)
  "Sends a lookup query of TARGET to NODE."
  (let ((transaction-id (generate-transaction-id)))
    (send-message :find_node (node-ip node) (node-port node) transaction-id
                  :id target)
    (setf (gethash transaction-id *active-lookups*) target)))

(defun ping-lookup-results (target)
  "Begins lookups of TARGET using nodes in the intermediary results list."
  (loop for node in (gethash target *results-list*)
        while (< (hash-table-count *active-lookups*) +alpha+)
        do (lookup target node)
           (pop (gethash target *results-list*))))

(defun push-to-best-results (node target)
  (insert node (gethash target *best-results*) #'<
          :key (lambda (node) (calculate-node-distance node target)))
  ;; we only want the k closest nodes
  (let ((results-list (gethash target *best-results*)))
    (when (> (length results-list) +k+)
      (setf (gethash target *best-results*) (subseq results-list 0 +k+)))))

(defun handle-lookup-response (transaction-id node target)
  "Handles a find_node response. Recursively calls find_node until the best
results are the same as the previous best results."
  (push-to-best-results node target)
  (remhash transaction-id *active-lookups*)
  (cond (*results-list* ; if *RESULTS-LIST* isn't empty
         (ping-lookup-results))
        ((equalp (gethash target *best-results*)
                 (gethash target *previous-best-results*))) ; stop recursion
        ;; FIXME: make sure we receive the K closest best results
        (t (setf (gethash target *previous-best-results*)
                 (gethash target *best-results*))
           (mapc (lambda (node) (lookup target node))
                 *best-results*))))