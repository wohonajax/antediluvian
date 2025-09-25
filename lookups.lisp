;;;; Code related to find_node lookups
;;;; When a find_node query is sent, we get a response of either the target's
;;;; contact information or the k closest nodes. If we get k nodes, we send
;;;; find_node queries to each of those nodes, continuing until the k closest
;;;; nodes are no closer than the previous k closest nodes.

(in-package #:dhticl)

(defconstant +alpha+ 3
  "The number of simultaneous lookups to perform.")

(defvar *lookup-results-lists* (make-hash-table :test #'equalp)
  "A hash table mapping lookup targets to lists containing nodes received from
find_node queries.")

(defvar *previous-best-lookup-results* (make-hash-table :test #'equalp))

(defvar *best-lookup-results* (make-hash-table :test #'equalp)
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

(defun active-lookups (target)
  "Returns the number of active lookups for TARGET that haven't completed yet."
  (let ((count 0))
    (maphash (lambda (transaction-id target-id)
               (declare (ignore transaction-id))
               (when (equalp target target-id)
                 (incf count)))
             *active-lookups*)
    count))

(defun initiate-lookup (target)
  "Initiates a lookup procedure for TARGET."
  (let ((alpha-closest-nodes (firstn +alpha+ (find-closest-nodes target))))
    (mapc (lambda (node) (lookup target node))
          alpha-closest-nodes)))

(defun recurse-on-lookup-results (target)
  "Begins lookups of TARGET using nodes in the intermediary results list."
  (loop for node in (gethash target *lookup-results-lists*)
        do (lookup target node)
        finally (remhash target *lookup-results-lists*)))

(defun push-to-best-results (node target)
  (insert node (gethash target *best-lookup-results*) #'<
          :key (rcurry #'calculate-node-distance target))
  ;; we only want the k closest nodes
  (let ((results-list (gethash target *best-lookup-results*)))
    (when (> (length results-list) +k+)
      (setf (gethash target *best-lookup-results*)
            (subseq results-list 0 +k+)))))

(defun handle-lookup-response (transaction-id node target)
  "Handles a find_node response. Recursively calls find_node until the best
results are the same as the previous best results."
  (remhash transaction-id *active-lookups*)
  (unless (= 0 (active-lookups target))
    (return-from handle-lookup-response))
  (when-let (results (gethash target *lookup-results-lists*))
    (mapc (lambda (node) (push-to-best-results node target))
          results)
    (remhash target *lookup-results-lists*))
  (cond ;; if the previous results are the same as the
        ;; current results, stop recursion. add the
        ;; best lookup results to the routing table
        ((equalp (gethash target *best-lookup-results*)
                 (gethash target *previous-best-lookup-results*))
         (remhash target *previous-best-lookup-results*)
         (mapc #'maybe-add-node (gethash target *best-lookup-results*))
         (remhash target *best-lookup-results*))
        ;; FIXME: make sure we receive the K closest
        ;; best results from multiple lookup queries,
        ;; not just the response we're handling now
        (t (shiftf (gethash target *previous-best-lookup-results*)
                   (gethash target *best-lookup-results*)
                   nil)
           (mapc (lambda (node) (lookup target node))
                 (gethash target *previous-best-lookup-results*)))))