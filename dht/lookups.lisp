;;;; Code related to find_node lookups
;;;; When a find_node query is sent, we get a response of either the target's
;;;; contact information or the k closest nodes. If we get k nodes, we send
;;;; find_node queries to each of those nodes, continuing until the k closest
;;;; nodes are no closer than the previous k closest nodes.

(in-package #:antediluvian)

(defconstant +alpha+ 3
  "The number of simultaneous lookups to perform.")

(defvar *lookup-results-lists* (make-hash-table :test #'equalp)
  "A hash table mapping lookup targets to lists containing nodes received from
find_node queries.")

(defvar *previous-best-lookup-results* (make-hash-table :test #'equalp)
  "A hash table mapping lookup targets to lists containing the k best results
from the previous iterations of find_node lookups.")

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
  (loop for target-id being the hash-values of *active-lookups*
        when (equalp target target-id)
          count it))

(defun initiate-lookup (target)
  "Initiates a lookup procedure for TARGET."
  (let ((alpha-closest-nodes (firstn +alpha+ (find-closest-nodes target))))
    (mapc (curry #'lookup target) alpha-closest-nodes)))

(defun recurse-on-lookup-results (target)
  "Begins lookups of TARGET using nodes in the intermediary results list."
  (loop for node in (gethash target *lookup-results-lists*)
        do (lookup target node)
        finally (remhash target *lookup-results-lists*)))

(defun push-to-best-results (node target)
  (insert node (gethash target *best-lookup-results*) #'<
          :key (rcurry #'calculate-node-distance target))
  ;; we only want the k closest nodes
  (setf (gethash target *best-lookup-results*)
        (firstn +k+ (gethash target *best-lookup-results*))))

(defun handle-lookup-response (transaction-id target)
  "Handles a find_node response. Recursively calls find_node until the best
results are the same as the previous best results."
  (remhash transaction-id *active-lookups*)
  ;; if there are still active lookups for this
  ;; target, don't process the lookup results yet,
  ;; since we don't have all our results
  (unless (= 0 (active-lookups target))
    (return-from handle-lookup-response))
  (when-let (results (gethash target *lookup-results-lists*))
    (mapc (rcurry #'push-to-best-results target) results)
    (remhash target *lookup-results-lists*))
  (cond ;; if the previous results are the same as the
        ;; current results, stop recursion. add the
        ;; best lookup results to the routing table
        ((equalp (gethash target *best-lookup-results*)
                 (gethash target *previous-best-lookup-results*))
         (remhash target *previous-best-lookup-results*)
         (mapc #'maybe-add-node (gethash target *best-lookup-results*))
         (remhash target *best-lookup-results*))
        ;; if the previous results aren't the same as the
        ;; current results, recurse on the results we got
        (t (shiftf (gethash target *previous-best-lookup-results*)
                   (gethash target *best-lookup-results*)
                   nil)
           (mapc (curry #'lookup target)
                 (gethash target *previous-best-lookup-results*)))))
