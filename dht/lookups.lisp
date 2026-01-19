;;;; Code related to find_node lookups
;;;; When a find_node query is sent, we get a response of either the target's
;;;; contact information or the k closest nodes. If we get k nodes, we send
;;;; find_node queries to each of those nodes, continuing until the k closest
;;;; nodes are no closer than the previous k closest nodes.

(in-package #:antediluvian)

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

(defun count-active-transactions (target hash-table)
  "Counts the number of active transactions under TARGET that haven't completed
yet."
  (loop for target-id being the hash-values of hash-table
        when (equalp target target-id)
          count it))

(defun active-lookups (target)
  "Returns the number of active lookups for TARGET that haven't completed yet."
  (count-active-transactions target *active-lookups*))

(defun initiate-dht-procedure (target procedure)
  "Initiates a DHT procedure for TARGET, calling PROCEDURE on each of the alpha
closest nodes in the routing table."
  (let ((alpha-closest-nodes (firstn +alpha+ (find-closest-nodes target))))
    (mapc procedure alpha-closest-nodes)))

(defun initiate-lookup (target)
  "Initiates a lookup procedure for TARGET."
  (setf (gethash target *best-lookup-results*)
        (red-black-tree:make-tree :key-func (rcurry #'calculate-node-distance target)))
  (initiate-dht-procedure target (curry #'lookup target)))

(defun recurse-on-lookup-results (target)
  "Begins lookups of TARGET using nodes in the intermediary results list."
  (loop for node in (gethash target *lookup-results-lists*)
        do (lookup target node)
        finally (remhash target *lookup-results-lists*)))

(defun push-to-results (node target hash-table)
  "Pushes NODE to the list of TARGET-related results under HASH-TABLE."
  (insert-bounded (gethash target hash-table) node))

(defun push-to-lookup-results (node target)
  "Pushes NODE to the list of lookup results under TARGET."
  (push-to-results node target *best-lookup-results*))

(defun handle-lookup-response (transaction-id target)
  "Handles a find_node response. Recursively calls find_node until the best
results are the same as the previous best results."
  (remhash transaction-id *active-lookups*)
  ;; if there are still other active lookups for this
  ;; target, don't process the lookup results yet,
  ;; since we don't have all our results
  (unless (= 0 (active-lookups target))
    (return-from handle-lookup-response))
  (when-let (results (gethash target *lookup-results-lists*))
    (mapc (rcurry #'push-to-lookup-results target) results)
    (remhash target *lookup-results-lists*))
  (let ((best-results-list (red-black-tree:list-elements (gethash target *best-lookup-results*)))
        (previous-best-results-list (red-black-tree:list-elements
                                     (gethash target *previous-best-lookup-results*))))
    (cond ;; if the previous results are the same as the
          ;; current results, stop recursion. add the
          ;; best lookup results to the routing table
          ((equalp best-results-list previous-best-results-list)
           (remhash target *previous-best-lookup-results*)
           (mapc #'maybe-add-node best-results-list)
           (remhash target *best-lookup-results*))
          ;; if the previous results aren't the same as the
          ;; current results, recurse on the results we got
          (t (shiftf (gethash target *previous-best-lookup-results*)
                     (gethash target *best-lookup-results*)
                     (red-black-tree:make-tree :key-func
                                               (rcurry #'calculate-node-distance target)))
             (mapc (curry #'lookup target) previous-best-results-list)))))

;;; Announcing peer status

(defvar *active-searches* (make-hash-table :test #'equalp)
  "A hash table mapping transaction IDs to search targets.")

(defvar *search-results* (make-hash-table :test #'equalp)
  "A hash table mapping info hashes to lists of nodes received when searching
the DHT for nearby nodes.")

(defun search-for-closest-nodes (info-hash)
  "Initiates a search of the DHT network for the k closest nodes to INFO-HASH."
  (setf (gethash info-hash *search-results*)
        (red-black-tree:make-tree :key-func (rcurry #'calculate-node-distance info-hash)))
  (initiate-dht-procedure
   info-hash
   (lambda (node)
     (let ((transaction-id (generate-transaction-id)))
       (send-message :get_peers (node-ip node) (node-port node)
                     transaction-id
                     :info-hash info-hash)
       (setf (gethash transaction-id *active-searches*) info-hash)))))

(defun active-searches (info-hash)
  "Returns the number of active search transactions for INFO-HASH."
  (count-active-transactions info-hash *active-searches*))

(defun push-to-search-results (node info-hash)
  "Pushes NODE to the search results under INFO-HASH."
  (push-to-results node info-hash *search-results*))

(defun handle-search-response (transaction-id info-hash)
  "Handles a response to a get_peers query."
  (remhash transaction-id *active-searches*)
  (unless (= (active-searches info-hash) 0)
    (return-from handle-search-response))
  (lret ((results (red-black-tree:list-elements (gethash info-hash *search-results*))))
    (remhash info-hash *search-results*)))