;;;; Code related to the routing table

(in-package #:dhticl)

(defvar *replacement-cache* (list)
  "A list of nodes to potentially add to the routing table, should a bucket
contain nodes that go stale.")

(defvar *replacement-candidates* (make-hash-table :test #'equalp)
  "A hash table mapping transaction IDs to a cons whose car is a promise and
whose cdr is the node to add to the bucket.")

(defun node-replacement-check (deletion-candidate replacement-candidate)
  "Initiates a check for whether to replace DELETION-CANDIDATE with
REPLACEMENT-CANDIDATE."
  (let ((transaction-id (generate-transaction-id))
        (promise (promise)))
    (setf (gethash transaction-id *replacement-candidates*)
          (cons promise replacement-candidate))
    ;; if the promise isn't fulfilled after 10 seconds, consider it failed
    (make-thread (lambda () (sleep 10) (fulfill promise 'timeout)))
    (send-message :ping (node-ip deletion-candidate)
                  (node-port deletion-candidate)
                  transaction-id)))

(defun maybe-add-to-table (node)
  "Adds NODE to the routing table if there's an empty slot in the appropriate
bucket, otherwise checks whether to replace the least-recently-active node in
that bucket."
  (let ((bucket (correct-bucket node)))
    (sort-bucket-by-age bucket)
    (when-let (empty-slot-index (first-empty-slot bucket))
      (setf (svref bucket empty-slot-index) node)
      ;; we've added the node to the bucket; we're done
      (return-from maybe-add-to-table))
    ;; check whether to replace the least-recently-active
    ;; node in the bucket with the new node we're handling
    (node-replacement-check (svref bucket 0) node)))

(defun bucket-split-candidate-p (node bucket)
  "Tests whether BUCKET fits the criteria for being split or not. In order to
be split, a bucket's range must encapsulate both the node's ID and our ID, and
NODE must be closer to us than the kth closest node in the routing table."
  (when (first-empty-slot bucket)
    ;; bucket isn't full; don't split it
    (return-from bucket-split-candidate-p))
  (labels ((node-distance-from-us (node)
             (calculate-node-distance node *id*))
           (kth-closest-node-to-us ()
             (extremum (find-closest-nodes *id*) #'>
                       :key #'node-distance-from-us)))
    (let ((id (node-id node))
          (lower-bound (bucket-min bucket))
          (upper-bound (bucket-max bucket)))
      (and (within (convert-id-to-int id) lower-bound upper-bound)
           (within (convert-id-to-int *id*) lower-bound upper-bound)
           (< (calculate-distance id *id*)
              (node-distance-from-us (kth-closest-node-to-us)))))))

(defun maybe-add-node (node)
  "Does nothing if NODE is already in the routing table. If it isn't, splits
the bucket NODE fits into if it's a candidate for splitting. Initiates the
procedure for potentially adding a node to a bucket."
  (let ((bucket (correct-bucket node)))
    (when (contains node bucket)
      (return-from maybe-add-node))
    (when (bucket-split-candidate-p node bucket)
      (split-bucket bucket))
    (maybe-add-to-table node)))

(defun check-replacement-candidates ()
  "Checks whether to replace potentially stale nodes with replacement
candidates or add those candidates to the replacement cache."
  (maphash (lambda (transaction-id promise-node-cons)
             (let ((promise (car promise-node-cons))
                   (node (cdr promise-node-cons)))
               (when (fulfilledp promise)
                 (case (force promise)
                   (timeout (setf (svref (correct-bucket node) 0) node))
                   (response (push node *replacement-cache*)))
                 (remhash transaction-id *replacement-candidates*))))
           *replacement-candidates*))