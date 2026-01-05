;;;; Code related to Kademlia kbuckets

(in-package #:antediluvian)

;;; recommended bucket size limit is 8
(defconstant +k+ 8
  "Replication parameter. Used for the bucket size limit, among other things.")

(defstruct bucket
  (min 0 :read-only t)
  (max (expt 2 160) :read-only t)
  (nodes (make-array +k+ :initial-element nil))
  (last-changed (get-universal-time)))

(defvar *routing-table* (list (make-bucket))
  "The routing table of known and added nodes.")

(defun make-new-bucket (min max)
  "Adds a bucket to the routing table with a range from MIN to MAX."
  (lret ((new-bucket (make-bucket :min min :max max)))
    (setf *routing-table*
          (insert new-bucket *routing-table* #'< :key #'bucket-min))))

(defun correct-bucket (id)
  "Returns the proper bucket for ID."
  (loop with id-int = (convert-id-to-int id)
        for bucket in *routing-table*
        when (<= (bucket-min bucket) id-int (bucket-max bucket))
          return bucket))

(defun iterate-bucket (bucket action)
  "Funcalls ACTION on each node in BUCKET."
  (loop for node across (bucket-nodes bucket)
        when node do (funcall action node)))

(defun iterate-table (action &key nodely)
  "Funcalls ACTION on each bucket in the routing table, or on each node
if NODELY is non-NIL."
  (dolist (bucket *routing-table*)
    (if nodely
        (iterate-bucket bucket action)
        (funcall action bucket))))

(defun find-node-in-table (id)
  "Tries to find a node in the routing table based on its ID. Returns the node
if successful, NIL otherwise."
  (iterate-bucket (correct-bucket id)
                  (lambda (node)
                    (when (equalp id (node-id node))
                      (return-from find-node-in-table node)))))

(defun find-closest-nodes (id)
  "Returns a list of the K closest nodes to ID."
  (let (worst winners)
    (flet ((lessp (x y)
             (cond ((and x y) (< x y))
                   (x t))))
      (iterate-table
       (lambda (node)
         (let ((distance (calculate-node-distance node id)))
           ;; if worst is set, check if the node's distance is closer
           ;; than the worst so far. if worst isn't set, the node's distance
           ;; is closer since there are no nodes in winners farther away
           (when (lessp distance worst) ; if worst is nil, this returns t
             (setf winners (insert node winners (curry #'node-closer-p id)))
             (when (> (length winners) +k+)
               (setf winners (butlast winners)))
             (setf worst (calculate-node-distance (lastcar winners) id)))))
       :nodely t))
    winners))

(defun first-empty-slot (bucket)
  "Returns the index of the first empty slot in BUCKET."
  (let ((nodes (bucket-nodes bucket)))
    (dotimes (i +k+)
      (unless (svref nodes i)
        (return i)))))

(defun update-bucket (bucket)
  "Updates BUCKET's LAST-CHANGED property."
  (setf (bucket-last-changed bucket) (get-universal-time)))

(flet ((node-sorter (x y field pred)
         (let ((xfield (when x
                         (funcall field x)))
               (yfield (when y
                         (funcall field y))))
           (cond ((and xfield yfield) (funcall pred xfield yfield))
                 (xfield t)
                 (yfield nil)
                 (x t)
                 (y nil)
                 (t t)))))

  (defun sort-bucket-by-age (bucket)
    "Sorts BUCKET so the nodes it contains are ordered from oldest to newest."
    (setf (bucket-nodes bucket)
          (sort (bucket-nodes bucket)
                (lambda (x y)
                  (node-sorter x y
                               #'node-last-activity
                               #'>))))))

(defun oldest-node-in-bucket (bucket)
  "Returns the node in BUCKET with the longest period of inactivity."
  (sort-bucket-by-age bucket)
  (svref (bucket-nodes bucket) 0))

(defun insert-into-bucket (node bucket)
  "Inserts NODE into the first empty slot in BUCKET."
  (setf (svref (bucket-nodes bucket) (first-empty-slot bucket))
        node))

(defun seed-buckets (smaller larger seed)
  "Seeds the values of a bucket into 2 fresh buckets."
  (iterate-bucket
   seed
   (lambda (node)
     (if (<= (convert-id-to-int (node-id node))
             (bucket-max smaller))
         (insert-into-bucket node smaller)
         (insert-into-bucket node larger)))))

(defun split-bucket (bucket)
  "Splits BUCKET into two new buckets."
  (let* ((min (bucket-min bucket))
         (max (bucket-max bucket))
         (mid (truncate max 2))
         (small-bucket (make-new-bucket min mid))
         (large-bucket (make-new-bucket (1+ mid) max)))
    (removef *routing-table* bucket :count 1)
    (seed-buckets small-bucket large-bucket bucket)))