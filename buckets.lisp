;;;; Code related to Kademlia kbuckets

(in-package #:dhticl)

(defvar *routing-table-location*
  (xdg-config-home "dhticl/table.sexp"))
(defvar *routing-table* (list))

(defvar *peer-list* (make-hash-table :test #'equalp)
  "A hash table containing info_hashes as keys and a list of nodes as values.")
;;; recommended bucket size limit is 8
(defconstant +k+ 8
  "Replication parameter. Used for the bucket size limit, among other things.")

(defstruct bucket
  (min 0 :read-only t)
  (max (expt 2 160) :read-only t)
  (nodes (make-array +k+ :initial-element nil))
  (last-changed (get-universal-time)))

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

(defun find-in-table (criteria)
  "Attempts to find a node in the routing table that satisfies CRITERIA. Returns
the node if found, NIL otherwise."
  (iterate-table (lambda (node)
                   (when (funcall criteria node)
                     (return-from find-in-table node)))
                 :nodely t))

(defun find-node-in-table (id)
  "Tries to find a node in the routing table based on its ID. Returns the node
if successful, NIL otherwise."
  (iterate-bucket (correct-bucket id)
                  (lambda (node)
                    (and (equalp id (node-id node))
                         (return-from find-node-in-table node)))))

(defun find-closest-nodes (id)
  "Returns a list of the K closest nodes to ID."
  (let (worst winners)
    (flet ((sorter (x y)
             (cond ((and x y) (< x y))
                   (x t)
                   (t nil)))
           (list-sorter (x y)
             (cond ((and x y) (node-closer-p id x y))
                   (x t)
                   (y nil))))
      (iterate-table
       (lambda (node)
         (let ((distance (calculate-node-distance node id)))
           (when (sorter distance worst)
             (insert node winners #'list-sorter)
             (when (> (length winners) +k+)
               (setf winners (butlast winners)))
             (setf worst (calculate-node-distance (lastcar winners) id)))))
       :nodely t))
    winners))

(defun sort-table ()
  "Ensures the buckets of the routing table are sorted."
  (setf *routing-table*
        (sort *routing-table*
              (lambda (x y)
                (< (bucket-min x) (bucket-min y))))))

(defun make-new-bucket (min max)
  "Adds a bucket to the routing table with a range from MIN to MAX."
  (let ((new-bucket (make-bucket :min min :max max)))
    (push new-bucket *routing-table*)
    new-bucket))

(defun correct-bucket (id)
  "Returns the proper bucket for ID."
  (let ((id-int (convert-id-to-int id)))
    (dolist (bucket *routing-table*)
      (when (within id-int (bucket-min bucket) (bucket-max bucket))
        (return bucket)))))

(defun first-empty-slot (bucket)
  "Returns the index of the first empty slot in BUCKET."
  (dotimes (i +k+)
    (unless (svref (bucket-nodes bucket) i)
      (return i))))

(defun update-bucket (bucket)
  "Updates BUCKET's LAST-CHANGED property."
  (setf (bucket-last-changed bucket) (get-universal-time)))

(defun last-node-in-bucket (bucket)
  "Returns the last node in BUCKET."
  (->> bucket
    first-empty-slot
    1-
    (svref (bucket-nodes bucket))))

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
                               #'>)))))

  (defun sort-bucket-by-distance (bucket target)
    "Sorts BUCKET so the nodes it contains are ordered by distance from
TARGET, closest to furthest."
    (setf (bucket-nodes bucket)
          (sort (bucket-nodes bucket)
                (lambda (x y)
                  (node-sorter x y
                               (lambda (node)
                                 (calculate-distance node target))
                               #'<)))))

  (defun sort-bucket-by-ids (bucket)
    "Sorts BUCKET so the nodes it contains are ordered by ID."
    (setf (bucket-nodes bucket)
          (sort (bucket-nodes bucket)
                (lambda (x y)
                  (node-sorter x y (lambda (node)
                                     (convert-id-to-int (node-id node)))
                               #'<))))))

(defun seed-buckets (smaller larger seed)
  "Seeds the values of a bucket into 2 fresh buckets."
  (iterate-bucket
   seed
   (lambda (node)
     (if (<= (convert-id-to-int (node-id node))
             (bucket-max smaller))
         (setf (svref (bucket-nodes smaller)
                      (first-empty-slot smaller))
               node)
         (setf (svref (bucket-nodes larger)
                      (first-empty-slot larger))
               node)))))

(defun split-bucket (bucket)
  "Splits BUCKET into two new buckets."
  (let* ((min (bucket-min bucket))
         (max (bucket-max bucket))
         (mid (truncate max 2))
         (small-bucket (make-new-bucket min mid))
         (large-bucket (make-new-bucket (1+ mid) max)))
    (setf *routing-table*
          (remove bucket *routing-table* :test #'eq :count 1))
    (seed-buckets small-bucket large-bucket bucket)
    (sort-table)))

(defun maybe-split-bucket (bucket id)
  "Splits BUCKET if it's full, ID is in its range, our ID is in its range,
and ID is closer to us than the kth closest node in the routing table,
otherwise pings from oldest to newest. Returns a boolean indicating whether
BUCKET was split."
  (when (first-empty-slot bucket)
    (return-from maybe-split-bucket))
  (labels ((node-id-as-number (node)
             (convert-id-to-int (node-id node)))
           (node-distance-from-us (node)
             (calculate-node-distance node *id*))
           (kth-closest-node-to-us ()
             (extremum (find-closest-nodes *id*) #'>
                       :key #'node-distance-from-us)))
    (let* ((nodes (bucket-nodes bucket))
           (lower-bound (reduce #'min nodes :key #'node-id-as-number))
           (upper-bound (reduce #'max nodes :key #'node-id-as-number)))
      (cond ((and (within (convert-id-to-int id)
                          lower-bound
                          upper-bound)
                  (within (convert-id-to-int *id*) lower-bound upper-bound)
                  ;; if ID is closer to our ID than the kth closest node
                  (< (calculate-distance id *id*)
                     (calculate-node-distance (kth-closest-node-to-us) *id*)))
             (split-bucket bucket)
             t)
            (t (ping-old-nodes bucket)
               (update-bucket bucket)
               nil)))))

(defun add-to-bucket (node)
  "Adds NODE to the correct bucket, per its ID."
  (let* ((id (node-id node))
         (bucket (correct-bucket id)))
    (unless (dotimes (i +k+) ; if DOTIMES runs to the end it returns nil
              (let ((current-bucket-index (svref (bucket-nodes bucket) i)))
                ;; unless one of these conditions is satisfied
                (cond ((equalp node current-bucket-index)
                       (return t))
                      ((null current-bucket-index)
                       (setf (svref (bucket-nodes bucket) i)
                             node)
                       (sort-bucket-by-ids bucket)
                       (update-bucket bucket)
                       (return t)))))
      ;; if the bucket was already full
      (maybe-split-bucket bucket id)
      (add-to-bucket node))))

(defun have-peers (info-hash)
  "Returns a list of peers for INFO-HASH."
  (let ((peer-list (gethash info-hash *peer-list*)))
    (unless peer-list
      (remhash info-hash *peer-list*))
    peer-list))

(defun save-table ()
  "Saves the routing table to a file."
  (with-open-file (file *routing-table-location*
                        :direction :output
                        :if-exists :overwrite
                        :if-does-not-exist :create)
    (iterate-table (lambda (node)
                     (format file "~&(~S ~S ~S ~S)"
                             (node-id node)
                             (node-ip node)
                             (node-port node)
                             (node-last-activity node)))
                   :nodely t)))

(defun load-table ()
  "Loads the routing table from the indicated location. Returns NIL and does
nothing if the specified file doesn't exist, otherwise returns the loaded
routing table."
  (unless *routing-table*
    (make-new-bucket 0 (expt 2 160)))
  (when (probe-file *routing-table-location*)
    (with-open-file (file *routing-table-location*)
      (loop for sexp = (read file nil :eof)
            until (eql sexp :eof)
            do (create-node :id (coerce (first sexp) '(vector (unsigned-byte 8)))
                            :ip (coerce (second sexp) '(vector (unsigned-byte 8)))
                            :port (third sexp)
                            :last-activity (fourth sexp))))))