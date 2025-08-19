;;;; Code related to Kademlia kbuckets

(in-package #:dhticl)

(defvar *routing-table-location*
  (merge-pathnames ".dhticl/table.sexp" (user-homedir-pathname)))
(defvar *routing-table* (list))

(defvar *peer-list* (make-hash-table :test #'equalp)
  "A hash table containing info_hashes as keys and a list of nodes as values.")

(defun save-table ()
  "Saves the routing table to a file."
  (with-open-file (file *routing-table-location*
                        :direction :output
                        :if-exists :overwrite
                        :if-does-not-exist :create)
    (format file "(")
    (map nil
         (lambda (bucket)
           (format file "(")
           (map nil
                (lambda (node)
                  (if node
                      (format file
                              "(~S ~S ~S ~S)"
                              (node-id node)
                              (node-ip node)
                              (node-port node)
                              (node-last-activity node))
                      (format file "(~S)" nil)))
                bucket)
           (format file ")"))
         *routing-table*)
    (format file ")")))

(defun load-table ()
  "Loads the routing table from the indicated location. Returns NIL and does
nothing if the specified file doesn't exist, otherwise returns the loaded
routing table."
  (when (probe-file *routing-table-location*)
    (with-open-file (file *routing-table-location*)
      (let ((empty-node (list nil)))
        (map-into *routing-table*
                  (lambda (bucket)
                    (map 'simple-vector
                         (lambda (node)
                           (unless (equal node empty-node)
                             (create-node :id (first node)
                                          :ip (second node)
                                          :port (third node)
                                          :last-activity (fourth node))))
                         bucket))
                  (read file))))))

(defun sort-table ()
  "Ensures the buckets of the routing table are sorted."
  (setf *routing-table*
    (sort *routing-table*
        (lambda (x y)
          (< (bucket-min x) (bucket-min y))))))
;;; recommended bucket size limit is 8
(defconstant +k+ 8 "Bucket size limit.")

(defstruct bucket
  (min 0 :read-only t)
  (max (expt 2 160) :read-only t)
  (nodes (make-array +k+ :initial-element nil))
  (last-changed (get-universal-time)))

(defun make-new-bucket (min max)
  "Adds a bucket to the routing table with a range from MIN to MAX."
  (let ((new-bucket (make-bucket :min min :max max)))
    (push new-bucket *routing-table*)
    new-bucket))

(defun correct-bucket (id)
  "Returns the proper bucket for ID."
  (dolist (x *routing-table*)
    (when (within id (bucket-min x) (bucket-max x))
      (return x))))

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
  (let ((nodes (bucket-nodes bucket)))
    (dotimes (i +k+ (svref nodes i))
      ;; buckets start out holding NIL until filled with nodes
      ;; sorting pushes the NILs to the back
      (when (null (svref nodes i))
        (if (zerop i)
            (return (svref nodes i))
            (return (svref nodes (1- i))))))))

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

(defun seed-buckets (first second seed)
  "Seeds the values of a bucket into 2 fresh buckets."
  (sort-bucket-by-ids seed)
  (dotimes (i +k+)
    (let ((current-node (svref (bucket-nodes seed) i)))
      (if (<= (convert-id-to-int (node-id current-node))
              (bucket-max first))
          (setf (svref (bucket-nodes first) (first-empty-slot first))
                current-node)
          (setf (svref (bucket-nodes second) (first-empty-slot second))
                current-node)))))

(defun split-bucket (bucket)
  "Splits BUCKET into two new buckets."
  (let* ((min (bucket-min bucket))
         (max (bucket-max bucket))
         (mid (truncate max 2))
         (small-bucket (make-new-bucket min mid))
         (large-bucket (make-new-bucket (1+ mid) max)))
    (setf *routing-table*
          (delete bucket *routing-table* :test #'eq))
    (push small-bucket *routing-table*)
    (push large-bucket *routing-table*)
    (seed-buckets small-bucket large-bucket bucket)
    (sort-table)))

(defun maybe-split-bucket (bucket id)
  "Splits BUCKET if ID is in its range, otherwise pings from oldest to
newest."
  (let ((target-id (convert-id-to-int id))
        (nodes (bucket-nodes bucket)))
    (flet ((node-id-as-number (node)
             (convert-id-to-int (node-id node))))
      (cond ((within target-id
                     (reduce #'min nodes :key #'node-id-as-number)
                     (reduce #'max nodes :key #'node-id-as-number))
             (split-bucket bucket))
            (t (ping-old-nodes bucket)
               (update-bucket bucket))))))

(defun add-to-bucket (node)
  "Adds NODE to the correct bucket, per its ID."
  (let* ((id (node-id node))
         (bucket (correct-bucket id)))
    (unless (dotimes (i +k+)
              (let ((current-bucket-index (svref (bucket-nodes bucket) i)))
                (cond ((equalp node current-bucket-index)
                       (return))
                      ((null current-bucket-index)
                       (setf (svref (bucket-nodes bucket) i)
                             node)
                       (sort-bucket-by-distance bucket)
                       (update-bucket bucket)
                       ;; unless this RETURN form is evaluated
                       ;; i.e., unless we add NODE to the bucket
                       (return t)))))
      ;; MAYBE-SPLIT-BUCKET only gets evaluated if the bucket was already full
      (maybe-split-bucket bucket id)
      ;; TODO: only call ADD-TO-BUCKET when MAYBE-SPLIT-BUCKET actually splits
      (add-to-bucket node))))

(defun iterate-bucket (bucket action)
  "Funcalls ACTION on each node in BUCKET."
  (let ((nodes (bucket-nodes bucket)))
    (dotimes (i (length nodes))
      (let ((node (svref nodes i)))
        (when node
          (funcall action node))))))

(defun iterate-table (action &key nodely)
  "Funcalls ACTION on each bucket in the routing table, or on each node
if NODELY is non-NIL."
  (dolist (x *routing-table*)
    (if nodely
        (iterate-bucket x action)
        (funcall action x))))

(defun find-in-table (criteria)
  "Attempts to find a node in the routing table that satisfies CRITERIA. Returns
the node if found, NIL otherwise."
  (iterate-table (lambda (node)
                   (when (funcall criteria node)
                     (return-from find-in-table node)))
                 :nodely t))

(defun find-node-in-table (id)
  "Tries to find a node in the routing table based on its ID."
  (find-in-table (lambda (x) (string= id (node-id x)))))
;;; FIXME: yuck
(defun find-closest-nodes (id)
  "Returns a list of the K closest nodes to ID."
  (let ((worst nil)
        (winners '()))
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
         (let ((distance (calculate-node-distance node id))
               (len (length winners)))
           (cond ((sorter distance worst)
                  (insert item winners #'list-sorter)
                  (setf worst distance)
                  (when (> len +k+)
                    (setf winners (butlast winners))))
                 (t (unless (< len +k+)
                      (return-from find-closest-nodes winners))))))
       :nodely t))
    winners))

(defun have-peers (info-hash)
  "Returns a list of peers for INFO-HASH."
  (let ((peer-list (gethash info-hash *peer-list*)))
    (unless peer-list
      (remhash info-hash *peer-list*))
    peer-list))