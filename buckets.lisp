;;; TODO: fix the routing table to organize by info_hash
(in-package #:dhticl)

(defvar *routing-table-location*
  (merge-pathnames ".dhticltable" (user-homedir-pathname)))
(defvar *routing-table* (list))

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
                              "(~S ~S ~S ~S ~S)"
                              (node-id node)
                              (node-ip node)
                              (node-distance node)
                              (node-last-activity node)
                              (node-hashes node))
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
                    (map 'vector
                         (lambda (node)
                           (unless (equal node empty-node)
                             (create-node :id (first node)
                                          :ip (second node)
                                          :distance (third node)
                                          :last-activity (fourth node)
                                          :hashes (fifth node))))
                         bucket))
                  (read file))))))

;;; recommended bucket size limit is 8
(defconstant +k+ 8 "Bucket size limit.")

(defstruct bucket
  (min 0 :read-only t)
  (max (expt 2 160) :read-only t)
  (nodes (make-array +k+ :initial-element nil))
  (last-changed (get-universal-time)))

(defun make-new-bucket (min max)
  "Adds a bucket to the routing table with a range from MIN to MAX."
  (push (make-bucket :min min :max max) *routing-table*))

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

(defun bucket-emptyp (bucket)
  (every #'null (bucket-nodes bucket)))

(defun bucket-fullp (bucket)
  (notany #'null (bucket-nodes bucket)))

(defun bucket-freshness (bucket)
  "Returns the number of minutes since the last change was made to BUCKET."
  (minutes-since (bucket-last-changed bucket)))

(defun update-bucket (bucket)
  (setf (bucket-last-changed bucket) (get-universal-time)))

(defun first-node-in-bucket (bucket)
  "Returns the first node in BUCKET."
  (svref (bucket-nodes bucket) 0))

(defun last-node-in-bucket (bucket)
  "Returns the last node in BUCKET."
  (let ((len (1- +k+))
        (nodes (bucket-nodes bucket)))
    (cond ((bucket-emptyp bucket) (first-node-in-bucket bucket))
          ((bucket-fullp bucket) (svref nodes len))
          (t (dotimes (i +k+)
               (when (null (svref nodes (1+ i)))
                 (return (svref nodes i))))))))

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
          (sort (bucket-nodes bucket) (lambda (x y)
                                        (node-sorter x y                               
                                                     #'node-last-activity
                                                     #'>)))))

  (defun sort-bucket-by-distance (bucket)
    "Sorts BUCKET so the nodes it contains are ordered by distance from
closest to furthest."
    (setf (bucket-nodes bucket)
          (sort (bucket-nodes bucket) (lambda (x y)
                                        (node-sorter x y
                                                     #'node-distance
                                                     #'<))))))

(defun seed-buckets (first second seed)
  "Seeds the values of a bucket into 2 fresh buckets."
  (sort-bucket-by-distance seed)
  (dotimes (i +k+)
    (let ((current-node (svref (bucket-nodes seed) i)))
      (if (<= (ironclad:octets-to-integer
               (ironclad:ascii-string-to-byte-array
                (node-id current-node)))
              (bucket-max first))
          (setf (svref (bucket-nodes first) (first-empty-slot first))
                current-node)
          (setf (svref (bucket-nodes second) (first-empty-slot second))
                current-node)))))

(defun bucket-split (bucket &aux (min (bucket-min bucket))
                              (max (bucket-max bucket))
                              (mid (truncate max 2)))
  "Splits BUCKET into two new buckets."
  (let ((a (make-new-bucket min mid))
        (b (make-new-bucket (1+ mid) max)))
    (seed-buckets a b bucket)))

(defun bucket-splitp (bucket &aux (id (convert-id-to-int +my-id+))
                               (nodes (bucket-nodes bucket)))
  "Splits BUCKET if our ID is in its range, otherwise pings from oldest to
newest."
  (if (within id
              (reduce #'min nodes)
              (reduce #'max nodes))
      (bucket-split bucket)
      (ping-old-nodes bucket))
  (update-bucket bucket))

(defun add-to-bucket (node &aux (bucket (correct-bucket (node-id node))))
  "Adds NODE to the correct bucket, per its ID."
  (if (bucket-fullp bucket)
      (bucket-splitp bucket)
      (dotimes (i +k+)
        (when (null (svref (bucket-nodes bucket) i))
          (setf (svref (bucket-nodes bucket) i)
                node)
          (sort-bucket-by-distance bucket)
          (update-bucket bucket)
          (return t)))))

(defun iterate-bucket (bucket action)
  "Funcalls ACTION on each node in BUCKET."
  (dotimes (i (length (bucket-nodes bucket)))
    (funcall action (svref (bucket-nodes bucket) i))))

(defun iterate-table (action &key (nodely nil)
                      &aux (limit (length *routing-table*)))
  "Funcalls ACTION on each bucket in the routing table, or on each node
if NODELY is non-NIL."
  (dotimes (i limit)
    (let ((current-bucket (svref *routing-table* i)))
      (if nodely
          (iterate-bucket current-bucket action)
          (funcall action current-bucket)))))

(defmacro find-in-table (criteria &body body)
  "Attempts to find a node in the routing table that satisfies CRITERIA. If
none is found, executes BODY, otherwise returns the node."
  (alexandria:with-unique-names (target node)
    `(let ((,target nil))
       (tagbody (iterate-table (lambda (,node)
                                 (when (funcall ,criteria ,node)
                                   (setf ,target ,node)
                                   (go away)))
                               :nodely t)
        away)
       (if ,target
           ,target
           ,@body))))

(defun sort-table ()
  "Ensures the buckets of the routing table are sorted."
  (setf *routing-table*
        (sort *routing-table*
              (lambda (x y)
                (< (bucket-min x) (bucket-min y))))))

(defun find-closest-nodes (id &aux (goal (convert-id-to-int id)) (worst '())
                                (winners '()) (ticker 0))
  "Returns a list of the K closest nodes to ID."
  (flet ((sorter (x y)
           (cond ((and x y) (< x y))
                 (x t)
                 (t nil)))
         (list-sorter (x y)
           (let ((xid (when x (node-id x)))
                 (yid (when y (node-id y))))
             (cond ((and xid yid)
                    (< (calculate-distance xid goal)
                       (calculate-distance yid goal)))
                   (xid t)
                   (yid nil)
                   (x t)
                   (y nil)
                   (t t)))))
    (tagbody
       (iterate-table
        (lambda (node)
          (when node
            (let ((distance (calculate-distance (node-id node) goal))
                  (len (length winners)))
              (cond ((sorter distance worst)
                     (push node winners)
                     (setf winners (sort winners #'list-sorter)
                           worst distance)
                     (when (> len +k+)
                       (setf winners (butlast winners))))
                    (t (unless (< len +k+)
                         (incf ticker))
                       (when (> ticker 1)
                         (go away)))))))
        :nodely t)
     away))
  winners)

(defun find-node-in-table (id)
  "Tries to find a node in the routing table based on its ID. Otherwise, returns
  a list of the K closest nodes."
  (find-in-table (lambda (x) (string-equal id (node-id x)))
    (find-closest-nodes id)))

(defun have-peers (info-hash)
  "Returns a list of peers for INFO-HASH from the routing table."
  (let ((bag '()))
    (iterate-table (lambda (node)
                     (when (member info-hash (node-hashes node) :test #'equal)
                       (push node bag)))
                   :nodely t)
    bag))
