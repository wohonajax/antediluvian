(in-package #:dhticl-routing)

(defvar *routing-table* (make-array 0 :fill-pointer t))

(defvar *routing-table-location*
  (merge-pathnames ".dhticltable" (user-homedir-pathname)))

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
			      (node-health node))
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
					  :health (fifth node))))
			 bucket))
		  (read file))))))

;;; recommended bucket size limit is 8
(defconstant +k+ 8 "Bucket size limit.")

;;; we make it optional so split-bucket can specify smaller
(defun make-bucket (&optional (size +k+))
  "Adds a bucket to the routing table."
  (vector-push-extend (make-array size :initial-element nil)
		      *routing-table*))

(defun ensure-bucket (bucket-designator)
  "Returns the bucket indicated by BUCKET-DESIGNATOR."
  (if (integerp bucket-designator)
      (aref *routing-table* bucket-designator)
      bucket-designator))

(defun bucket-emptyp (bucket)
  "Tests whether BUCKET is empty."
  (every #'null bucket))

(defun bucket-fullp (bucket)
  (notany #'null bucket))

(defun first-node-in-bucket (bucket)
  "Returns the first node in BUCKET."
  (aref bucket 0))

(defun last-node-in-bucket (bucket)
  "Returns the last node in BUCKET."
  (let ((len (1- +k+)))
    (cond ((bucket-emptyp bucket) (first-node-in-bucket bucket))
	  ((bucket-fullp bucket) (aref bucket len))
	  (t (dotimes (i +k+)
	       (when (null (aref bucket (1+ i)))
		 (return (aref bucket i))))))))

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
    (sort bucket (lambda (x y)
		   (node-sorter x y			       
				#'node-last-activity
				#'>))))

  (defun sort-bucket-by-distance (bucket)
    "Sorts BUCKET so the nodes it contains are ordered by distance from
  closest to furthest."
    (sort bucket (lambda (x y)
		   (node-sorter x y
				#'node-distance
				#'<)))))

(defun add-to-bucket (node bucket)
  "Adds NODE to BUCKET."
  (let ((true-bucket (ensure-bucket bucket)))
    (if (bucket-fullp true-bucket)
	(bucket-splitp true-bucket)
	(dotimes (i +k+)
	  (when (null (aref true-bucket i))
	    (setf (aref true-bucket i)
		  node)
	    (sort-bucket-by-distance true-bucket)
	    (return))))))

(defun add-to-table (node)
  "Adds NODE to the appropriate bucket in the routing table."
  (tagbody
     (let ((distance (node-distance node))
	   (tracker 0)
	   (best 0)
	   (closest))
       (iterate-table (lambda (bucket)
			(let ((clamp (alexandria:clamp
				      distance
				      (node-distance (first-node-in-bucket
						      bucket))
				      (node-distance (last-node-in-bucket
						      bucket)))))
			  (cond ((= distance clamp)
				 (add-to-bucket node bucket)
				 ;; RETURN didn't exit from the DOTIMES
				 ;; in the definition of ITERATE-TABLE
				 (go away)) ; so we have to use TAGBODY and GO
				((< clamp closest distance)
				 (setf closest (- distance clamp)
				       best tracker))
				((> clamp distance closest)
				 (setf closest (- clamp distance)
				       best tracker))
				(t t))
			  (incf tracker))))
       (add-to-bucket node best))
     away)) ; for control flow in order to directly exit the function

(defun sort-table ()
  "Ensures the buckets of the routing table are sorted by distance."
  (setf *routing-table*
	(sort *routing-table*
	      (lambda (x y)
		(< (node-distance (aref x 0))
		   (node-distance (aref y 0)))))))

;;; split bucket if our id is in its range, otherwise ping from oldest to newest
(defun bucket-splitp (bucket)
  "Determines whether to split BUCKET."
  (let ((id (convert-id-to-int +my-id+))
	(true-bucket (ensure-bucket bucket)))
    (if (within id
		(reduce #'min true-bucket)
		(reduce #'max true-bucket))
	(bucket-split true-bucket)
	(ping-old-nodes true-bucket))))

(defun bucket-split (bucket)
  "Splits BUCKET into two new buckets."
  (let ((true-bucket (ensure-bucket bucket))
	(a (make-bucket))
	(b (make-bucket)))
    (seed-buckets a b true-bucket)
    (values)))

(defun seed-buckets (first second seed)
  "Seeds the values of a bucket into 2 fresh buckets."
  (let* ((true-first (ensure-bucket first))
	 (true-second (ensure-bucket second))
	 (sorted-seed (sort-bucket-by-distance seed))
	 (split-size (floor +k+ 2)))
    (dotimes (i split-size)
      (setf (aref true-first i) (aref sorted-seed i)
	    (aref true-second i) (aref sorted-seed (+ split-size i))))))

(defun iterate-bucket (bucket action)
  "Performs ACTION on each node in BUCKET."
  (dotimes (i (length bucket))
    (funcall action (aref bucket i))))

(defun iterate-table (action &key (nodely nil))
  "Performs ACTION on each bucket in the routing table, or on each node
  if NODELY is non-NIL."
  (let ((lim (length *routing-table*)))
    (dotimes (i lim)
      (let ((current-bucket (aref *routing-table* i)))
	(if nodely
	    (iterate-bucket current-bucket action)
	    (funcall action current-bucket))))))

(defun ping-old-nodes (bucket)
  "Pings the nodes in a bucket from oldest to newest."
  (let ((true-bucket (ensure-bucket bucket)))
    (sort-bucket-by-age true-bucket)
    (iterate-bucket true-bucket #'ping-node)
    (sort-bucket-by-distance true-bucket)))

(defun purge-bad-nodes (bucket)
  "Removes all nodes of bad health from BUCKET."
  (let ((true-bucket (ensure-bucket bucket)))
    (map-into true-bucket
	      (lambda (node)
		(unless (eq :bad (node-health node))
		  node))
	      true-bucket)))

(defun handle-questionable-node (node)
  "Elucidates the health of NODE."
  (setf (node-health node)
	(cond ((ping-node node) :good)
	      ((ping-node node) :good)
	      (t :bad))))

(defun handle-questionable-nodes (bucket)
  "Handles all nodes in BUCKET that are of questionable health."
  (let ((true-bucket (ensure-bucket bucket)))
    (iterate-bucket true-bucket
		    (lambda (node)
		      (when (eq :questionable (node-health node))
			(handle-questionable-node node))))))

#| TODO: this is pseudocode of what happens
(defun closest-nodes ()
  (min (logxor (infohash torrent)
	       (ids nodes-in-routing-table))))

(when (finding node)
  (ask-for-peers (closest-nodes))
  (when (exhausted nodes-in-routing-table)
    (send +my-id+)))

(when (asked-for-peers)
  (if (have-peers)
      (send peer-info)
      (send closest-nodes)))

(let* ((secret (change-every-five-minutes))
       (token (concat (sha1 ip)
		      secret)))
  (maintain previous-secret))
|#
