(in-package #:dhticl-nodes)

(defconstant +my-id+ (format nil "~44,'0,':,8:x" (random (expt 2 160))))

;;; distance is bit-wise XOR
(defun node-distance (a b)
  "Returns the distance between nodes A and B."
  (logxor a b))

(defclass node ()
  ((id :accessor id
       :initarg :id
       :initform 0)
   (ip :accessor ip
       :initarg :ip
       :initform 0)
   (distance :accessor distance
	     :initarg :distance
	     :initform 0)))

(defparameter *routing-table* (make-array))

;;; size defaults to 8 (bucket size limit)
;;; we make it optional so split-bucket can specify smaller
(defun make-bucket (&optional (size 8))
  (make-array size :initial-element nil))

(defun add-to-bucket (node bucket)
  (unless (dotimes (i (length bucket))
	          (when (null (aref bucket i))
	            (setf (aref bucket i) node)
	            (return t)))
    (kbucket-splitp bucket)))

;;; split bucket if our id is in its range, otherwise ping from oldest to new
(defun kbucket-splitp (bucket)
  (if (= +my-id+
	       (clamp +my-id+
		            (min (values-list bucket))
		            (max (values-list bucket))))
      (kbucket-split bucket)
      (ping-old-nodes bucket)))

(defun kbucket-split (bucket)
  (let ((new-len (/ (length bucket)
		                2)))
    (when (> new-len 1)
      (let ((a (make-bucket new-len))
	          (b (make-bucket new-len)))
	      (seed-buckets a b bucket)
	      (values a b)))))

;;; FIXME: add new buckets to the table
(defun seed-buckets (first second seed)
  "Seeds the values of a bucket into 2 new buckets."
  (let* ((seed-len (length seed))
         (new-len (/ seed-len 2))
	       (sorted-seed (sort seed #'<)))
    (dotimes (i new-len)
      (setf (aref first i) (aref sorted-seed i)
	    (aref second i) (aref sorted-seed (+ new-len i))))))

(defun ping-old-nodes (bucket)
  "")
