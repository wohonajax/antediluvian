;;;; Code related to find_node lookups

(in-package #:dhticl)

(defconstant +alpha+ 3
  "The number of simultaneous lookups to perform.")

(defvar *results-list* (list)
  "A list containing nodes received from find_node queries.")

(defvar *previous-best-results* (list))

(defvar *best-results* (list)
  "A list containing the k best results from a find_node lookup.")

(defvar *active-lookups* (make-hash-table :test #'equalp)
  "A hash table containing currently active lookups.")

;;; FIXME: we need to perform recursive find_node queries for lookups until
;;; the k best results are the same distance from the target as the previous
;;; k best results
(defun lookup (node)
  "Begins a lookup of NODE."
  (let ((transaction-id (generate-transaction-id)))
    (send-message :ping (node-ip node) (node-port node) transaction-id)
    (setf (gethash transaction-id *active-lookups*) node)))

(defun ping-results! ()
  "Begins looking up nodes in the intermediary results list."
  (loop for node in *results-list*
        while (< (hash-table-count *active-lookups*) +alpha+)
        do (lookup node)
           (pop *results-list*)))

(defun push-to-best-results (node target)
  (insert node *best-results* #'<
          :key (lambda (node)
                 (calculate-node-distance node target))))

(defun handle-lookup-response (transaction-id node target)
  "Handles a find_node response. Recursively calls find_node until the best
results are the same as the previous best results."
  (when (gethash transaction-id *active-lookups*)
    (cond ((= (length *best-results*) +k+)
           ;; we only want the k closest nodes
           ;; TODO: use insertion sort to avoid calling SORT on the whole list
           (unless (< (calculate-node-distance (first *best-results*) target)
                      (calculate-node-distance node target))
             (push-to-best-results node target)
             ;; TODO: handle this in PUSH-TO-BEST-RESULTS?
             (setf *best-results* (subseq *best-results* 0 +k+))))
          (t (push-to-best-results node target)))
    (remhash transaction-id *active-lookups*)
    (cond (*results-list* ; if *RESULTS-LIST* isn't empty
           (lookup (first *results-list*))
           (pop *results-list*))
          ((equalp *best-results* *previous-best-results*)) ; stop recursion
          ;; FIXME: make sure we receive the K closest best results
          (t (setf *previous-best-results* *best-results*)
             (mapc (lambda (node)
                     (send-message :find_node
                                   (node-ip node)
                                   (node-port node)
                                   (generate-transaction-id)
                                   :id target))
                   *best-results*)))))