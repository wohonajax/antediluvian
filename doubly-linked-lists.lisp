;;;; Code related to doubly-linked lists

(in-package #:antediluvian)

(defun insert (item doubly-linked-list predicate &key (key #'identity))
  "Inserts ITEM into DOUBLY-LINKED-LIST in the correct position such that it
will be sorted according to PREDICATE."
  (flet ((compare (item1 item2)
           (funcall predicate (funcall key item1) (funcall key item2))))
    ;; loop variables aren't initialized to their
    ;; initial values before initially clauses
    ;; are run, so we need to bind the initial
    ;; value before the loop form. then we
    ;; check that binding in the initially clause
    (let ((head (doubly-linked-list:head doubly-linked-list)))
      (loop for current-node = head
              then (doubly-linked-list:next current-node)
            initially (unless head
                        (doubly-linked-list:insert doubly-linked-list item)
                        (return doubly-linked-list))
            while current-node
            when (compare item (doubly-linked-list:value current-node))
              do (doubly-linked-list:insert doubly-linked-list
                                            item
                                            :target current-node
                                            :where :before)
                 (return doubly-linked-list)
            ;; we've reached the end of the list
            finally (doubly-linked-list:insert doubly-linked-list
                                               item
                                               :target (doubly-linked-list:tail doubly-linked-list))
                    (return doubly-linked-list)))))

(defun pop-from-end (doubly-linked-list)
  "Removes the last element from DOUBLY-LINKED-LIST."
  (doubly-linked-list:delete doubly-linked-list t :key (constantly t) :from-end t))