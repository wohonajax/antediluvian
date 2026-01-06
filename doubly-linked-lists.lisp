;;;; Code related to doubly-linked lists

(in-package #:antediluvian)

(defun insert (item doubly-linked-list predicate &key (key #'identity))
  "Inserts ITEM into DOUBLY-LINKED-LIST in the correct position such that it
will be sorted according to PREDICATE."
  (flet ((compare (item1 item2)
           (funcall predicate (funcall key item1) (funcall key item2))))
    (let ((head (doubly-linked-list:head doubly-linked-list)))
      (unless head
        (return-from insert (doubly-linked-list:make-list item)))
      (unless (doubly-linked-list:next head)
        (return-from insert (let ((head-value (doubly-linked-list:value head)))
                              (if (compare item head-value)
                                  (doubly-linked-list:make-list item head-value)
                                  (doubly-linked-list:make-list head-value item))))))
    (loop for current-node = (doubly-linked-list:head doubly-linked-list)
            then (doubly-linked-list:next current-node)
          for previous = (doubly-linked-list:previous current-node)
          while current-node
          when (compare item (doubly-linked-list:value current-node))
            do (let ((new-node (doubly-linked-list::make-node :value item
                                                              :previous previous
                                                              :next current-node)))
                 (setf (doubly-linked-list:next previous) new-node
                       (doubly-linked-list:previous current-node) new-node)
                 (return doubly-linked-list))
          ;; we've reached the end of the list
          finally (let ((new-node (doubly-linked-list::make-node :value item
                                                                 :previous previous)))
                    (setf (doubly-linked-list:next previous) new-node
                          (doubly-linked-list:tail doubly-linked-list) new-node)
                    (return doubly-linked-list)))))

(defun pop-from-end (doubly-linked-list)
  "Removes the last element from DOUBLY-LINKED-LIST."
  (doubly-linked-list:delete doubly-linked-list t :key (constantly t) :from-end t))