;;;; Code related to doubly-linked lists

(in-package #:antediluvian)

(defun insert (item doubly-linked-list predicate &key (key #'identity))
  "Inserts ITEM into DOUBLY-LINKED-LIST in the correct position such that it
will be sorted according to PREDICATE."
  (flet ((compare (item1 item2)
           (funcall predicate (funcall key item1) (funcall key item2))))
    (let ((head (doubly-linked-list:head doubly-linked-list)))
      (unless head
        (doubly-linked-list:insert doubly-linked-list item)
        (return-from insert doubly-linked-list))
      (unless (doubly-linked-list:next head)
        (doubly-linked-list:insert doubly-linked-list item :where
                                   (if (compare item (doubly-linked-list:value head))
                                       :before
                                       :after))
        (return-from insert doubly-linked-list))
      (loop for current-node = head
              then (doubly-linked-list:next current-node)
            with previous
            while current-node
            do (setf previous (doubly-linked-list:previous current-node))
            when (compare item (doubly-linked-list:value current-node))
              do (let ((new-node (doubly-linked-list::make-node :value item
                                                                :previous previous
                                                                :next current-node)))
                   (if previous
                       (setf (doubly-linked-list:next previous) new-node)
                       (setf (doubly-linked-list:head doubly-linked-list) new-node))
                   (setf (doubly-linked-list:previous current-node) new-node)
                   (return doubly-linked-list))
            ;; we've reached the end of the list
            ;; previous is still set to the second-to-last element
            ;; since assignment only occurs while current-node isn't nil
            finally (let* ((true-previous (doubly-linked-list:next previous))
                           (new-node (doubly-linked-list::make-node :value item
                                                                    :previous true-previous)))
                      (setf (doubly-linked-list:next (doubly-linked-list:next previous)) new-node
                            (doubly-linked-list:tail doubly-linked-list) new-node)
                      (return doubly-linked-list))))))

(defun pop-from-end (doubly-linked-list)
  "Removes the last element from DOUBLY-LINKED-LIST."
  (doubly-linked-list:delete doubly-linked-list t :key (constantly t) :from-end t))