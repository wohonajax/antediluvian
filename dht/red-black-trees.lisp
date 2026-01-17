(in-package #:antediluvian)

(defun red-black-tree-count (red-black-tree)
  "Returns the number of elements in RED-BLACK-TREE."
  (let ((count 0))
    (red-black-tree:walk red-black-tree
                         (lambda (element)
                           (declare (ignore element))
                           (incf count)))
    count))
;;; instead of traversing the tree twice to insert an element and
;;; immediately delete it, check whether we've inserted something
;;; that will be deleted right away and just don't add it if so
(defun insert-bounded (tree item)
  (declare (optimize speed))
  (let ((not-rightmost-p nil))
    (flet ((%insert (tree node)
             (let ((sort-func (red-black-tree::sort-func tree))
                   (x (red-black-tree::root tree))
                   (y (red-black-tree::sentinel tree)))
               (mfiano-utils:while (red-black-tree::node-p x)
                 (setf y x)
                 (if (funcall sort-func (red-black-tree::key node) (red-black-tree::key x))
                     (setf x (red-black-tree::left x)
                           not-rightmost-p t)
                     (setf x (red-black-tree::right x))))
               (unless not-rightmost-p
                 (return-from insert-bounded))
               (setf (red-black-tree::parent node) y)
               (cond
                 ((not (red-black-tree::node-p y))
                  (setf (red-black-tree::root tree) node))
                 ((funcall sort-func (red-black-tree::key node) (red-black-tree::key y))
                  (setf (red-black-tree::left y) node))
                 (t
                  (setf (red-black-tree::right y) node)))
               (setf (red-black-tree::left node) (red-black-tree::sentinel tree)
                     (red-black-tree::right node) (red-black-tree::sentinel tree)
                     (red-black-tree::color node) :red)
               (red-black-tree::insert-fixup tree node))))
      (let ((node (red-black-tree::make-node tree item)))
        (%insert tree node)
        (when not-rightmost-p
          (red-black-tree:delete-node tree (red-black-tree:max (red-black-tree::root tree)))
        node)))))

(defun insert (red-black-tree item)
  "Inserts ITEM into RED-BLACK-TREE. Maintains a maximum size of k."
  (if (= (red-black-tree-count red-black-tree) +k+)
      (insert-bounded red-black-tree item)
      (red-black-tree:insert red-black-tree item)))

(defun red-black-tree-to-list (red-black-tree)
  "Converts RED-BLACK-TREE to a list."
  (loop for node = (red-black-tree:min (red-black-tree::root red-black-tree))
          then (red-black-tree:next node)
        while node
        collect (red-black-tree::value node)))