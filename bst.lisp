;;; This library implements a binary search tree.
;;; Copyright 2017-2019 Guillaume LE VAILLANT
;;; This library is free software released under the GNU GPL-3 license.

(defpackage :bst
  (:use :cl)
  (:export #:*bst-copy-function*
           #:*bst-equal-p-function*
           #:*bst-lesser-p-function*
           #:+bst-empty+
           #:bst-add
           #:bst-add!
           #:bst-balance
           #:bst-balance!
           #:bst-copy
           #:bst-count
           #:bst-empty-p
           #:bst-equal-p
           #:bst-from-values
           #:bst-from-sorted-values
           #:bst-lesser-p
           #:bst-map
           #:bst-max-depth
           #:bst-max-value
           #:bst-min-depth
           #:bst-min-value
           #:bst-remove
           #:bst-remove!
           #:bst-search
           #:bst-search-max-value-below
           #:bst-search-min-value-above
           #:bst-tree-copy
           #:bst-tree-equal-p
           #:bst-values
           #:bst-values-equal-p))

(in-package :bst)


(defparameter *bst-copy-function* #'identity
  "A function used to copy a value of a tree.")
(defparameter *bst-equal-p-function* #'=
  "A function used to check if two values of a tree are equal.")
(defparameter *bst-lesser-p-function* #'<
  "A function used to check if a value of a tree is lesser than another.")
(defconstant +bst-empty+ nil
  "An empty tree is represented by NIL.")

(defstruct bst
  (value nil)
  (left +bst-empty+)
  (right +bst-empty+))

(declaim (inline bst-copy))
(defun bst-copy (value)
  "Return a copy of VALUE."
  (funcall *bst-copy-function* value))

(declaim (inline bst-equal-p))
(defun bst-equal-p (value1 value2)
  "Return T if VALUE1 and VALUE2 are equal, and NIL otherwise."
  (funcall *bst-equal-p-function* value1 value2))

(declaim (inline bst-lesser-p))
(defun bst-lesser-p (value1 value2)
  "Return T if VALUE1 is lesser than VALUE2, and NIL otherwise."
  (funcall *bst-lesser-p-function* value1 value2))

(declaim (inline bst-empty-p))
(defun bst-empty-p (tree)
  "Return T if TREE is empty and NIL otherwise."
  (eq tree +bst-empty+))

(defun bst-tree-equal-p (tree1 tree2)
  "Return T if TREE1 and TREE2 have the same structure, and NIL otherwise."
  (let ((tree1-empty-p (bst-empty-p tree1))
        (tree2-empty-p (bst-empty-p tree2)))
    (or (and tree1-empty-p tree2-empty-p)
        (and (not tree1-empty-p)
             (not tree2-empty-p)
             (bst-equal-p (bst-value tree1) (bst-value tree2))
             (bst-tree-equal-p (bst-left tree1) (bst-left tree2))
             (bst-tree-equal-p (bst-right tree1) (bst-right tree2))))))

(defun bst-search (tree value)
  "If VALUE it is present in TREE, return VALUE and T,
otherwise return NIL and NIL."
  (labels ((search-value (tree value)
             (unless (bst-empty-p tree)
               (let ((tree-value (bst-value tree)))
                 (or (bst-equal-p value tree-value)
                     (search-value (if (bst-lesser-p value tree-value)
                                       (bst-left tree)
                                       (bst-right tree))
                                   value))))))
    (if (search-value tree value)
        (values value t)
        (values nil nil))))

(defun bst-max-value (tree)
  "If TREE is not empty, return its maximum value and T,
otherwise return NIL and NIL."
  (labels ((max-value (tree)
             (let ((right (bst-right tree)))
               (if (bst-empty-p right)
                   (bst-value tree)
                   (max-value right)))))
    (if (bst-empty-p tree)
        (values nil nil)
        (values (max-value tree) t))))

(defun bst-search-max-value-below (tree value)
  "Search the maximum value in TREE lesser that VALUE. If such a value is
found, return it and T, otherwise return NIL and NIL."
  (labels ((search-value (tree value max max-p)
             (if (bst-empty-p tree)
                 (if max-p
                     (values max t)
                     (values nil nil))
                 (let ((tree-value (bst-value tree)))
                   (if (bst-lesser-p tree-value value)
                       (search-value (bst-right tree) value tree-value t)
                       (search-value (bst-left tree) value max max-p))))))
    (search-value tree value nil nil)))

(defun bst-min-value (tree)
  "If TREE is not empty, return its minimum value and T,
otherwise return NIL and NIL."
  (labels ((min-value (tree)
             (let ((left (bst-left tree)))
               (if (bst-empty-p left)
                 (bst-value tree)
                 (min-value left)))))
    (if (bst-empty-p tree)
        (values nil nil)
        (values (min-value tree) t))))

(defun bst-search-min-value-above (tree value)
  "Search the minimum value in TREE greater that VALUE. If such a value is
found, return it and T, otherwise return NIL and NIL."
  (labels ((search-value (tree value min min-p)
             (if (bst-empty-p tree)
                 (if min-p
                     (values min t)
                     (values nil nil))
                 (let ((tree-value (bst-value tree)))
                   (if (bst-lesser-p value tree-value)
                       (search-value (bst-left tree) value tree-value t)
                       (search-value (bst-right tree) value min min-p))))))
    (search-value tree value nil nil)))

(defun bst-count (tree)
  "Return the number of nodes in a TREE."
  (if (bst-empty-p tree)
      0
      (+ (bst-count (bst-left tree))
         (bst-count (bst-right tree))
         1)))

(defun bst-max-depth (tree)
  "Return the length of the longest branch in a TREE."
  (if (bst-empty-p tree)
      0
      (1+ (max (bst-max-depth (bst-left tree))
               (bst-max-depth (bst-right tree))))))

(defun bst-min-depth (tree)
  "Return the length of the shortest branch in a TREE."
  (if (bst-empty-p tree)
      0
      (1+ (min (bst-min-depth (bst-left tree))
               (bst-min-depth (bst-right tree))))))

(defun bst-tree-copy (tree)
  "Return a copy of a TREE."
  (if (bst-empty-p tree)
      +bst-empty+
      (make-bst :value (bst-copy (bst-value tree))
                :left (bst-tree-copy (bst-left tree))
                :right (bst-tree-copy (bst-right tree)))))

(defun bst-add! (tree value)
  "Insert a VALUE in a TREE. The TREE argument is destroyed."
  (labels ((add (tree value)
             (let ((tree-value (bst-value tree)))
               (unless (bst-equal-p value tree-value)
                 (if (bst-lesser-p value tree-value)
                     (let ((left (bst-left tree)))
                       (if (bst-empty-p left)
                           (setf (bst-left tree) (make-bst :value value))
                           (add left value)))
                     (let ((right (bst-right tree)))
                       (if (bst-empty-p right)
                           (setf (bst-right tree) (make-bst :value value))
                           (add right value))))))))
    (if (bst-empty-p tree)
        (make-bst :value value)
        (progn
          (add tree value)
          tree))))

(defun bst-add (tree value)
  "Insert a VALUE in a TREE."
  (bst-add! (bst-tree-copy tree) (bst-copy value)))

(defun bst-remove! (tree value)
  "Delete a VALUE from a TREE. The TREE argument is destroyed."
  (labels ((remove-value (tree value parent)
             (let* ((tree-value (bst-value tree))
                    (left (bst-left tree))
                    (left-empty-p (bst-empty-p left))
                    (right (bst-right tree))
                    (right-empty-p (bst-empty-p right)))
               (if (bst-equal-p value tree-value)
                   (if (or left-empty-p right-empty-p)
                       (let ((child (if left-empty-p right left)))
                         (if (eq tree (bst-left parent))
                             (setf (bst-left parent) child)
                             (setf (bst-right parent) child)))
                       (let ((min-value (bst-min-value right)))
                         (setf (bst-value tree) min-value)
                         (remove-value right min-value tree)))
                   (let ((child (if (bst-lesser-p value tree-value) left right)))
                     (unless (bst-empty-p child)
                       (remove-value child value tree)))))))
    (if (bst-empty-p tree)
        +bst-empty+
        (let ((parent (make-bst :value 0 :left tree)))
          (remove-value tree value parent)
          (bst-left parent)))))

(defun bst-remove (tree value)
  "Delete a VALUE from a TREE."
  (bst-remove! (bst-tree-copy tree) value))

(defun bst-map (tree function)
  "Apply a FUNCTION to each value of a TREE in ascending order."
  (unless (bst-empty-p tree)
    (bst-map (bst-left tree) function)
    (funcall function (bst-value tree))
    (bst-map (bst-right tree) function)))

(defun bst-values (tree)
  "Return all the values of a TREE in a vector."
  (let* ((count (bst-count tree))
         (values (make-array count))
         (index 0))
    (labels ((get-values (tree)
               (unless (bst-empty-p tree)
                 (get-values (bst-left tree))
                 (setf (aref values index) (bst-value tree))
                 (incf index)
                 (get-values (bst-right tree)))))
      (get-values tree)
      values)))

(defun bst-from-values (values)
  "Make a tree from a sequence of values."
  (reduce #'bst-add! values :initial-value +bst-empty+))

(defun bst-from-sorted-values (values)
  "Make a balanced tree from a vector of sorted values."
  (labels ((make-tree (values start end)
             (if (= start end)
                 +bst-empty+
                 (let ((middle (floor (+ start end) 2)))
                   (make-bst :value (aref values middle)
                             :left (make-tree values start middle)
                             :right (make-tree values (1+ middle) end))))))
    (make-tree values 0 (length values))))

(defun bst-values-equal-p (tree1 tree2)
  "Return T if TREE1 and TREE2 contain the same values, and NIL otherwise."
  (let ((values1 (bst-values tree1))
        (values2 (bst-values tree2)))
    (and (= (length values1) (length values2))
         (every #'bst-equal-p values1 values2))))

(defun bst-balance! (tree)
  "Balance a TREE. The TREE argument is destroyed."
  (bst-from-sorted-values (bst-values tree)))

(defun bst-balance (tree)
  "Balance a TREE."
  (bst-balance! (bst-tree-copy tree)))
