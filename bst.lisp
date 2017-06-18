;;; This library implements a binary search tree.
;;; Copyright 2017 Guillaume LE VAILLANT
;;; This library is free software released under the GNU GPL-3 license.

(defpackage bst
  (:use cl)
  (:export *bst-copy-function*
           *bst-equal-p-function*
           *bst-lesser-p-function*
           +bst-empty+
           bst-add
           bst-add!
           bst-balance
           bst-balance!
           bst-copy
           bst-count
           bst-empty-p
           bst-equal-p
           bst-from-values
           bst-lesser-p
           bst-max-depth
           bst-min-depth
           bst-remove
           bst-remove!
           bst-search
           bst-tree-copy
           bst-tree-equal-p
           bst-values
           bst-values-equal-p))

(in-package bst)


(defparameter *bst-copy-function* nil
  "A function used instead of IDENTITY to copy a value of a tree.")
(defparameter *bst-equal-p-function* nil
  "A function used instead of = to check if two values of a tree are equal.")
(defparameter *bst-lesser-p-function* nil
  "A function used instead of < to check if a value of a tree is
lesser than another.")
(defconstant +bst-empty+ nil
  "An empty tree is represented by NIL.")

(defstruct bst
  (value nil)
  (left +bst-empty+)
  (right +bst-empty+))

(declaim (inline bst-copy))
(defun bst-copy (value)
  "Return a copy of VALUE."
  (if (null *bst-copy-function*)
      value
      (funcall *bst-copy-function* value)))

(declaim (inline bst-equal-p))
(defun bst-equal-p (value1 value2)
  "Return T if VALUE1 and VALUE2 are equal, and NIL otherwise."
  (if (null *bst-equal-p-function*)
      (= value1 value2)
      (funcall *bst-equal-p-function* value1 value2)))

(declaim (inline bst-lesser-p))
(defun bst-lesser-p (value1 value2)
  "Return T if VALUE1 is lesser than VALUE2, and NIL otherwise."
  (if (null *bst-lesser-p-function*)
      (< value1 value2)
      (funcall *bst-lesser-p-function* value1 value2)))

(declaim (inline bst-empty-p))
(defun bst-empty-p (tree)
  "Return T if TREE is empty and NIL otherwise."
  (eq tree +bst-empty+))

(defun bst-tree-equal-p (tree1 tree2)
  "Return T if TREE1 and TREE2 have the same structure, and NIL otherwise."
  (or (and (bst-empty-p tree1) (bst-empty-p tree2))
      (and (not (bst-empty-p tree1))
           (not (bst-empty-p tree2))
           (bst-equal-p (bst-value tree1) (bst-value tree1))
           (bst-tree-equal-p (bst-left tree1) (bst-left tree2))
           (bst-tree-equal-p (bst-right tree1) (bst-right tree2)))))

(defun bst-search (tree value)
  "If VALUE it is present in TREE, return VALUE and T,
otherwise return NIL and NIL."
  (if (and (not (bst-empty-p tree))
           (or (bst-equal-p value (bst-value tree))
               (bst-search (bst-left tree) value)
               (bst-search (bst-right tree) value)))
      (values value t)
      (values nil nil)))

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
             (cond
               ((bst-equal-p value (bst-value tree))
                tree)
               ((bst-lesser-p value (bst-value tree))
                (if (bst-empty-p (bst-left tree))
                    (setf (bst-left tree) (make-bst :value value))
                    (add (bst-left tree) value)))
                (t
                 (if (bst-empty-p (bst-right tree))
                     (setf (bst-right tree) (make-bst :value value))
                     (add (bst-right tree) value))))))
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
  (labels ((min-value (tree)
             (if (bst-empty-p (bst-left tree))
                 (bst-value tree)
                 (min-value (bst-left tree))))
           (remove-value (tree value parent)
             (let ((left (bst-left tree))
                   (right (bst-right tree)))
               (if (bst-equal-p value (bst-value tree))
                   (cond
                     ((and (not (bst-empty-p left))
                           (not (bst-empty-p right)))
                      (let ((min-value (min-value right)))
                        (setf (bst-value tree) min-value)
                        (remove-value right min-value tree)))
                     ((eq tree (bst-left parent))
                      (setf (bst-left parent) (if (bst-empty-p left)
                                                  right
                                                  left)))
                     ((eq tree (bst-right parent))
                      (setf (bst-right parent) (if (bst-empty-p left)
                                                   right
                                                   left))))
                   (let ((child (if (bst-lesser-p value (bst-value tree))
                                    left
                                    right)))
                     (unless (bst-empty-p child)
                       (remove-value child value tree)))))))
    (if (bst-empty-p tree)
        +bst-empty+
        (let ((tmp (make-bst :value 0 :left tree)))
          (remove-value tree value tmp)
          (let ((tmp (bst-left tmp)))
            (if (bst-empty-p tmp)
                +bst-empty+
                (progn
                  (setf (bst-value tree) (bst-value tmp)
                        (bst-left tree) (bst-left tmp)
                        (bst-right tree) (bst-right tmp))
                  tree)))))))

(defun bst-remove (tree value)
  "Delete a VALUE from a TREE."
  (bst-remove! (bst-tree-copy tree) value))

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
  (let ((tree +bst-empty+))
    (dotimes (i (length values) tree)
      (setf tree (bst-add! tree (elt values i))))))

(defun bst-values-equal-p (tree1 tree2)
  "Return T if TREE1 and TREE2 contain the same values, and NIL otherwise."
  (let ((values1 (bst-values tree1))
        (values2 (bst-values tree2)))
    (and (= (length values1) (length values2))
         (every #'bst-equal-p tree1 tree2))))

(defun bst-balance! (tree)
  "Balance a TREE. The TREE argument is destroyed."
  (labels ((insert-values (tree values start end)
             (if (= start end)
                 tree
                 (let* ((middle (+ start (floor (- end start) 2)))
                        (median (aref values middle))
                        (tree1 (bst-add! tree median))
                        (tree2 (insert-values tree1 values start middle)))
                   (insert-values tree2 values (1+ middle) end)))))
    (let ((values (bst-values tree)))
      (insert-values +bst-empty+ values 0 (length values)))))

(defun bst-balance (tree)
  "Balance a TREE."
  (bst-balance! (bst-tree-copy tree)))
