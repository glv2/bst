;;; This library implements a binary search tree.
;;; Copyright 2017-2018 Guillaume LE VAILLANT
;;; This library is free software released under the GNU GPL-3 license.

(defpackage :bst/test
  (:use :cl :bst :fiveam)
  (:import-from :bst #:make-bst))

(in-package :bst/test)


(def-suite bst-unit-tests :description "Unit tests for binary search tree.")

(in-suite bst-unit-tests)


(test bst-empty-p
  (is-true (bst-empty-p +bst-empty+))
  (is-false (bst-empty-p (bst-from-values '(1))))
  (is-false (bst-empty-p (bst-from-values '(3 2 1 4 5)))))

(test bst-search
  (let ((tree (bst-from-values '(3 2 1 4 5 6 7 8 9))))
    (is (bst-equal-p (bst-search tree 1) 1))
    (is (bst-equal-p (bst-search tree 3) 3))
    (is (bst-equal-p (bst-search tree 8) 8))
    (is (bst-equal-p (bst-search tree 9) 9))
    (is-false (bst-search tree 0))
    (is-false (bst-search tree 10))
    (is-false (bst-search tree 61))))

(test bst-count
  (is (= 0 (bst-count +bst-empty+)))
  (is (= 1 (bst-count (bst-from-values '(3)))))
  (is (= 5 (bst-count (bst-from-values '(1 2 3 4 5)))))
  (is (= 9 (bst-count (bst-from-values '(1 2 3 4 5 6 7 8 9))))))

(test bst-max-depth
  (let ((tree (bst-add (bst-add (bst-add (bst-add +bst-empty+ 2) 3) 4) 1)))
    (is (= 3 (bst-max-depth tree)))))

(test bst-min-depth
  (let ((tree (bst-add (bst-add (bst-add (bst-add +bst-empty+ 2) 3) 4) 1)))
    (is (= 2 (bst-min-depth tree)))))

(test bst-tree-equal-p
  (is (bst-tree-equal-p (bst-from-values '(3 2 1 4 5 6))
                        (bst-from-values '(3 2 1 4 5 6))))
  (is-false (bst-tree-equal-p (bst-from-values '(3 2 1 4 5 6))
                              (bst-from-values '(3 1 2 4 5 6)))))

(test bst-tree-copy
  (let ((tree (bst-from-values '(3 2 1 4 5 6 7 8 9))))
    (is (bst-tree-equal-p tree (bst-tree-copy tree)))))

(test bst-add
  (is (bst-tree-equal-p (make-bst :value 1)
                        (bst-add +bst-empty+ 1)))
  (is (bst-tree-equal-p (make-bst :value 2
                                  :left (make-bst :value 1))
                        (bst-add (bst-add +bst-empty+ 2) 1)))
  (is (bst-tree-equal-p (make-bst :value 2
                                  :left (make-bst :value 1)
                                  :right (make-bst :value 3))
                        (bst-add (bst-add (bst-add +bst-empty+ 2) 1) 3)))
  (is (= 6 (bst-count (bst-add (bst-from-values '(4 2 5 3 1 6)) 5)))))

(test bst-remove
  (is-true (bst-empty-p (bst-remove +bst-empty+ 10)))
  (is-true (bst-empty-p (bst-remove (bst-add +bst-empty+ 10) 10)))
  (is (bst-tree-equal-p (make-bst :value 2
                                  :right (make-bst :value 3))
                        (bst-remove (bst-from-values '(2 1 3)) 1)))
  (is (bst-tree-equal-p (make-bst :value 2
                                  :left (make-bst :value 1))
                        (bst-remove (bst-from-values '(2 1 3)) 3)))
  (is (bst-tree-equal-p (make-bst :value 3
                                  :left (make-bst :value 1))
                        (bst-remove (bst-from-values '(2 1 3)) 2))))

(test bst-values
  (flet ((vector-equal (v1 v2)
           (every #'bst-equal-p v1 v2)))
    (is (vector-equal #() (bst-values +bst-empty+)))
    (is (vector-equal #(1) (bst-values (bst-from-values '(1)))))
    (is (vector-equal #(1 2 3 4 5 6)
                      (bst-values (bst-from-values '(1 6 2 3 5 4)))))))

(test bst-from-values
  (is (bst-tree-equal-p (make-bst :value 2
                                  :left (make-bst :value 1)
                                  :right (make-bst :value 3
                                                   :right (make-bst :value 4)))
                        (bst-from-values '(2 1 3 4)))))

(test bst-values-equal-p
  (is (bst-values-equal-p (bst-from-values '(1 2 5 4 3))
                          (bst-from-values '(3 1 2 4 5)))))

(test bst-balance
  (is (bst-tree-equal-p (make-bst :value 2
                                  :left (make-bst :value 1)
                                  :right (make-bst :value 3))
                        (bst-balance (bst-from-values '(1 2 3)))))
  (is (bst-tree-equal-p (make-bst :value 4
                                  :left (make-bst :value 2
                                                  :left (make-bst :value 1)
                                                  :right (make-bst :value 3))
                                  :right (make-bst :value 6
                                                   :left (make-bst :value 5)
                                                   :right (make-bst :value 7)))
                        (bst-balance (bst-from-values '(1 2 3 7 6 4 5)))))
  (let ((*bst-copy-function* #'copy-seq)
        (*bst-equal-p-function* #'string=)
        (*bst-lesser-p-function* #'string<))
    (is (bst-tree-equal-p (make-bst :value "medium"
                                    :left (make-bst :value "less"
                                                    :left (make-bst :value "few")
                                                    :right (make-bst :value "loop"))
                                    :right (make-bst :value "more"
                                                     :left (make-bst :value "many")
                                                     :right (make-bst :value "zillion")))
                          (bst-balance (bst-from-values '("few"
                                                          "less"
                                                          "loop"
                                                          "zillion"
                                                          "more"
                                                          "medium"
                                                          "many")))))))
