;;; This library implements a binary search tree.
;;; Copyright 2017-2018 Guillaume LE VAILLANT
;;; This library is free software released under the GNU GPL-3 license.

(defsystem "bst"
  :name "bst"
  :description "Binary search tree"
  :version "1.0"
  :author "Guillaume LE VAILLANT"
  :license "GPL-3"
  :in-order-to ((test-op (test-op "bst/test")))
  :components ((:file "bst")))

(defsystem "bst/test"
  :depends-on ("bst" "fiveam")
  :version "1.0"
  :in-order-to ((test-op (load-op "bst/test")))
  :perform (test-op (o s)
             (let ((tests (uiop:find-symbol* 'bst-unit-tests :bst/test)))
               (uiop:symbol-call :fiveam 'run! tests)))
  :components ((:file "test")))
