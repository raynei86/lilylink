(defpackage lilylink/tests/main
  (:use :cl
        :lilylink
        :rove))
(in-package :lilylink/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :lilylink)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
