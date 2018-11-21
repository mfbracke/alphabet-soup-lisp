;;;; Test file for alphabet-soup.lisp.
;;;; Uses Prove as test framework.
;;;; To execute, make sure Prove is installed and loaded and execute
;;;; the following command in Common Lisp (using the correct path):
;;;; (prove:run #P"path/to/alphabet-soup-tests.lisp")

(load (merge-pathnames "alphabet-soup.lisp" *load-truename*))

(defpackage alphabet-soup.tests
  (:use :cl
        :prove
	:alphabet-soup))
(in-package :alphabet-soup.tests)

(plan 2)


;;; Correctness tests

(subtest "Testing can-form-message-p for correct results."
  (ok (can-form-message-p "hi" "sqfisqfh"))
  (ok (not (can-form-message-p "hello" "helojqsd")))
  (ok (not (can-form-message-p "hello" "aaaaaaaaaaaajqsd")))
  (ok (not (can-form-message-p "wordl" "")))
  (ok (can-form-message-p "" "abc"))
  (ok (not (can-form-message-p "aaaa" "aaa")))
  (ok (can-form-message-p "aaaa" "aaaaa"))
  (ok (can-form-message-p "aaaa" "aaaa")))


;;; Complexity tests

(defparameter *time-tolerance-percentage* 20
  "How much the execution time may vary from the expected time, in percent.")

(defun acceptable-time-p (time expected-time)
  "Whether the time is close enough to the expected time."
  (let* ((time-tolerance (/ *time-tolerance-percentage* 100))
	 (absolute-time-tolerance (* time-tolerance expected-time))
	 (upper-limit (+ expected-time absolute-time-tolerance))
	 (lower-limit (- expected-time absolute-time-tolerance)))
    (and (< time upper-limit)
	 (< lower-limit time))))

(defun can-form-message-time-for-soup-size (size)
  "The execution time of can-form-message with the given soup size when
    the whole soup is traversed (worst case scenario)."
  (let ((time-before (get-internal-run-time)))
    (can-form-message-p "b" (make-string size :initial-element #\a))
    (- (get-internal-run-time) time-before)))


(let ((base-time (can-form-message-time-for-soup-size 1000000)))
  (subtest "Test whether the time complexity is linear."
    (ok (acceptable-time-p
	 (can-form-message-time-for-soup-size 2000000)
	 (* 2 base-time)))
    (ok (acceptable-time-p
	 (can-form-message-time-for-soup-size 5000000)
	 (* 5 base-time)))
    (ok (acceptable-time-p
	 (can-form-message-time-for-soup-size 10000000)
	 (* 10 base-time)))))
  
(finalize)
