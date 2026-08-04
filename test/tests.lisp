(defpackage #:cl-toolkit/tests
  (:use #:cl #:fiveam #:cl-toolkit-ast #:cl-toolkit-grammar #:cl-toolkit))

(in-package #:cl-toolkit/tests)

(def-suite* :cl-toolkit
  :description "cl-toolkit tests")

;;; Parse tests

(test parse-simple-list
  (let ((ast (parse-lisp-source "(+ 1 2)")))
    (is (eq :list (node-type ast)))
    (is (= 3 (length (node-children ast))))))

(test parse-nested-list
  (let ((ast (parse-lisp-source "(defun foo (x) (+ x 1))")))
    (is (eq :list (node-type ast)))
    (let ((children (node-children ast)))
      (is (= 4 (length children)))
      (is (eq :symbol (node-type (first children))))
      (is (string= "defun" (node-name (first children)))))))

(test parse-string
  (let ((ast (parse-lisp-source "\"hello\"")))
    (is (eq :string (node-type ast)))
    (is (string= "hello" (node-value ast)))))

(test parse-number
  (let ((ast (parse-lisp-source "42")))
    (is (eq :number (node-type ast)))
    (is (= 42 (node-value ast)))))

(test parse-symbol
  (let ((ast (parse-lisp-source "foo")))
    (is (eq :symbol (node-type ast)))
    (is (string= "foo" (node-name ast)))))

(test parse-comment
  (let ((ast (parse-lisp-source "; comment\n(+ 1 2)")))
    (is (eq :list (node-type ast)))))

(test parse-block-comment
  (let ((ast (parse-lisp-source "#| comment |# (+ 1 2)")))
    (is (eq :list (node-type ast)))))

;;; Recovery parse tests

(test recovery-parse-invalid
  (let ((ast (parse-with-recovery "(+ 1 2")))
    (is (eq :list (node-type ast)))))

;;; Top-level tests

(test list-top-level-forms
  (let ((forms (list-top-level (parse-lisp-source "(+ 1 2) (foo)"))))
    (is (= 2 (length forms)))))

;;; Validate tests

(test validate-balanced
  (let ((result (validate (parse-lisp-source "(+ 1 2)"))))
    (is (getf result :balanced))))

(test validate-unbalanced
  (let ((result (validate (parse-lisp-source "(+ 1 2"))))
    (is (not (getf result :balanced)))))

;;; Find tests

(test find-form-at
  (let* ((ast (parse-lisp-source "(defun foo (x)\n  (+ x 1))"))
         (found (find-form-at ast (node-source ast) 2 3)))
    (is (not (null found)))))

;;; Balance tests

(test analyze-balance-balanced
  (let ((result (analyze-balance "(+ 1 2)")))
    (is (= 0 (getf result :final-depth)))))

(test analyze-balance-unbalanced
  (let ((result (analyze-balance "(+ 1 2")))
    (is (/= 0 (getf result :final-depth)))))

;;; JSON output tests

(test node-to-json-output
  (let* ((ast (parse-lisp-source "(+ 1 2)"))
         (json (node-to-json-string ast)))
    (is (search "LIST" json))
    (is (search "children" json))))

;;; Edit operation tests

(test delete-form-at-valid
  (let* ((text "(foo)\n(bar)")
         (result (delete-form-at text 2 1)))
    (is (stringp result))
    (is (search "foo" result))))

(test insert-form-at-valid
  (let* ((text "(foo)")
         (result (insert-form-at text 1 6 "(bar)" :after nil)))
    (is (stringp result))
    (is (search "bar" result))))

(test replace-form-at-valid
  (let* ((text "(foo)")
         (result (replace-form-at text 1 2 "(baz)")))
    (is (stringp result))
    (is (search "baz" result))))
