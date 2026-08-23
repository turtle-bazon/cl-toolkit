(defpackage #:cl-toolkit/tests
  (:use #:cl #:fiveam #:cl-toolkit-ast #:cl-toolkit-grammar #:cl-toolkit))

(in-package #:cl-toolkit/tests)

(def-suite* :cl-toolkit
  :description "cl-toolkit tests")

;;; Parse tests

(test parse-simple-list
  (let ((ast (parse-lisp-source "(+ 1 2)")))
    (is (eq :list (node-type ast)))
    (is (= 1 (length (node-children ast))))
    (is (= 3 (length (node-children (first (node-children ast))))))))

(test parse-nested-list
  (let* ((ast (parse-lisp-source "(defun foo (x) (+ x 1))"))
         (form (first (node-children ast)))
         (children (node-children form)))
    (is (eq :list (node-type form)))
    (is (= 4 (length children)))
    (is (eq :symbol (node-type (first children))))
    (is (string= "defun" (node-name (first children))))))

(test parse-string
  (let ((ast (parse-lisp-source "\"hello\"")))
    (is (eq :string (node-type (first (node-children ast)))))
    (is (string= "hello" (node-value (first (node-children ast)))))))

(test parse-number
  (let ((ast (parse-lisp-source "42")))
    (is (eq :number (node-type (first (node-children ast)))))
    (is (= 42 (node-value (first (node-children ast)))))))

(test parse-symbol
  (let ((ast (parse-lisp-source "foo")))
    (is (eq :symbol (node-type (first (node-children ast)))))
    (is (string= "foo" (node-name (first (node-children ast)))))))

(test parse-comment
  (let ((ast (parse-lisp-source (format nil "; comment~%(+ 1 2)"))))
    (is (eq :list (node-type ast)))))

(test parse-block-comment
  (let ((ast (parse-lisp-source "#| comment |# (+ 1 2)")))
    (is (eq :list (node-type ast)))))

;;; Tokenizer regression tests (2026-08 symbol/number fixes)

(test symbol-with-digits
  ;; alpha2 must be ONE symbol, not symbol `alpha' + number 2
  (let* ((ast (parse-lisp-source "(defun alpha2 () 11)"))
         (form (first (node-children ast)))
         (kids (node-children form)))
    (is (eq :symbol (node-type (second kids))))
    (is (string= "alpha2" (node-name (second kids))))))

(test symbol-with-trailing-plus
  (let* ((ast (parse-lisp-source "(1+ x)"))
         (form (first (node-children ast))))
    (is (string= "1+" (node-name (first (node-children form)))))))

(test symbol-with-trailing-minus
  (let* ((ast (parse-lisp-source "(1- y)"))
         (form (first (node-children ast))))
    (is (string= "1-" (node-name (first (node-children form)))))))

(test digit-leading-symbol-not-number
  (let* ((ast (parse-lisp-source "(f 123abc)"))
         (form (first (node-children ast)))
         (arg (second (node-children form))))
    (is (eq :symbol (node-type arg)))
    (is (string= "123abc" (node-name arg)))))

(test negative-integer-is-number
  (let* ((ast (parse-lisp-source "-5"))
         (num (first (node-children ast))))
    (is (eq :number (node-type num)))
    (is (= -5 (node-value num)))))

(test signed-float-with-exponent
  (let* ((ast (parse-lisp-source "-2.5e2"))
         (num (first (node-children ast))))
    (is (eq :number (node-type num)))
    (is (= -250.0d0 (node-value num)))))

(test integer-exponent-is-float
  (let* ((ast (parse-lisp-source "1e5"))
         (num (first (node-children ast))))
    (is (eq :number (node-type num)))
    (is (= 100000.0d0 (node-value num)))))

(test minus-operator-stays-symbol
  (let* ((ast (parse-lisp-source "(- 5 3)"))
         (form (first (node-children ast))))
    (is (string= "-" (node-name (first (node-children form)))))))

(test quote-marker-has-bounds
  ;; every node needs usable start/end for source extraction
  (let* ((ast (parse-lisp-source "'foo"))
         (form (first (node-children ast)))
         (marker (first (node-children form))))
    (is (string= "QUOTE" (node-name marker)))
    (is (numberp (node-start marker)))
    (is (numberp (node-end marker)))))

(test sharp-quote-marker-has-bounds
  (let* ((ast (parse-lisp-source "#'foo"))
         (form (first (node-children ast)))
         (marker (first (node-children form))))
    (is (string= "FUNCTION" (node-name marker)))
    (is (numberp (node-start marker)))))

;;; Recovery parse tests

(test recovery-parse-invalid
  (let ((ast (parse-with-recovery "(+ 1 2")))
    (is (eq :list (node-type ast)))))

;;; Compact error messages (TUI-safety regression)

(test parse-error-value-single-line
  (let ((ast (parse-lisp-source "(unclosed")))
    (is (eq :error (node-type ast)))
    (is (not (find #\Newline (node-value ast))))
    (is (search "Syntax error at" (node-value ast)))))

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
  (let* ((text (format nil "(defun foo (x)~%  (+ x 1))"))
         (ast (parse-lisp-source text))
         (found (find-form-at ast text 0 0)))
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

;;; Edit operation tests (0-based line/col)

(test delete-form-at-valid
  (let* ((text (format nil "(foo)~%(bar)"))
         (result (delete-form-at text 1 0)))
    (is (stringp result))
    (is (search "foo" result))
    (not (search "bar" result))))

(test insert-form-at-valid
  (let* ((text "(foo)")
         (result (insert-form-at text 0 0 "(bar)")))
    (is (stringp result))
    (is (search "bar" result))))

(test replace-form-at-valid
  (let* ((text "(foo)")
         (result (replace-form-at text 0 0 "(baz)")))
    (is (stringp result))
    (is (search "baz" result))))

;;; Name-based targeting regressions

(test find-top-level-by-name-basic
  (let ((node (find-top-level-by-name (format nil "(defun alpha () 1)~%(defun beta () 2)") "beta")))
    (is (not (null node)))
    (is (string= "beta" (node-form-name node)))))

(test source-of-top-level-by-name
  (let ((src (source-of-top-level (format nil "(defun a ()~%  1)~%(defun b () 2)") :name "a")))
    (is (string= src (format nil "(defun a ()~%  1)")))))

(test source-of-top-level-by-index-and-end
  (let ((text (format nil "(defun a () 1)~%(defun b () 2)")))
    (is (string= (source-of-top-level text :index 1) "(defun b () 2)"))
    (is (string= (source-of-top-level text :end t) "(defun b () 2)"))))

(test find-subform-matching-exact-preferred
  (let* ((text "(defun f () (g (h 1)))")
         (top (first (list-top-level (parse-lisp-source text))))
         (sub (find-subform-matching top text "(h 1)")))
    (is (not (null sub)))
    (is (string= (node-source-text text sub) "(h 1)"))))

(test find-subform-matching-smallest-exact
  ;; two exact matches at different sizes -> smallest wins
  (let* ((text "(defun f () (* x 3) (* y (* x 3)))")
         (top (first (list-top-level (parse-lisp-source text))))
         (sub (find-subform-matching top text "(* x 3)")))
    (is (string= (node-source-text text sub) "(* x 3)"))))

(test replace-node-with-code-splice
  (let* ((text (format nil "(foo)~%(bar)"))
         (node (find-top-level-by-name text "bar"))
         (result (cl-toolkit::splice-replacement text node "(baz)")))
    (is (string= result (format nil "(foo)~%(baz)")))))

;;; Batch edit regressions

(test batch-descending-sort-deletes
  ;; deleting indices {1,3} in one batch must remove bar and qux
  (let* ((text (format nil "(defun foo () 1)~%(defun bar () 2)~%(defun baz () 3)~%(defun qux () 4)~%(defun quux () 5)"))
         (result (apply-batch-edits text
                                    (list (list :operation :delete-index :index 1)
                                          (list :operation :delete-index :index 3)))))
    (is (null (find-top-level-by-name result "bar")))
    (is (null (find-top-level-by-name result "qux")))
    (is (not (null (find-top-level-by-name result "foo"))))
    (is (not (null (find-top-level-by-name result "quux"))))))

(test batch-replace-and-insert-mixed
  (let* ((text (format nil "(defun a () 1)~%(defun b () 2)~%(defun c () 3)"))
         (result (apply-batch-edits text
                                    (list (list :operation :replace-index :index 2 :code "(defun c2 () 33)")
                                          (list :operation :insert-after-index :index 1 :code (format nil "~%(defun b2 () 22)"))))))
    (is (not (null (find-top-level-by-name result "c2"))))
    (is (not (null (find-top-level-by-name result "b2"))))
    (is (null (find-top-level-by-name result "c")))))

;;; Whitespace-jam repair

(test split-jammed-forms
  (let* ((text (format nil "(defun one () 1)~%(defun two () 2)(defun three () 3)"))
         (result (split-jammed-top-level text)))
    (is (search (format nil "(defun two () 2)~%(defun three () 3)") result))
    ;; already-separated forms untouched
    (is (search (format nil "(defun one () 1)~%") result))))

(test split-jammed-idempotent
  (let* ((text "(defun two () 2)(defun three () 3)")
         (once (split-jammed-top-level text)))
    (is (string= once (split-jammed-top-level once)))))

;;; Pretty replacement

(test replace-form-pretty-preserves-indent
  (let* ((text (format nil "  (defun foo ()~%    1)"))
         (node (find-top-level-by-name text "foo"))
         (result (replace-form-pretty text node (format nil "(defun bar ()~%  2)"))))
    ;; first line keeps original base indent of two spaces
    (is (string= "  (defun bar" (subseq result 0 12)))))

;;; 0.3.0 analysis-layer regressions

(test count-text-occurrences-basic
  (multiple-value-bind (count off)
      (count-text-occurrences "(a)(a)(b)" "(a)")
    (is (= 2 count))
    (is (= 0 off))))

(test count-text-occurrences-none
  (multiple-value-bind (count off)
      (count-text-occurrences "(a)" "(zzz)")
    (is (= 0 count))
    (is (= -1 off))))

(test net-depth-delta-balanced
  (is (= 0 (net-depth-delta "(a)" "(b)"))))

(test net-depth-delta-shift
  (is (= 1 (net-depth-delta "(a)" "(+ a"))))

(test duplicate-top-level-forms-detects
  (let* ((text "(in-package :cl)
(defun a () 1)
(in-package :cl)")
         (groups (duplicate-top-level-forms text)))
    (is (= 1 (length groups)))
    (is (= 2 (length (first groups))))))

(test duplicate-top-level-forms-clean
  (is (null (duplicate-top-level-forms "(defun a () 1)
(defun b () 2)"))))

(test find-subform-matching-exact-no-fuzzy
  ;; contains-match would hit; exact must refuse
  (let* ((text "(defun f () (g (h 123)))")
         (top (first (list-top-level (parse-lisp-source text)))))
    (is (null (find-subform-matching-exact top text "(h")))
    (is (not (null (find-subform-matching-exact top text "(h 123)"))))))

;;; 0.4.0 — anchor addressing + scope-aware insertion helpers

(test unique-anchor-offset-end
  (is (= 14 (unique-anchor-offset "(defun f () 1)" "1)"))))

(test count-text-occurrences-overlap
  (multiple-value-bind (count off)
      (count-text-occurrences "(a)(a)" "(a)")
    (is (= 2 count))
    (is (= 0 off))))

;;; 0.4.3 — --match ambiguity policy

(test subform-candidates-counts
  (let* ((text "(defun d () (cond ((eq x :a) (v s)) ((eq x :b) (v s))))")
         (top (first (list-top-level (parse-lisp-source text)))))
    (multiple-value-bind (exact contains)
        (subform-candidates top text "(v s)")
      (is (= 2 (length exact)))
      (is (= 0 (length contains))))))

(test subform-candidates-unique
  (let* ((text "(defun d () (cond ((eq x :a) (v s)) ((eq x :b) (other))))")
         (top (first (list-top-level (parse-lisp-source text)))))
    (multiple-value-bind (exact contains)
        (subform-candidates top text "(v s)")
      (is (= 1 (length exact)))
      (is (= 0 (length contains))))
    (multiple-value-bind (exact2 contains2)
        (subform-candidates top text "(other)")
      (is (= 1 (length exact2)))
      (is (= 0 (length contains2))))))

;;; 0.5.0 — path addressing + atomic extraction helpers

(test split-on-char-basic
  (is (equal '("3" "1") (split-string-on-char "3/1" #\/)))
  (is (equal '("a") (split-string-on-char "a" #\/)))
  (is (equal '("" "") (split-string-on-char "/" #\/))))

(test node-at-path-walks
  (let* ((text "(defun d (x) (a) (b))")
         (host (find-top-level-by-name text "d")))
    (is (string= "(a)" (node-source-text text (node-at-path text host "3/0"))))
    (is (string= "(x)" (node-source-text text (node-at-path text host "2"))))
    (is (null (node-at-path text host "9")))
    (is (null (node-at-path text host "3/9")))))
