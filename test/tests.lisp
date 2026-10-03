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

;;; Number syntax follows the CL reader exactly: ratios, radix
;;; prefixes, leading-dot floats, trailing-dot integers and the
;;; float-format marker letter all have to round-trip.

(defun parse-single-number (text)
  (let* ((ast (parse-lisp-source text))
         (node (first (node-children ast))))
    (unless (eq (node-type node) :number)
      (error "~a did not parse as a number (got ~s)" text (node-type node)))
    node))

(defun rejects-as-number (text)
  (let ((ast (parse-lisp-source text)))
    (or (eq (node-type ast) :error)
        (not (eq (node-type (first (node-children ast))) :number)))))

(test ratio-is-number
  (is (= 3/4 (node-value (parse-single-number "3/4"))))
  (is (= -3/4 (node-value (parse-single-number "-3/4"))))
  (is (= 5/2 (node-value (parse-single-number "10/4"))))
  (is (= 0 (node-value (parse-single-number "0/5")))))

(test radix-prefixed-integer-is-number
  (is (= 255 (node-value (parse-single-number "#xFF"))))
  (is (= 183 (node-value (parse-single-number "#xb7"))))
  (is (= 5 (node-value (parse-single-number "#b101"))))
  (is (= 15 (node-value (parse-single-number "#o17"))))
  (is (= 5 (node-value (parse-single-number "#2r101"))))
  (is (= 1295 (node-value (parse-single-number "#36rZZ"))))
  (is (= 255 (node-value (parse-single-number "#x+FF"))))
  (is (= 255 (node-value (parse-single-number "#16rff"))))
  (is (= -255 (node-value (parse-single-number "#x-FF"))))
  (is (= -5 (node-value (parse-single-number "#10r-5")))))

(test bad-radix-prefix-is-rejected
  (dolist (text '("#xFFg" "#b12" "#o89" "#2r2" "#37r0" "#x" "#b" "#10r"
                  "3/0"))
    (is (rejects-as-number text) (format nil "~a must not parse as a number" text))))

(test leading-and-trailing-dot-numbers
  (is (= 0.5 (node-value (parse-single-number ".5"))))
  (is (= 50.0 (node-value (parse-single-number ".5e2"))))
  (is (= 5000.0 (node-value (parse-single-number "5.e3"))))
  (is (= 5 (node-value (parse-single-number "5.")))))

(test float-marker-picks-float-format
  ;; The reader's default is single; d/l force double, e/f/s follow
  ;; the default. e is NOT an alias for d.
  (is (typep (node-value (parse-single-number "1.5")) 'single-float))
  (is (typep (node-value (parse-single-number "1e0")) 'single-float))
  (is (typep (node-value (parse-single-number "1f0")) 'single-float))
  (is (typep (node-value (parse-single-number "1s0")) 'single-float))
  (is (typep (node-value (parse-single-number "1d0")) 'double-float))
  (is (typep (node-value (parse-single-number "1D0")) 'double-float))
  (is (typep (node-value (parse-single-number "1l0")) 'double-float))
  (is (typep (node-value (parse-single-number "1.5d0")) 'double-float))
  (is (typep (node-value (parse-single-number "1.5l0")) 'double-float))
  (is (typep (node-value (parse-single-number "1.5f0")) 'single-float))
  (is (typep (node-value (parse-single-number "1.5s0")) 'single-float))
  (is (typep (node-value (parse-single-number ".5d0")) 'double-float))
  (is (= 0.001d0 (node-value (parse-single-number "1d-3"))))
  (is (= 1.0e10 (node-value (parse-single-number "1e10")))))

(test out-of-range-numbers-are-rejected-not-signalled
  ;; 1e542 is well formed but has no single-float value, and the reader
  ;; rejects it (READER-IMPOSSIBLE-NUMBER-ERROR). The arithmetic error
  ;; raised while coercing it must not escape the parser as an
  ;; unhandled condition.
  (dolist (text '("1e542" "1.5f342" "1e400" "1d400" "1e39" "1e308" "3.5e38"))
    (let ((ast (parse-lisp-source text)))
      (is (eq :error (node-type ast))
          (format nil "~a must be reported as a parse error, not signalled" text)))
  ;; ...while the ones that do fit still parse
  ;; e means the default (single) format, so 1e308 has no value; the
  ;; same magnitude with a d marker is a perfectly good double
  (dolist (text '("1e38" "3.4e38" "1.5" "1e0" "1d0" "1d308"))
    (let ((ast (parse-lisp-source text)))
      (is (eq :list (node-type ast))
          (format nil "~a must still parse" text))))))

(test slash-is-symbol-when-not-a-ratio
  (dolist (text '("3/-4" "-3/-4" "3.5/2"))
    (let ((node (first (node-children (parse-lisp-source text)))))
      (is (eq :symbol (node-type node)))
      (is (string= text (node-name node))))))

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
    (is (not (search "bar" result)))))

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

(test lint-schema-and-registry
  (is (not (null (member "duplicate-top-level" (lint-rule-ids) :test #'string=))))
  (let ((d (make-lint-diagnostic :rule "r" :severity :warning
                                 :line 0 :col 1 :start 2 :end 3
                                 :message "m" :fix "f")))
    (is (string= "r" (getf d :rule)))
    (is (eq :warning (getf d :severity)))
    (is (string= "m" (getf d :message))))
  (is (null (ignore-errors (make-lint-diagnostic :rule "r" :severity :nope
                                                 :message "m"))))
  (is (null (ignore-errors (lint-source "(a)" :rules '("no-such-rule"))))))

(test lint-duplicate-diagnostics
  (let* ((text "(in-package :cl)
(defun a () 1)
(in-package :cl)")
         (result (lint-source text :rules '("duplicate-top-level"))))
    (is (null (getf result :ok)))
    (is (= 1 (length (getf result :diagnostics))))
    (let ((d (first (getf result :diagnostics))))
      (is (string= "duplicate-top-level" (getf d :rule)))
      (is (eq :warning (getf d :severity)))
      (is (search "first copy" (getf d :message)))))
  (let ((result (lint-source "(defun a () 1)
(defun b () 2)" :rules '("duplicate-top-level"))))
    (is (getf result :ok))
    (is (null (getf result :diagnostics)))
    (is (string= "{\"ok\":true,\"diagnostics\":[]}"
                 (lint-diagnostics-json result)))))

(test lint-syntax-error-diagnostic
  (let ((result (lint-source "(defun a () ")))
    (is (null (getf result :ok)))
    (is (= 1 (length (getf result :diagnostics))))
    (is (string= "syntax-error" (getf (first (getf result :diagnostics)) :rule)))
    (is (eq :error (getf (first (getf result :diagnostics)) :severity)))))

(test lint-sharp-underscore-dispatch
  (let ((result (lint-source "(list 1 #_2 3)"
                             :rules '("sharp-underscore-dispatch"))))
    (is (null (getf result :ok)))
    (is (= 1 (length (getf result :diagnostics))))
    (let ((d (first (getf result :diagnostics))))
      (is (string= "sharp-underscore-dispatch" (getf d :rule)))
      (is (eq :portability (getf d :severity)))))
  (let ((result (lint-source "(list 1 2 3)"
                             :rules '("sharp-underscore-dispatch"))))
    (is (getf result :ok))))

(test lint-redefined-top-level
  (let ((result (lint-source "(defun f () 1)
(defun f () 2)"
                             :rules '("redefined-top-level"))))
    (is (null (getf result :ok)))
    (is (= 1 (length (getf result :diagnostics))))
    (let ((d (first (getf result :diagnostics))))
      (is (string= "redefined-top-level" (getf d :rule)))
      (is (eq :warning (getf d :severity)))
      (is (search "line 0" (getf d :message)))))
  ;; identical copies belong to duplicate-top-level, not this rule
  (let ((result (lint-source "(defun f () 1)
(defun f () 1)"
                             :rules '("redefined-top-level"))))
    (is (getf result :ok)))
  ;; different heads do not clash, and overloading is not redefinition
  (let ((result (lint-source "(defun f () 1)
(defmacro f () 2)
(defmethod f ((x t)) x)
(defmethod f ((x null)) nil)"
                             :rules '("redefined-top-level"))))
    (is (getf result :ok)))
  ;; repeated calls are not definitions
  (let ((result (lint-source "(foo 1)
(foo 2)"
                             :rules '("redefined-top-level"))))
    (is (getf result :ok))))

(test lint-empty-operator
  ;; (()) calls NIL: flag it
  (let ((result (lint-source "(f (()))"
                             :rules '("empty-operator"))))
    (is (null (getf result :ok)))
    (is (= 1 (length (getf result :diagnostics))))
    (let ((d (first (getf result :diagnostics))))
      (is (string= "empty-operator" (getf d :rule)))
      (is (eq :warning (getf d :severity)))))
  ;; NIL as a value is ordinary: no findings
  (dolist (code (list "(f ())" "()" "'(())" "(quote (()))"))
    (let ((result (lint-source code :rules '("empty-operator"))))
      (is (getf result :ok) "code=~s should be clean" code))))

(test lint-eval-hazard
  (dolist (code (list "(eval x)" "#.(foo)" "(EVAL x)"))
    (let ((result (lint-source code :rules '("eval-hazard"))))
      (is (null (getf result :ok)) "code=~s should flag" code)
      (is (= 1 (length (getf result :diagnostics))))))
  (let ((result (lint-source "(defun f () 1)" :rules '("eval-hazard"))))
    (is (getf result :ok))))

(test lint-skipped-conditional-branch
  (let ((result (lint-source "(list #-other-lisp #\\Name-Only-That-Lisp-Knows)"
                             :rules '("skipped-conditional-branch"))))
    (is (null (getf result :ok)))
    (is (= 1 (length (getf result :diagnostics))))
    (let ((d (first (getf result :diagnostics))))
      (is (string= "skipped-conditional-branch" (getf d :rule)))
      (is (eq :info (getf d :severity)))))
  (let ((result (lint-source "(list #+sbcl (a b))"
                             :rules '("skipped-conditional-branch"))))
    (is (getf result :ok))))

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
      ;; two clauses + cond + host defun itself all contain the snippet
      (is (= 4 (length contains))))))

(test subform-candidates-unique
  (let* ((text "(defun d () (cond ((eq x :a) (v s)) ((eq x :b) (other))))")
         (top (first (list-top-level (parse-lisp-source text)))))
    (multiple-value-bind (exact contains)
        (subform-candidates top text "(v s)")
      (is (= 1 (length exact)))
      (is (= 3 (length contains))))
    (multiple-value-bind (exact2 contains2)
        (subform-candidates top text "(other)")
      (is (= 1 (length exact2)))
      (is (= 3 (length contains2))))))

;;; 0.5.0 — path addressing + atomic extraction helpers

(test split-on-char-basic
  (is (equal '("3" "1") (split-string-on-char "3/1" #\/)))
  (is (equal '("a") (split-string-on-char "a" #\/)))
  (is (equal '("" "") (split-string-on-char "/" #\/))))

(test node-at-path-walks
  (let* ((text "(defun d (x) (a) (b))")
         (host (find-top-level-by-name text "d")))
    (is (string= "(a)" (node-source-text text (node-at-path text host "3"))))
    (is (string= "a" (node-source-text text (node-at-path text host "3/0"))))
    (is (string= "(x)" (node-source-text text (node-at-path text host "2"))))
    (is (null (node-at-path text host "9")))
    (is (null (node-at-path text host "3/9")))))

;;; F15 regression: append into an empty host must not lead with a blank line

(test insert-form-end-empty-host
  ;; text files end with a newline — the trailing \n is by design
  (is (string= (concatenate 'string "(defun x () 1)" (string #\Newline))
               (insert-form-end "" "(defun x () 1)" :validate t)))
  (is (string= (concatenate 'string "(defun x () 1)" (string #\Newline))
               (insert-form-end "   " "(defun x () 1)")))
  (is (string= (concatenate 'string "pre" (string #\Newline)
                            "(defun x () 1)" (string #\Newline))
               (insert-form-end "pre" "(defun x () 1)"))))

;;; F14 regression: UTF-8 multibyte files must not gain NUL tails

(test read-file-to-string-utf8-no-nuls
  (let ((path "/tmp/ctk-utf8-test.md"))
    (with-open-file (s path :direction :output :if-exists :supersede
                            :external-format :utf-8)
      (write-string "emoji → arrow and Turkish İı Şş" s))
    (unwind-protect
         (let ((content (read-file-to-string path)))
           (is (not (find #\Null content)))
           (is (search "→" content))
           (is (search "İı" content)))
      (ignore-errors (delete-file path)))))

;;; Bugfix batch: 2026-10 audit

(test recovery-leading-whitespace
  (let ((ast (parse-with-recovery "  (+ 1 2)")))
    (is (eq :list (node-type ast)))
    (is (= 1 (length (node-children ast))))
    (is (eq :list (node-type (first (node-children ast)))))))

(test recovery-line-comment-skipped
  (let ((ast (parse-with-recovery (format nil "; hi~%(+ 1 2)"))))
    ;; comment is skipped via skip-whitespace: only the form remains
    (is (= 1 (length (node-children ast))))
    (is (eq :list (node-type (first (node-children ast)))))))

(test dotted-pair-parses
  (let* ((ast (parse-lisp-source "(a . b)"))
         (form (first (node-children ast))))
    (is (eq :list (node-type form)))
    (is (= 3 (length (node-children form))))))

;;; The reader splices a dotted tail that is itself a list, so
;;; (a . (b c)) is the three-element list (a b c) — not four children
;;; with a "." and a sub-list in the middle.

(defun form-shape (ast)
  (mapcar (lambda (child)
            (case (node-type child)
              (:list :list)
              (:symbol (node-name child))
              ((:number :string :character) (node-type child))
              (t (node-type child))))
          (node-children (first (node-children ast)))))

(defun parse-rejects (text)
  (let ((ast (parse-lisp-source text)))
    (or (eq (node-type ast) :error)
        (eq (node-type (first (node-children ast))) :error))))

(test dotted-tail-that-is-a-list-is-spliced
  (is (equal '("a" "b") (form-shape (parse-lisp-source "(a . (b))"))))
  (is (equal '("a" "b" "c") (form-shape (parse-lisp-source "(a . (b c))"))))
  (is (equal '("a" "b" "c") (form-shape (parse-lisp-source "(a b . (c))"))))
  (is (equal '("a" "b" "c" "d")
             (form-shape (parse-lisp-source "(a b . (c d))"))))
  (is (equal '("a" "b") (form-shape (parse-lisp-source "(a . (b . nil))"))))
  ;; NIL is the empty list, so a NIL tail just ends the list
  (is (equal '("col") (form-shape (parse-lisp-source "(col . nil)"))))
  (is (equal '("a" "b") (form-shape (parse-lisp-source "(a b . nil)"))))
  (is (equal '("for" :list "in" "y")
             (form-shape (parse-lisp-source "(for (x . nil) in y)"))))
  (is (equal '("for" :list "in" "y")
             (form-shape (parse-lisp-source "(for (x . (nil)) in y)"))))
  ;; ...but a quoted nil is a form, not the empty list
  (is (equal '("a" "QUOTE" "nil") (form-shape (parse-lisp-source "(a . 'nil)")))))

(test nested-dotted-tail-is-spliced
  (let* ((ast (parse-lisp-source "(f (a . (b)) . (c))"))
         (children (node-children (first (node-children ast))))
         (nested (second children)))
    (is (= 3 (length children)))
    (is (eq :list (node-type nested)))
    (is (equal '("a" "b") (mapcar #'node-name (node-children nested))))
    (is (string= "c" (node-name (third children))))))

(test lone-dot-is-not-a-symbol
  ;; the reader signals a bare "."; ".b", ".." and "a.b" stay symbols
  (is (parse-rejects "."))
  (is (not (parse-rejects ".b")))
  (is (not (parse-rejects "..")))
  (is (not (parse-rejects "a.b")))
  (is (string= ".b" (node-name (first (top-forms-of ".b")))))
  (is (string= ".." (node-name (first (top-forms-of ".."))))))

(test malformed-dotted-forms-are-rejected
  (dolist (text '("(. b)" "(a .)" "(a . . b)" "(a . b . c)"
                  "(a b . )" "(.)"))
    (is (parse-rejects text)
        (format nil "~a must be rejected like the reader rejects it" text))))

(test dot-inside-vector-is-rejected
  (dolist (text '("#(a . b)" "#(a b . c)" "#(. b)"))
    (is (parse-rejects text)
        (format nil "~a must be rejected like the reader rejects it" text))))

;;; --- Span invariants ---
;;;;
;;; Every node's [start, end) must be a real range inside its parent, in
;;; source order among its siblings, and non-empty once a token has been
;;; consumed -- the editing commands slice with exactly these numbers.

(test span-invariants-hold-on-tricky-inputs
  (dolist (text (list "(defun f (a b) (+ a b))"
                      "(a . b)"
                      "(a . (b c))"
                      "(a . nil)"
                      "#()"
                      "#(1 #() 2)"
                      "#*"
                      "#*101"
                      "\"a\\\"b\""
                      "#\\Space"
                      "(quote x)"
                      "(quote (quote x))"
                      "`(a ,b ,@c)"
                      "#+sbcl 1 #-no-such-feature 2"
                      "#.(+ 1 2)"
                      ";; comment only"
                      "(a ; trailing\n  b)"
                      "|weird symbol|"
                      ":pkg:name"
                      "1/2"
                      "-3/4"
                      ".5"
                      "5."
                      "#xFF"))
    (let ((ast (parse-lisp-source text)))
      (unless (eq (node-type ast) :error)
        (is (null (check-node-spans ast))
            (format nil "span problems in ~s: ~s" text (check-node-spans ast)))
        (is (null (check-source-spans ast text))
            (format nil "leaf span problems in ~s: ~s"
                    text (check-source-spans ast text)))))))

(test span-checker-rejects-broken-nodes
  ;; a child that reaches outside its parent
  (let ((bad (make-node :list
                        :children (list (make-node :symbol :name "a"
                                                   :start 0 :end 1))
                        :start 5 :end 6)))
    (is (check-node-spans bad))
    (is (member "escapes its parent"
                (mapcar (lambda (p) (getf p :reason)) (check-node-spans bad))
                :test #'string=)))
  ;; start after end
  (is (check-node-spans (make-node :symbol :name "a" :start 9 :end 2)))
  ;; siblings running backwards
  (let ((bad (make-node :list
                        :children (list (make-node :symbol :name "a" :start 4 :end 5)
                                        (make-node :symbol :name "b" :start 1 :end 2))
                        :start 0 :end 9)))
    (is (member "sibling starts before its predecessor"
                (mapcar (lambda (p) (getf p :reason)) (check-node-spans bad))
                :test #'string=)))
  ;; a well-formed tree reports nothing
  (let ((ok (parse-lisp-source "(defun f () 1)")))
    (is (null (check-node-spans ok)))
    (is (null (check-source-spans ok "(defun f () 1)")))))

(test leaf-span-must-read-back-as-the-token
  (let ((ast (parse-lisp-source "\"hi\"")))
    (is (null (check-source-spans ast "\"hi\""))))
  ;; a :string node whose range does not cover the quotes is reported
  (let ((bogus (make-node :list
                          :children (list (make-node :string :value "hi"
                                                     :start 0 :end 2))
                          :start 0 :end 2)))
    (is (member "string span is not wrapped in quotes"
                (mapcar (lambda (p) (getf p :reason))
                        (check-source-spans bogus "hi"))
                :test #'string=))))

;;; --- Formatter round-trip ---
;;;;
;;; Formatting may move code around but must never change what the code
;;; means: the result has to parse, parse to the same structure, and be a
;;; fixed point (formatting twice changes nothing). Checked over the whole
;;; corpus for both the minimal and the canonical formatter, and pinned
;;; here so a later change cannot quietly break it.

(defun structural-shape (node)
  "Structure of NODE ignoring positions: type, name, value, children."
  (list (node-type node)
        (node-name node)
        (node-value node)
        (mapcar #'structural-shape (node-children node))))

(defparameter *format-samples*
  (list "(defun f (a b) (+ a b))"
        "(a . b)"
        "(a . (b c))"
        "(a . nil)"
        "#()"
        "#(1 2 3)"
        "#*101"
        "\"a \\\"b\""
        "(quote x)"
        "`(a ,b ,@c)"
        "#+sbcl 1"
        "#-no-such-feature 2"
        "(let ((x 1) (y 2))(+ x y))"
        "(if a b c d e)"
        "(cond ((a) 1) ((b) 2) (t 3))"
        "(lambda (x) body)"
        "(do ((i 0 (1+ i))) ((= i 10)))"
        "; just a comment"
        "(defun broken ("
        "|weird sym|"
        "(f #'g #' #'x)"
        "(a .5 b 5. .c)"
        "(a #xFF #b101 1/2)"))

(defun format-round-trip-ok-p (text formatter)
  (let* ((ast (parse-lisp-source text)))
    (cond ((eq (node-type ast) :error) t)
          (t
           (let* ((once (funcall formatter text))
                  (ast2 (parse-lisp-source once))
                  (twice (funcall formatter once)))
             (and (stringp once)
                  (not (eq (node-type ast2) :error))
                  (equal (structural-shape ast) (structural-shape ast2))
                  (stringp twice)
                  (string= once twice)))))))

(test format-preserves-structure-and-is-idempotent
  (dolist (text *format-samples*)
    (is (format-round-trip-ok-p text #'format-source)
        (format nil "canonical format changed the meaning of ~s" text))
    (is (format-round-trip-ok-p text #'format-minimal)
        (format nil "minimal format changed the meaning of ~s" text))))

(defparameter *literal-samples*
  ;; built with CODE so the escapes under test are unambiguous
  (list (format nil "~a\tab~a~a" (code-char 34) (code-char 34) (code-char 34))
        (format nil "~aquote ~a~a inside~a" (code-char 34) (code-char 92) (code-char 34) (code-char 34))
        (format nil "~a~a Space~a" (code-char 35) (code-char 92) (code-char 34))
        (format nil "~a~a(~a" (code-char 35) (code-char 92) (code-char 34))
        "'.5"
        "'.a"
        "1/2"
        "#xFF"
        "#b101"
        "-3/4"))

(test format-keeps-escapes-and-literals-intact
  ;; a formatter may reflow code, but it must never rewrite what a
  ;; literal means -- an escape that decodes differently is a bug that
  ;; only shows up downstream
  (dolist (text *literal-samples*)
    (let* ((ast (parse-lisp-source text))
           (once (format-source text))
           (ast2 (parse-lisp-source once)))
      (is (eql (node-type ast) (node-type ast2))
          (format nil "formatting ~s changed its type" text))
      (is (equal (structural-shape ast) (structural-shape ast2))
          (format nil "formatting ~s changed it to ~s" text once))
      (is (equal (node-value (first (node-children ast2)))
                 (node-value (first (node-children ast))))
          (format nil "formatting ~s changed the literal's value" text)))))

;;; --- Edit-operation properties ---
;;;;
;;; An edit may move code but must never corrupt the file. Checked over
;;; the corpus: replacing a form with its own source text restores the
;;; file byte for byte; the same edit twice gives the same bytes; a
;;; successful edit leaves text that still parses with valid spans;
;;; inserting cannot shorten a file and deleting cannot lengthen one;
;;; and a batch equals its parts applied one at a time.

(defun edit-source-valid-p (text)
  (if (zerop (length (string-trim '(#\Space #\Tab #\Newline #\Return) text)))
      t
      (let ((ast (parse-lisp-source text)))
        (and (not (eq (node-type ast) :error))
             (null (check-source-spans ast text))))))

(defparameter *edit-samples*
  '("(defun alpha () 1)
(defun beta (x) (* x 2))
(setq *y* 3)"
    "(defpackage #:demo)"
    "(in-package #:demo)
(defun only () :one)"
    "(a . b)"
    "#()"
    "(defstruct point (x 0) (y 0))"))

(test replacing-a-form-with-its-own-source-is-a-no-op
  (dolist (text *edit-samples*)
    (let* ((ast (parse-lisp-source text))
           (forms (list-top-level ast))
           (i (random (length forms)))
           (node (nth i forms))
           (code (node-source-text text node))
           (edit (list :operation :replace-index :index i :code code)))
      (is (equal text (apply-single-edit text edit))
          (format nil "replace-index ~a was not reversible in ~s" i text))
      (is (edit-source-valid-p (apply-single-edit text edit))))))

(test edits-are-deterministic
  (dolist (text *edit-samples*)
    (dolist (edit (list (list :operation :replace-index :index 0 :code "(new)")
                          (list :operation :delete-index :index 0)
                          (list :operation :insert-after-index :index 0 :code "(x)")
                          (list :operation :replace-name :name "defun"
                                :code "(defun renamed () 1)")
                          (list :operation :delete-name :name "defun")
                          (list :operation :rename-name :name "beta" :code "gamma")
                          (list :operation :wrap-name :name "beta"
                                :open "(wrap" :close "wrap)")
                          (list :operation :unwrap-name :name "beta")
                          (list :operation :replace-match :match "defun" :code "(d)")))
      (let ((once (handler-case (apply-single-edit text edit)
                    (error (c) (list :refused (type-of c)))))
            (twice (handler-case (apply-single-edit text edit)
                     (error (c) (list :refused (type-of c))))))
        ;; two freshly signalled conditions are never EQ, so compare the
        ;; refusal by type and only compare bytes when an edit succeeded
        (is (cond ((and (stringp once) (stringp twice))
                   (string= once twice))
                  ((and (consp once) (consp twice))
                   (eq (getf once :refused) (getf twice :refused)))
                  (t nil))
            (format nil "~s was not deterministic on ~s" edit text))
        (when (stringp once)
          (is (edit-source-valid-p once)))))))

(test insertions-grow-and-deletions-shrink
  (let ((text "(defun a () 1)
(defun b () 2)
(defun c () 3)"))
    (dolist (index '(0 1 2))
      (let ((inserted (apply-single-edit text
                                        (list :operation :insert-after-index
                                              :index index :code "(defnew () 0)"))))
        (is (>= (length inserted) (length text))))
      (let ((deleted (apply-single-edit text
                                       (list :operation :delete-index :index index))))
        (is (<= (length deleted) (length text))))
      ;; an impossible index must be refused, not silently corrupt
      (let ((outcome (handler-case
                        (apply-single-edit text (list :operation :delete-index
                                                     :index 99))
                      (error (c) (list :refused (type-of c))))))
        (is (consp outcome)
            "an out-of-range delete must be refused, not silently applied")))))

(test deleting-the-only-form-leaves-an-empty-file
  (let* ((text "(defun only () 1)")
         (result (apply-single-edit text (list :operation :delete-index :index 0))))
    (is (string= "" (string-trim '(#\Space #\Tab #\Newline #\Return) result)))
    (is (edit-source-valid-p result))))

(test batch-edits-match-the-parts-applied-one-at-a-time
  (let ((text "(defun a () 1)
(defun b () 2)
(defun c () 3)"))
    (let* ((e1 (list :operation :replace-index :index 0 :code "(first)"))
           (e2 (list :operation :insert-after-index :index 1 :code "(extra)"))
           (batched (apply-batch-edits text (list e1 e2)))
           (single (apply-single-edit (apply-single-edit text e1) e2)))
      (is (string= single batched))
      (is (edit-source-valid-p batched)))))

;;; --- Machine-output hygiene ---
;;;;
;;; Scripts parse this JSON, so the shape of a node and of a lint
;;; diagnostic is a contract: the documented keys with the documented
;;; types, bounds inside the file, leaves without children. The output
;;; must also be byte-identical across runs -- nothing may depend on
;;; hash order or on a clock -- and a command must finish in bounded
;;; time. Verified over the whole corpus; pinned here over the node kinds
;;; most likely to drift.

(defun alist-get (alist key)
  (cdr (assoc key alist :test #'eq)))

(defparameter *schema-samples*
  (list "(defun f (x) \"doc\" (+ x 1))"
        "(a . b)"
        "(a . (b c))"
        "#()"
        "#(1 2)"
        "#*101"
        "()"
        "'x"
        "`(a ,b)"
        "#+sbcl 1 #-none 2"
        "0"
        "-3/4"
        "#xFF"
        "|odd sym|"
        ":pkg:name"
        "(list (vector #(+ 1 2)) #*1010)"
        ";; only a comment"
        ""))

(defun top-level-schema-problems (alist text)
  "Schema checks that need no recursion: every top-level node carries the
   documented keys with the documented types and bounds inside the file."
  (let ((out nil)
        (type (alist-get alist :type))
        (start (alist-get alist :start))
        (end (alist-get alist :end)))
    (unless (and (stringp type) (> (length type) 0))
      (push "type must be a non-empty string" out))
    (unless (integerp start) (push "start must be an integer" out))
    (unless (integerp end) (push "end must be an integer" out))
    (when (and (integerp start) (integerp end))
      (when (> start end) (push "start after end" out))
      (when (> end (length text)) (push "end past the source" out))
      (when (< start 0) (push "negative start" out)))
    (dolist (key '(:name :package :source))
      (let ((v (alist-get alist key)))
        (when (and v (not (stringp v)))
          (push (format nil "~a must be a string" key) out))))
    (let ((v (alist-get alist :value)))
      (when (and v (not (or (stringp v) (numberp v))))
        (push "value must be a string or a number" out)))
    (nreverse out)))

(test parse-node-schema-holds
  (dolist (text *schema-samples*)
    (let ((ast (parse-lisp-source text)))
      (unless (eq (node-type ast) :error)
        (let ((problems (top-level-schema-problems (node-to-alist ast) text)))
          (is (null problems)
              (format nil "schema problems in ~s: ~s" text problems)))))))

(test leaf-nodes-have-no-children
  (dolist (text *schema-samples*)
    (let ((ast (parse-lisp-source text)))
      (unless (eq (node-type ast) :error)
        (labels ((walk (n)
                   (when (member (node-type n) '(:symbol :number :string :char))
                     (is (null (node-children n))
                         (format nil "leaf ~s in ~s has children" (node-type n) text)))
                   (dolist (k (node-children n)) (walk k))))
          (walk ast))))))

(test lint-diagnostic-schema-holds
  (dolist (text *schema-samples*)
    (let ((result (lint-source text)))
      (is (or (eql (getf result :ok) t) (null (getf result :ok)))
          ":ok must be a boolean")
      (is (listp (getf result :diagnostics)))
      (dolist (d (getf result :diagnostics))
        (is (stringp (getf d :rule)))
        (is (stringp (getf d :message)))
        (is (member (getf d :severity) '(:error :warning :portability :style :info)))
        (dolist (key '(:line :col :start :end))
          (is (integerp (getf d key))
              (format nil "~a must be an integer in ~s" key d)))
        (let ((fix (getf d :fix)))
          (is (or (null fix) (stringp fix)
                  (and (consp fix)
                       (every (lambda (kv) (and (consp kv) (stringp (cdr kv)))) fix))))))))

(test lint-json-is-stable-across-runs
  (dolist (text *schema-samples*)
    (let* ((result (lint-source text))
           (a (lint-diagnostics-json result))
           (b (lint-diagnostics-json (lint-source text))))
      (is (string= a b)
          (format nil "lint JSON differed between runs for ~s" text)))))

(test parse-json-is-stable-across-runs
  (dolist (text *schema-samples*)
    (let* ((ast (parse-lisp-source text))
           (a (node-to-json-string ast))
           (b (node-to-json-string (parse-lisp-source text))))
      (is (string= a b)
          (format nil "parse JSON differed between runs for ~s" text)))))

(defun j (text)
  "TEXT wrapped in double quotes, the way the JSON encoder writes it."
  (format nil "~a~a~a" (code-char 34) text (code-char 34)))

(test lint-json-is-parseable-machine-output
  (let ((json (lint-diagnostics-json
               (lint-source "(defun f () (eval (read-from-string "
                            (j "x") ")))"))))
    (is (search (j "ok:") json))
    (is (search (j "diagnostics":[) json))
    (is (search (format nil "~a:~a~a" (j "rule") (j "eval-hazard")) json))
    (is (search (format nil "~a:~a~a" (j "severity") (j "warning")) json))
    (is (search (format nil "~a:~a~a" (j "line") 12) json))))

(test pathological-inputs-are-bounded
  ;; deeply nested and unterminated input must not hang or blow the stack
  (dolist (text (list (format nil "~{~a~}" (make-list 2000 :initial-element "("))
                      (format nil "~a~a" (make-string 5000 :initial-element #\() ")")
                      (format nil "~a~a" (make-string 500 :initial-element #\() ")")
                      (concatenate 'string (make-list 300 :initial-element "#(")))))
    (let ((start (get-internal-real-time)))
      (handler-case (parse-lisp-source text) (error () nil))
      (let ((elapsed (/ (- (get-internal-real-time) start)
                        internal-time-units-per-second)))
        (is (< elapsed 5)
            (format nil "parsing ~a characters took ~as" (length text) elapsed))))))

(test empty-bit-vector-has-no-zero-width-child
  (let* ((ast (parse-lisp-source "#*"))
         (node (first (node-children ast)))
         (kids (node-children node)))
    (is (= 1 (length kids)))
    (is (string= "BIT-VECTOR" (node-name (first kids))))
    (is (= (node-start node) (node-start (first kids))))
    (is (= (node-end node) (node-end (first kids)))))
  (let ((ast (parse-lisp-source "#*101")))
    (is (null (check-source-spans ast "#*101")))))

(test empty-vector-is-supported
  ;; #() is a vector with no elements; it used to parse as a "#" symbol
  ;; plus an empty list, and then to fail outright once "#(" was barred
  ;; from the symbol rule
  (let* ((ast (parse-lisp-source "#()"))
         (node (first (node-children ast))))
    (is (eq :vector (node-type node)))
    (is (null (node-children node))))
  (is (not (parse-rejects "(vector #() :type vector)")))
  (let* ((nested (parse-lisp-source "#(1 #() 2)"))
         (kids (node-children (first (node-children nested)))))
    (is (= 3 (length kids)))
    (is (eq :vector (node-type (second kids))))
    (is (null (node-children (second kids))))))

(test vector-open-is-never-a-symbol
  (dolist (text '("#(1 2)" "#(a)" "#(a b)"))
    (is (not (parse-rejects text)))))

(test package-split-double-colon
  (let* ((ast (parse-lisp-source "foo::bar"))
         (sym (first (node-children ast))))
    (is (string= "bar" (node-name sym)))
    (is (string= "foo" (node-package sym)))))

(test package-split-keyword
  (let* ((ast (parse-lisp-source ":foo"))
         (sym (first (node-children ast))))
    (is (string= "foo" (node-name sym)))))

(test offset-inverse-oob-signals
  (is (null (ignore-errors (cl-toolkit-ast:offset-to-line-col-inverse "(a)" 10 0)))))

(test offset-inverse-eof-ok
  (is (= 4 (cl-toolkit-ast:offset-to-line-col-inverse (format nil "(a)~%") 1 0))))

(test insert-end-preserves-leading-blanks
  (let ((result (insert-form-end (format nil "~%~%(a)") "(b)")))
    (is (search "(a)" result))
    (is (search "(b)" result))
    ;; leading blank lines survive (right-trim only)
    (is (char= #\Newline (char result 0)))))

(test timestamped-backup-dot
  (let ((p (cl-toolkit::timestamped-backup-path "foo" "/tmp/bak")))
    (is (search ".lisp.bak" p))))

(test node-at-path-malformed-nil
  (let* ((txt "(a)")
         (host (first (list-top-level (parse-lisp-source txt)))))
    (is (null (node-at-path txt host "x")))
    (is (null (node-at-path txt host "")))
    (is (null (node-at-path txt host "3/0")))))

(test subform-whole-host-matches
  (let* ((txt "(defun foo () 1)")
         (top (first (list-top-level (parse-lisp-source txt)))))
    (is (not (null (find-subform-matching top txt txt))))
    (is (not (null (find-subform-matching-exact top txt txt))))))

(test balance-escaped-quote
  (let ((result (analyze-balance (format nil "(a ~s)" "b\\\"(c"))))
    (is (= 0 (getf result :final-depth)))))

(test balance-zero-based-lines
  (let ((result (analyze-balance "(a)")))
    (is (= 0 (getf (first (getf result :lines)) :line)))))

(test balance-block-comment-ends
  (let ((result (analyze-balance "#| hi |# (a)")))
    (is (= 0 (getf result :final-depth)))
    (is (= 1 (getf result :max-depth)))))

(test move-first-no-leading-newline
  (let ((result (move-form (format nil "(a)~%(b)~%(c)") 0 0 2 0)))
    (is (not (char= #\Newline (char result 0))))
    (is (search "(a)" result))
    (is (search "(c)" result))))

(test move-duplicate-resolves-by-offset
  ;; two identical forms: moving first after second must keep both
  (let* ((txt (format nil "(foo)~%(foo)~%(bar)"))
         (result (move-form txt 0 0 1 0)))
    (is (= 3 (length (list-top-level (parse-lisp-source result)))))
    (is (search "(bar)" result))))

(test indent-no-trailing-ws
  (let ((result (cl-toolkit::indent-continuation-lines
                 (format nil "(a)~%~%(b)") 2)))
    ;; blank middle line must stay empty (no trailing spaces)
    (is (search (format nil "~%~%  (b)") result))
    (is (search "(b)" result))))

(test find-forms-empty-rejected
  (is (null (ignore-errors (find-forms-containing "(a)" "")))))

;;; Reader-macro + Unicode regressions (real-lib sweep: trivia/iterate)

(defun top-forms-of (code)
  "Top-level forms of CODE. Signals via failed assertions downstream when
   parsing errors (an :ERROR root is not a :LIST with children)."
  (list-top-level (parse-lisp-source code)))

(defun first-form-of (code)
  "First top-level form of CODE (single value, for name/value checks)."
  (first (top-forms-of code)))

(test unicode-keyword-parses
  (let* ((code "(defun f (x) :λlist)")
         (ast (parse-lisp-source code))
         (forms (list-top-level ast)))
    (is (eq :list (node-type ast)))
    (is (= 1 (length forms)))
    (is (string= "λlist"
                 (node-name (fourth (node-children (first forms))))))))

(test unicode-defun-name
  (let ((node (find-top-level-by-name "(defun λlist (x) x)" "λlist")))
    (is (not (null node)))
    (is (string= "λlist" (node-form-name node)))))

(test char-literal-case-preserved
  (is (string= "a" (node-value (first (top-forms-of "#\\a")))))
  (is (string= "SPACE" (node-value (first (top-forms-of "#\\Space")))))
  (is (string= "λ" (node-value (first (top-forms-of "#\\λ"))))))

(defun char-value (code)
  (node-value (first (top-forms-of code))))

(defun single-symbol-name (code)
  "Name of the single symbol CODE reads as. Fails the test loudly if
   CODE is anything other than exactly one symbol."
  (let ((forms (top-forms-of code)))
    (unless (= 1 (length forms))
      (error "not a single form: ~s" code))
    (let ((node (first forms)))
      (unless (eq (node-type node) :symbol)
        (error "not a symbol: ~s" code))
      (node-name node))))

(defun code-rejected-p (code)
  "T when CODE does not parse into a single clean form (an :ERROR root,
   or an :ERROR anywhere in the tree — recovery hides inner failures)."
  (labels ((errp (node)
             (and (nodep node)
                  (or (eq (node-type node) :error)
                      (some #'errp (node-children node))))))
    (let ((ast (parse-lisp-source code)))
      (or (errp ast)
          (not (getf (validate ast) :balanced))
          (/= 0 (getf (analyze-balance code) :final-depth))
          (plusp (length (getf (analyze-balance code) :errors)))))))

(test char-literal-exotic-singles
  ;; SBCL reads all of these; the toolkit must not narrow them away.
  ;; Known names keep the upcased convention ("#\Space" -> "SPACE"),
  ;; while graphic singles keep their exact case ("#\a" -> "a").
  (is (string= "LOCK" (char-value "#\\Lock")))
  (is (string= "LOCK" (char-value "#\\lock")))
  (is (string= "«" (char-value "#\\«")))                   ; non-alphanumeric
  (is (string= "→" (char-value "#\\→")))
  (is (string= "€" (char-value "#\\€")))
  (is (string= "\\" (char-value "#\\\\")))                 ; backslash at EOF
  (is (string= "|" (char-value "#\\|")))
  (is (string= (string (code-char 1))                       ; control code
               (char-value (concatenate 'string "#\\"
                                        (string (code-char 1))))))
  (is (string= (string (code-char 127))
               (char-value (concatenate 'string "#\\"
                                        (string (code-char 127)))))))

(test char-name-not-extended
  "A char that continues a name must not degrade into char + symbol."
  (dolist (code '("#\\Nulx" "#\\Sp!" "#\\Spac" "#\\ |" "#\\A{" "#\\AB"
                  "#\\Space!" "#\\Nul\\" "#\\ 7" "#\\Lockx"))
    (is (code-rejected-p code) "code=~s should be rejected~%" code)))

(test char-name-stoppers-accepted
  "Whitespace, delimiters and EOF legitimately end a char name."
  (dolist (code '("(list #\\Space)" "(list #\\Space x)" "(list #\\Space(x))"
                  "(list #\\Space\"s\")" "(list #\\A)"))
    (is (not (code-rejected-p code)) "code=~s should parse~%" code)))

(test backquote-single-form
  (let* ((ast (parse-lisp-source "`(a ,b)"))
         (forms (list-top-level ast)))
    (is (eq :list (node-type ast)))
    (is (= 1 (length forms)))
    (let ((marker (first (node-children (first forms)))))
      (is (string= "BACKQUOTE" (node-name marker)))
      (is (numberp (node-start marker)))
      (is (numberp (node-end marker))))))

(test comma-markers
  (let* ((ast (parse-lisp-source "`(a ,b ,@c)"))
         (forms (list-top-level ast)))
    (is (eq :list (node-type ast)))
    (let* ((inner (second (node-children (first forms))))
           (kids (node-children inner)))
      (is (= 3 (length kids)))
      (is (string= "UNQUOTE" (node-name (first (node-children (second kids))))))
      (is (string= "UNQUOTE-SPLICING"
                   (node-name (first (node-children (third kids)))))))))

(test feature-single-top-level
  (let* ((ast (parse-lisp-source "#+sbcl (defun f () 1)"))
         (forms (list-top-level ast)))
    (is (eq :list (node-type ast)))
    (is (= 1 (length forms))))
  (let* ((ast (parse-lisp-source "#+(or sbcl ccl) (defun f () 1)"))
         (forms (list-top-level ast)))
    (is (eq :list (node-type ast)))
    (is (= 1 (length forms)))))

(test dispatch-literals-single-form
  (dolist (code (list "#S(a 1)" "#C(1 2)" "#P\"/x\"" "#2A((1))" "#*101"))
    (let* ((ast (parse-lisp-source code))
           (forms (list-top-level ast)))
      (is (eq :list (node-type ast)))
      (is (= 1 (length forms))))))

(test dispatch-literal-markers
  (is (string= "STRUCT" (node-name (first (node-children (first (top-forms-of "#S(a 1)")))))))
  (is (string= "COMPLEX" (node-name (first (node-children (first (top-forms-of "#C(1 2)")))))))
  (is (string= "PATHNAME" (node-name (first (node-children (first (top-forms-of "#P\"/x\"")))))))
  (is (string= "ARRAY" (node-name (first (node-children (first (top-forms-of "#2A((1))")))))))
  (is (string= "BIT-VECTOR" (node-name (first (node-children (first (top-forms-of "#*101"))))))))

(test symbol-escapes-and-bars
  ;; the reader drops the backslash of an escape, so node-name must
  ;; hold the name the reader sees -- otherwise find-top-level-by-name
  ;; and rename can never match a symbol that needed escaping
  (is (string= "a" (node-name (first (top-forms-of "\\a")))))
  (is (string= "some!thing" (node-name (first (top-forms-of "some\\!thing")))))
  (is (string= "(paren)" (node-name (first (top-forms-of "\\(paren\\)")))))
  (is (string= "a\\b" (node-name (first (top-forms-of "a\\\\b")))))
  (let ((kids (node-children (first-form-of "(foo |a b| bar)"))))
    (is (= 3 (length kids)))
    (is (string= "|a b|" (node-name (second kids)))))
  ;; a bar segment keeps its bars and its colons are not separators
  (is (string= "|a b|" (node-name (first (top-forms-of "|a b|")))))
  (is (string= "|x:y|" (node-name (first (top-forms-of "|x:y|")))))
  ;; an escaped colon is part of the name, never a package separator
  (let ((sym (first (top-forms-of "foo\\:bar"))))
    (is (string= "foo:bar" (node-name sym)))
    (is (null (node-package sym))))
  (let ((sym (first (top-forms-of "foo::bar"))))
    (is (string= "bar" (node-name sym)))
    (is (string= "foo" (node-package sym))))
  (let* ((ast (parse-lisp-source "(foo |(a)| bar)"))
         (forms (list-top-level ast)))
    (is (eq :list (node-type ast)))
    (is (= 1 (length forms))))
  (is (string= "|" (node-name (first (top-forms-of "|")))))
  (is (string= "||" (node-name (first (top-forms-of "||"))))))

(test balance-sharp-circular
  (is (= 0 (getf (analyze-balance "'#1=(#1#)") :final-depth)))
  (is (= 0 (getf (analyze-balance "#2A((1 2))") :final-depth)))
  (is (= 0 (getf (analyze-balance "(a #+sbcl b)") :final-depth))))

;;; --- CRLF / CR line endings -------------------------------------
;;; Regression: #\Return was missing from the whitespace rule, so every
;;; CRLF file (Windows checkouts, old CVS archives) failed with a bogus
;;; "Syntax error". Found by sweeping every Lisp file on the machine.

(defvar *cr* (code-char 13))
(defvar *lf* (code-char 10))

(defun crlf (s)
  "S — a FORMAT string — with every newline replaced by CRLF."
  (let ((expanded (format nil s)))
    (with-output-to-string (out)
      (loop for ch across expanded
            do (if (char= ch *lf*)
                   (progn (write-char *cr* out) (write-char *lf* out))
                   (write-char ch out))))))

(test crlf-files-parse
  (dolist (code (list "(a) (b)"
                      "(defun f (x)~%  (+ x 1))"
                      ";; comment~%(a)"
                      "#| block |# (a)"
                      "(list #\\a #\\Space \"s\")"))
    (let ((text (crlf code)))
      (is (not (code-rejected-p text)) "code=~s should parse as CRLF~%" code)
      (is (= (length (top-forms-of (format nil code)))
             (length (top-forms-of text)))
          "CRLF must yield the same forms as LF"))))

(test cr-only-line-endings
  (let ((text (substitute *cr* *lf* (crlf "(a)~%(b)"))))
    (is (= 2 (length (top-forms-of text)))))
  (is (= 0 (length (top-forms-of (string *cr*))))
      "a file holding only a CR holds no forms"))

(test crlf-line-and-column
  ;; "(a)\r\n(bb)\r\n": the second line starts at offset 5, so its
  ;; first 'b' is line 1 column 1 — CRLF must count as ONE break.
  (let* ((text (crlf "(a)~%(bb)~%"))
         (offset (position #\b text)))
    (multiple-value-bind (line col) (offset-to-line-col text offset)
      (is (= 1 line) "second line, CRLF counted once")
      (is (= 1 col) "first column of the second line")
      (is (= offset (offset-to-line-col-inverse text line col)))))
  ;; a CR-only file counts lines too
  (let* ((text (substitute *cr* *lf* (format nil "(a)~%b")))
         (offset (position #\b text)))
    (multiple-value-bind (line col) (offset-to-line-col text offset)
      (is (= 1 line) "CR ends a line too")
      (is (= 0 col) "column resets after CR"))))

;;; --- Deep nesting and huge inputs -------------------------------
;;; Regression: esrap recurses per bracket level and builds one
;;; production per repetition element, so both very deep nesting and
;;; very long literals killed the process on the control stack. Neither
;;; is catchable, so the parser now refuses deep input with a clean
;;; :ERROR node and matches long bodies in one production.

(test deep-nesting-refused-not-crashed
  (let ((deep (concatenate 'string
                           (make-string 1500 :initial-element #\()
                           (make-string 1500 :initial-element #\))))
        (ast (parse-lisp-source
              (concatenate 'string
                           (make-string 1500 :initial-element #\()
                           (make-string 1500 :initial-element #\))))))
    (declare (ignore deep))
    (is (eq :error (node-type ast)))
    (is (search "Nesting too deep" (node-value ast))))
  ;; under the limit it still parses
  (is (eq :list (node-type
                (parse-lisp-source
                 (concatenate 'string
                              (make-string 100 :initial-element #\()
                              (make-string 100 :initial-element #\))))))))

(test long-string-literal-is-one-production
  ;; A ~80KB string full of escapes used to segfault the parser.
  (let* ((body (make-string 40000 :initial-element #\x))
         (text (format nil "(defparameter *d* \"~a\")"
                       (map 'string (lambda (i c)
                                      (if (evenp i) c #\"))
                                    (loop for i from 0
                                          for c across body)))))
    (is (= 1 (length (top-forms-of text))))))

(test many-top-level-forms
  (let ((text (with-output-to-string (out)
                (dotimes (i 5000)
                  (write-string "(defun f" out)
                  (write-string (princ-to-string i) out)
                  (write-string " () 1)" out)
                  (write-char #\Newline out)))))
    (is (= 5000 (length (top-forms-of text))))))

(test reader-conditional-is-not-a-symbol
  ;; "#+sbcl" used to parse as a symbol named #+sbcl, splitting the
  ;; conditional from its forms.
  (is (= 1 (length (top-forms-of "#+sbcl (a)"))))
  (is (= 1 (length (top-forms-of "#-sbcl (a)"))))
  (is (code-rejected-p "#+sbcl")
      "a conditional with no form is not a symbol either")
  (is (eq :symbol (node-type (first (top-forms-of "#:foo"))))
      "#:foo is still a keyword, not a conditional"))

;;; --- Commas outside backquote ------------------------------------
;;; SBCL rejects these, so the balance scan now does too. A backquote
;;; has no closing delimiter: its extent is the rest of the enclosing
;;; form, which the scanner models with an inherited per-bracket flag.

(defun comma-error-count (code)
  (length (getf (analyze-balance code) :errors)))

;;; --- Character names and literals, SBCL-exact -------------------
;;; Found by sweeping every Lisp file on the machine: real code uses
;;; 2-3 letter control mnemonics (#\Dle, #\Nak, #\Ack), SBCL's
;;; code-point escapes (#\U+DF), underscore Unicode names, and #\Vt.

(test char-mnemonic-names
  (dolist (name '("Nul" "Null" "Soh" "Stx" "Etx" "Eot" "Enq" "Ack" "Bel"
                  "Bell" "Backspace" "Bs" "Tab" "Ht" "Newline" "Nl"
                  "Linefeed" "Lf" "Vt" "Page" "Return" "Cr" "So" "Si"
                  "Dle" "Dc1" "Dc2" "Dc3" "Dc4" "Nak" "Syn" "Etb" "Can"
                  "Em" "Sub" "Esc" "Escape" "Fs" "Gs" "Rs" "Us" "Space"
                  "Sp" "Rubout" "Delete" "Del" "Alt" "Altmode" "Lock"))
    (let ((code (concatenate 'string "#\\" name)))
      (is (not (code-rejected-p code)) "code=~s should be a name~%" code)))
  ;; case-insensitive, and a longer name must not be truncated
  (is (string= "NULL" (char-value "#\\null")))
  (is (string= "NULL" (char-value "#\\NULL")))
  (is (code-rejected-p "#\\Nulx"))
  (is (code-rejected-p "#\\Space!"))
  ;; hyphens are NOT part of SBCL's names
  (is (code-rejected-p "#\\Latin-Small-Letter-E-With-Acute"))
  (is (code-rejected-p "#\\Foo-Bar")))

(test char-code-point-escapes
  (is (string= "ß" (char-value "#\\U+DF")))
  (is (string= "ß" (char-value "#\\u+DF")))
  (is (string= "ß" (char-value "#\\uDF")))
  (is (string= "1" (char-value "#\\u0031")))
  (is (string= "A" (char-value "#\\u0041")))
  (is (string= "😀" (char-value "#\\U+1F600")))
  (is (code-rejected-p "#\\x41"))
  ;; "#\u" alone is just the character u
  (is (string= "u" (char-value "#\\u"))))

(test char-unicode-underscore-names
  (is (not (code-rejected-p "#\\LATIN_SMALL_LETTER_E_WITH_ACUTE")))
  (is (not (code-rejected-p "#\\NO_BREAK_SPACE"))))

(test unicode-symbol-constituents
  ;; the reader makes ANY non-ASCII character a symbol constituent
  (let ((section (string (code-char 167)))
        (laquo (string (code-char 171))))
    (is (= 1 (length (top-forms-of (concatenate 'string section "derpy")))))
    (is (= 1 (length (top-forms-of (concatenate 'string laquo "x" laquo)))))
    (is (= 1 (length (top-forms-of "λfoo"))))))

(test empty-and-long-literals
  ;; an empty string body consumes nothing, which esrap rejects unless
  ;; the terminal explicitly reports success-without-progress
  (dolist (code (list "\"\"" "\"\" \"\"" "(a \"\")" "\"\"\"\""))
    (is (not (code-rejected-p code)) "code=~s should parse~%" code))
  (is (string= "" (char-value "\"\""))))

(test block-comment-is-opaque
  ;; quotes, semicolons, bars and brackets inside #| ... |# are text
  (dolist (code (list "#| \" | (a) ; |# (b)"
                      "#| , |# (b)"
                      "#| #| , |# , |# (b)"
                      "#| (unbalanced |# (b)"))
    (is (not (code-rejected-p code)) "code=~s should parse~%" code))
  ;; deleting a quote inside a block comment changes nothing
  (is (= 1 (length (top-forms-of (format nil "#| \" |# (a)"))))))

;;; --- Feature conditionals skip unreadable branches ---------------
;;; The reader never even looks at the branch of an absent feature, so
;;; a file may legally contain "#-other-lisp #\Name-Only-That-Lisp-Knows"
;;; and still load. We validate live branches with the real grammar and
;;; fall back to a permissive scan for anything else.

(test feature-conditional-skips-foreign-branch
  (dolist (code (list "(list #-other-lisp #\\Name-Only-That-Lisp-Knows)"
                      "(list #+other-lisp #\\Replacement-Character)"
                      "(let ((x #-(or abcl lispworks) #\\Some-Other-Lisp-Name))
                         x)"
                      "(list #+sbcl #| a block comment |# 5)"))
    (is (not (code-rejected-p code)) "code=~s should parse~%" code)))

(test feature-conditional-keeps-live-branch-ast
  ;; a branch that IS valid keeps a real AST, not a raw-text blob
  (let* ((kids (node-children (first-form-of "(list #+sbcl (a b))")))
         (branch (second kids))
         (branch-kids (node-children branch)))
    (is (string= "list" (node-name (first kids))))
    (is (eq :list (node-type branch)))
    ;; children: the "#+" marker, the feature, and the target form
    (is (= 3 (length branch-kids)))
    (is (eq :list (node-type (third branch-kids))))
    (is (= 2 (length (node-children (third branch-kids))))))
  ;; and the whole conditional is still ONE top-level form
  (is (= 1 (length (top-forms-of "#+sbcl (a)")))))

;;; --- Brackets are constituent characters, not delimiters ---------
;;; CLHS 2.1.3 makes [ ] { } constituent characters, and every reader
;;; honours that: "[1]" is the SYMBOL |[1]|, not a list holding 1. A
;;; parser that treats them as delimiters disagrees with the reader it
;;; is supposed to model -- and the disagreement is silent, because the
;;; enclosing form still parses, just with the wrong shape.

(test brackets-are-symbol-constituents
  (dolist (code (list "[1]" "[]" "{}" "{a}" "[1 2]" "[a]b" "{a}b" "[|a|]"
                      "1]" "1}" "1{" "1[" "(f [1])" "(f [1] [2])"
                      "[#\\a]" "(let ([x 1]) x)"))
    (is (not (code-rejected-p code)) "code=~s should parse~%" code)))

(test bracket-symbols-have-the-right-names
  (is (string= "[1]" (single-symbol-name "[1]")))
  (is (string= "[]" (single-symbol-name "[]")))
  ;; a space still ends a name, so "[1 2]" is the two symbols [1 and 2]
  (let ((forms (top-forms-of "[1 2]")))
    (is (= 2 (length forms)))
    (is (string= "[1" (node-name (first forms))))
    (is (string= "2]" (node-name (second forms)))))
  ;; the AST keeps the name as written (it does not upcase symbols)
  (is (string= "[a]b" (single-symbol-name "[a]b")))
  (is (string= "{a}b" (single-symbol-name "{a}b")))
  (is (string= "1]" (single-symbol-name "1]")))
  ;; ... and the bracket is part of the name, not a wrapper: [1] is one
  ;; symbol, while (f [1]) is a two-element list F / [1]
  (let ((kids (node-children (first-form-of "(f [1])"))))
    (is (= 2 (length kids)))
    (is (string= "f" (node-name (first kids))))
    (is (string= "[1]" (node-name (second kids))))))

(test unmatched-bracket-is-still-an-error
  ;; brackets being constituents does not make them balance: a stray
  ;; ")" still closes a list that was never opened
  (dolist (code (list "[)]" "[1)" "(1}"))
    (is (code-rejected-p code) "code=~s should fail~%" code)))

(test unknown-single-word-char-names-still-rejected
  ;; SBCL knows a large table of Unicode names (#\CARON, #\BREVE); we
  ;; know the ASCII mnemonics plus underscore-separated names. Refusing
  ;; an unknown name is the safe direction, so these stay errors.
  (dolist (code (list "#\\Caron" "#\\Breve" "#\\NBSP" "#\\AB"))
    (is (code-rejected-p code) "code=~s is not a name we know~%" code)))

(test comma-outside-backquote-is-an-error
  (dolist (code (list "(a ,b)" "(a ,@b)" "(a . ,b)" ",x" ",@x"
                      "(list #,x)" "'(a ,b)" "(a `(b)) ,c"))
    (is (plusp (comma-error-count code)) "code=~s should be flagged~%" code)))

(test comma-inside-backquote-is-fine
  (dolist (code (list "`x" "`(a ,b)" "`(a ,@b)" "(a `(b ,c))"
                      "(a `(b `(c ,d)))" "(let ((x 1)) `,x)"))
    (is (zerop (comma-error-count code)) "code=~s should be clean~%" code))
  (is (zerop (comma-error-count "(a \"str,with,commas\")"))
      "commas inside strings are not commas")
  (is (zerop (comma-error-count "(a |,c|)"))
      "commas inside bar symbols are not commas")
  (is (zerop (comma-error-count (format nil "(a) ; ,c~%")))
      "commas inside comments are not commas"))

(test balance-unclosed-extras
  (is (plusp (length (getf (analyze-balance "(a \"open") :errors))))
  (is (plusp (length (getf (analyze-balance "#| never closed") :errors))))
  (is (plusp (length (getf (analyze-balance ")") :errors))))
  (is (= 1 (getf (analyze-balance "(a") :final-depth)))
  (is (= 3 (getf (analyze-balance "(((") :max-depth)))
  ;; [ ] { } are constituent characters, not brackets: they neither open
  ;; a level nor complain about being unclosed (CLHS 2.1.3)
  (is (= 0 (getf (analyze-balance "[a") :final-depth)))
  (is (= 1 (getf (analyze-balance "{[(") :max-depth))) ; only the ( counts
  (is (zerop (length (getf (analyze-balance "]a[") :errors)))))

(test format-close-dedent
  (is (string= (format nil "(a~%)") (format-source (format nil "(a~%)"))))
  (is (string= (format nil "(defun f ()~%  1~%)")
               (format-source (format nil "(defun f ()~%  1~%)")))))

(test format-escaped-string
  (let ((code "(a \"b\\\"c\")"))
    (is (string= code (format-source code)))))

(test format-block-comment-gap
  ;; stale need-indent used to eat the space after |# before a form
  (is (string= "#| hi |# (a)" (format-source "#| hi |# (a)"))))

(test extract-range-basic
  (let* ((text "(a) (b)")
         (nodes (extract-range (parse-lisp-source text) text 0 0 0 3)))
    (is (= 1 (length nodes))))
  (let* ((text (format nil "(a)~%(b)"))
         ;; range end is exclusive: [0,4) touches (b) at its start only
         (one (extract-range (parse-lisp-source text) text 0 0 1 0))
         ;; a range covering everything returns the root itself
         (root (extract-range (parse-lisp-source text) text 0 0 1 3)))
    (is (= 1 (length one)))
    (is (= 1 (length root)))
    (is (eq :list (node-type (first root))))
    (is (= 2 (node-form-count (first root))))))

(test find-form-starting-exact-and-miss
  (let* ((text "(a)")
         (ast (parse-lisp-source text)))
    (is (string= "a" (node-name (cl-toolkit::find-form-starting-at ast text 0 1))))
    (is (null (cl-toolkit::find-form-starting-at ast text 0 3)))))

(test top-level-node-at-oob
  (is (null (ignore-errors (top-level-node-at "(a)" 5)))))

(test unique-containing-zero-and-multi
  (is (null (ignore-errors (cl-toolkit::unique-containing-top-level "(a)" "(zzz)" nil))))
  (is (null (ignore-errors (cl-toolkit::unique-containing-top-level "(a) (a)" "(a)" nil)))))

(test unique-anchor-zero-and-multi
  (is (null (ignore-errors (unique-anchor-offset "(a)" "(zzz)"))))
  (is (null (ignore-errors (unique-anchor-offset "(a) (a)" "(a)")))))

(test count-empty-and-find-forms-empty
  (multiple-value-bind (c o) (count-text-occurrences "(a)" "")
    (is (= 0 c)) (is (= -1 o)))
  (is (null (ignore-errors (find-forms-containing "(a)" "")))))

(test node-form-names
  (is (string= "v" (node-form-name (first-form-of "(defvar v 1)"))))
  (is (string= "t1" (node-form-name (first-form-of "(test t1 (is t))"))))
  (is (string= "s" (node-form-name (first-form-of "s"))))
  (is (string= "7" (node-form-name (first-form-of "7"))))
  (is (string= "\"hi\"" (node-form-name (first-form-of "\"hi\""))))
  ;; a wrapper list names what it wraps, so --name reaches through
  (is (string= "a" (node-form-name (first-form-of "((defun a () 1))"))))
  (is (= 2 (node-form-count (parse-lisp-source "(a) (b)"))))
  (is (= 1 (node-form-count (first-form-of "(a)")))))

(test find-top-level-case-insensitive
  (is (not (null (find-top-level-by-name "(defun Alpha () 1)" "alpha")))))

(test top-level-node-by-name-missing
  (is (null (ignore-errors (cl-toolkit::top-level-node-by-name "(a)" (list :name "nope"))))))

(test offset-forward
  (multiple-value-bind (l c) (offset-to-line-col (format nil "(a)~%(b)") 0)
    (is (= 0 l)) (is (= 0 c)))
  (multiple-value-bind (l c) (offset-to-line-col (format nil "(a)~%(b)") 4)
    (is (= 1 l)) (is (= 0 c))))

(test validate-empty-list-warning
  (let ((r (validate (parse-lisp-source "()"))))
    (is (getf r :balanced))
    (is (plusp (length (getf r :warnings))))))

(test delete-variants
  (let ((r (delete-top-level-at (format nil "(a)~%(b)") 1)))
    (is (search "(a)" r))
    (is (null (search "(b)" r))))
  (is (search "(a)" (cl-toolkit::delete-last-top-level (format nil "(a)~%(b)"))))
  (is (null (search "(b)" (cl-toolkit::delete-last-top-level (format nil "(a)~%(b)")))))
  (is (null (ignore-errors (delete-top-level-at "(a)" 9)))))

(test replace-top-level-variants
  (is (search "(B)" (cl-toolkit::replace-top-level-at "(a)" 0 "(B)")))
  (is (search "(Z)" (cl-toolkit::replace-last-top-level "(a)" "(Z)"))))

(test parse-multi-forms-pairs
  (let ((pairs (cl-toolkit::parse-multi-forms "(a) (b)")))
    (is (= 2 (length pairs)))
    (is (= 0 (first (first pairs))))))

(test move-noop-and-into-itself
  (let ((text (format nil "(a)~%(b)")))
    (is (string= text (move-form text 0 0 0 0)))
    ;; moving inner (b) to after its own outer form signals
    (is (null (ignore-errors (move-form "(a (b))" 0 4 0 0))))))

(test move-source-after-dest
  (let ((r (move-form (format nil "(a)~%(b)~%(c)") 2 0 0 0)))
    (is (search "(c)" r))
    (is (search "(b)" r))
    (is (not (char= #\Newline (char r 0))))))

(test insert-promotes-symbol-to-parent
  (let ((r (insert-form-at "(foo bar)" 0 5 "(X)")))
    (is (search "(X)" r))
    (is (search "(foo bar)" r))))

(test append-fallback-and-replace-miss
  (is (search "ZZ" (cl-toolkit::append-form-at "" 0 0 "ZZ")))
  (is (null (ignore-errors (replace-form-at "(a)" 5 0 "(b)")))))

(test find-all-nesting
  (let ((all (cl-toolkit::find-node-at-offset-all (parse-lisp-source "(a (b))") 1)))
    (is (>= (length all) 3))))

(test split-jammed-with-comment
  (let ((r (split-jammed-top-level "(a) ; c (b)")))
    (is (search "(b)" r))))

(test net-depth-ignores-string
  (is (= 0 (net-depth-delta "(a \"(\")" "(b)"))))

(test lisp-file-pred
  (is (cl-toolkit::lisp-file-p nil))
  (is (cl-toolkit::lisp-file-p "x.lisp"))
  (is (cl-toolkit::lisp-file-p "x.ASD"))
  (is (not (cl-toolkit::lisp-file-p "x.md"))))

(test backup-path-shapes
  (is (search ".bak" (cl-toolkit::backup-path-for "f.lisp")))
  (is (search ".lisp.bak" (cl-toolkit::timestamped-backup-path "f.lisp" "/tmp"))))

(test unified-diff-shapes
  (is (null (cl-toolkit::generate-unified-diff "(a)" "(a)" "f.lisp")))
  (is (search "@@" (cl-toolkit::generate-unified-diff "(a)" "(b)" "f.lisp"))))

(test single-line-preview-shapes
  (let* ((text "(defun aaaaaaaaaa-bbbbbbbbbb-cccccccccc () 1)")
         (node (first-form-of text)))
    (is (search "..." (cl-toolkit::single-line-preview text node 10))))
  (let* ((text (format nil "(defun f ()~%  1)"))
         (node (first-form-of text)))
    (is (not (search (string #\Newline) (cl-toolkit::single-line-preview text node))))))

(test find-forms-containing-basic
  (is (= 1 (length (find-forms-containing "(a 1) (b 2)" "1")))))

(test duplicate-triple
  (is (= 3 (length (first (duplicate-top-level-forms "(x) (x) (x)"))))))

(test insert-end-no-double-newline
  (is (string= (format nil "p~%(n)~%") (insert-form-end (format nil "p~%") "(n)"))))

(test parse-file-roundtrip
  (let ((p "/tmp/ctk-parse-test.lisp"))
    (with-open-file (s p :direction :output :if-exists :supersede)
      (write-string "(defun q () 1)" s))
    (unwind-protect
         (is (eq :list (node-type (parse-file p))))
      (ignore-errors (delete-file p))))
  (is (null (ignore-errors (parse-file "/tmp/ctk-nope-missing.lisp")))))

(test write-result-roundtrip
  (let ((p "/tmp/ctk-write-test.lisp"))
    (with-open-file (s p :direction :output :if-exists :supersede)
      (write-string "(a)" s))
    (unwind-protect
         (progn
           (cl-toolkit::write-result-to-file p "(b)" t)
           (is (search "(b)" (read-file-to-string p)))
           (is (probe-file (concatenate 'string p ".bak"))))
      (ignore-errors (delete-file p))
      (ignore-errors (delete-file (concatenate 'string p ".bak")))))
  (is (null (ignore-errors (cl-toolkit::write-result-to-file "/tmp/ctk-nope-missing.lisp" "(a)")))))

(test apply-single-edit-all-ops
  (let ((text (format nil "(defun a () 1)~%(defun b () 2)")))
    (is (search "a2" (apply-single-edit text (list :operation :replace-name :name "a" :code "(defun a2 () 1)"))))
    (is (null (find-top-level-by-name (apply-single-edit text (list :operation :delete-name :name "a")) "a")))
    (is (search "(defun n () 0)" (apply-single-edit text (list :operation :insert-after-name :name "a" :code "(defun n () 0)"))))
    (is (search "9" (apply-single-edit text (list :operation :replace-match :match "(defun b () 2)" :code "9"))))
    (is (null (search "gamma" (apply-single-edit "(g (gamma))" (list :operation :delete-match :match "(gamma)")))))
    (is (search "(R)" (apply-single-edit text (list :operation :replace-index :index 0 :code "(R)"))))
    (is (search "(P)" (apply-single-edit text (list :operation :replace-position :line 0 :col 0 :code "(P)"))))
    (is (null (find-top-level-by-name (apply-single-edit text (list :operation :delete-index :index 0)) "a")))
    (is (search "(I)" (apply-single-edit text (list :operation :insert-after-index :index 0 :code "(I)"))))
    (is (null (ignore-errors (apply-single-edit text (list :operation :nope)))))
    (is (null (ignore-errors (apply-single-edit text (list :operation :replace-match :match "defun a" :code "z")))))
    (is (search "z" (apply-single-edit text (list :operation :replace-match :match "defun a" :code "z" :allow-fuzzy t))))))

(test batch-missing-op-and-order
  (is (null (ignore-errors (apply-batch-edits "(a)" (list (list :code "(b)"))))))
  (let ((r (apply-batch-edits (format nil "(defun a () 1)~%(defun b () 2)")
                              (list (list :operation :replace-name :name "b" :code "(defun b2 () 2)")
                                    (list :operation :delete-index :index 0)))))
    (is (null (find-top-level-by-name r "a")))
    (is (not (null (find-top-level-by-name r "b2"))))))

(test batch-error-names-failing-edit
  ;; the failure names its 1-based position and operation
  (handler-case
      (progn
        (apply-batch-edits
         "(defun a () 1)"
         (list (list :operation :replace-name :name "a" :code "(defun b () 1)")
               (list :operation :delete-name :name "missing")))
        (is nil "batch should have signalled"))
    (error (c)
      (let ((msg (princ-to-string c)))
        (is (search "Batch edit 2" msg))
        (is (search "DELETE-NAME" msg))))))

(test batch-match-with-selectors
  (let ((text "(defun d () (v 1) (v 2))"))
    (is (search "(w 1)" (apply-batch-edits text (list (list :operation :replace-match :match "(v 1)" :code "(w 1)")))))
    (is (search "(w 2)" (apply-batch-edits text (list (list :operation :replace-match :match "(v 1)" :code "(w 2)" :first t :allow-fuzzy t)))))
    (is (search "(w 2)" (apply-batch-edits text (list (list :operation :replace-match :match "(v" :code "(w 2)" :occurrence 2 :allow-fuzzy t)))))))

(test rename-wrap-unwrap-spans
  ;; rename a definition slot
  (is (search "(defun bar () 1)"
             (rename-node-in-text
              "(defun foo () 1)"
              (find-top-level-by-name "(defun foo () 1)" "foo")
              "bar")))
  ;; rename a call operator
  (is (search "(bar 1)"
             (rename-node-in-text
              "(foo 1)"
              (find-top-level-by-name "(foo 1)" "foo")
              "bar")))
  ;; references elsewhere are untouched
  (is (search "(foo 2)"
             (rename-node-in-text
              "(defun foo () 1) (foo 2)"
              (find-top-level-by-name "(defun foo () 1) (foo 2)" "foo")
              "bar")))
  ;; a bad new name refuses loudly
  (is (null (ignore-errors
              (rename-node-in-text "(foo 1)"
                                   (find-top-level-by-name "(foo 1)" "foo")
                                   "(bar"))))
  ;; wrap is one atomic splice
  (is (search "(progn (foo 1))"
             (wrap-node-in-text
              "(foo 1)"
              (find-top-level-by-name "(foo 1)" "foo")
              "(progn " ")")))
  ;; unwrap a single child
  (is (search "(foo 1)"
             (unwrap-node-in-text
              "((foo 1))"
              (first (list-top-level (parse-lisp-source "((foo 1))"))))))
  ;; multi-child and empty unwraps refuse
  (is (null (ignore-errors
              (unwrap-node-in-text
               "(a b)"
               (first (list-top-level (parse-lisp-source "(a b)")))))))
  (is (null (ignore-errors
              (unwrap-node-in-text
               "()"
               (first (list-top-level (parse-lisp-source "()"))))))))

(test rename-wrap-unwrap-batch-ops
  (let ((text "(defun a () 1)"))
    (is (search "(defun b () 1)"
                (apply-single-edit text (list :operation :rename-name
                                              :name "a" :to "b"))))
    (is (search "(progn (defun a () 1))"
                (apply-single-edit text (list :operation :wrap-name
                                              :name "a"
                                              :open "(progn " :close ")"))))
    (is (search "(defun a () 1)"
                (apply-single-edit "((defun a () 1))"
                                   (list :operation :unwrap-name
                                         :name "a")))))
  (is (null (ignore-errors
              (apply-single-edit "(defun a () 1)"
                                 (list :operation :rename-name :name "a")))))
  (is (null (ignore-errors
              (apply-single-edit "(defun a () 1)"
                                 (list :operation :wrap-name :name "a"
                                       :open "(progn "))))))

(test subform-global-policies
  (is (null (ignore-errors (cl-toolkit::find-subform-globally "(defun d () (v s) (v s))" "(v s)"))))
  (is (not (null (cl-toolkit::find-subform-globally "(defun d () (v s))" "(v s)" :first t))))
  (is (not (null (cl-toolkit::find-subform-globally "(defun d () (a) (b))" "(b)" :occurrence 1))))
  (is (null (ignore-errors (cl-toolkit::find-subform-globally "(defun d () (v 1))" "(v" :match-exact t)))))

(test resolve-target-policies
  (let ((text "(defun d () (v s) (v s))")
        (top (first-form-of "(defun d () (v s) (v s))")))
    (is (null (ignore-errors (cl-toolkit::resolve-replace-target text top "(v s)"))))
    (is (null (ignore-errors (cl-toolkit::resolve-replace-target text top "(v s)" :occurrence 9))))
    (is (not (null (cl-toolkit::resolve-replace-target text top "(v s)" :first t))))
    (is (null (ignore-errors (cl-toolkit::resolve-replace-target text top "zzz"))))
    (is (null (ignore-errors (cl-toolkit::resolve-replace-target text top "v s" :match-exact t))))))

