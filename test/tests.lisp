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
  (is (string= "\\a" (node-name (first (top-forms-of "\\a")))))
  (let ((kids (node-children (first-form-of "(foo |a b| bar)"))))
    (is (= 3 (length kids)))
    (is (string= "|a b|" (node-name (second kids)))))
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

(test batch-match-with-selectors
  (let ((text "(defun d () (v 1) (v 2))"))
    (is (search "(w 1)" (apply-batch-edits text (list (list :operation :replace-match :match "(v 1)" :code "(w 1)")))))
    (is (search "(w 2)" (apply-batch-edits text (list (list :operation :replace-match :match "(v 1)" :code "(w 2)" :first t :allow-fuzzy t)))))
    (is (search "(w 2)" (apply-batch-edits text (list (list :operation :replace-match :match "(v" :code "(w 2)" :occurrence 2 :allow-fuzzy t)))))))

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

