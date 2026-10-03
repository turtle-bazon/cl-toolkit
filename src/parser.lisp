(in-package #:cl-toolkit)

;;; ============================================================
;;; High-level parser API
;;; ============================================================

(defun parse-file (path)
  "Parse a Lisp file at PATH. Returns AST root node.
   Buffered read-to-EOF: FILE-LENGTH counts bytes, not characters,
   so multibyte UTF-8 leaves NUL padding and pipes report bogus lengths."
  (let ((text (with-open-file (stream path :direction :input :if-does-not-exist nil)
                (when stream
                  (with-output-to-string (out)
                    (let ((buf (make-string 4096)))
                      (loop for n = (read-sequence buf stream)
                            do (write-sequence buf out :end n)
                            while (= n (length buf)))))))))
    (unless text
      (error "Cannot read file: ~a" path))
    (let ((ast (cl-toolkit-grammar::parse-lisp-source text)))
      (setf (getf ast :source) (namestring path))
      ast)))

(defun parse-string (text)
  "Parse a string of Lisp code. Returns AST root node."
  (cl-toolkit-grammar::parse-lisp-source text))

;;; --- Helper functions (defined before use) ---

(defun whitespace-char-p (ch)
  "True for any character the Lisp reader treats as whitespace.
   CLHS 2.1.1: Space, Tab, Newline, Return, Page. Vt is an SBCL
   extension, kept for parity with the reader we model."
  (or (char= ch #\Space) (char= ch #\Tab) (char= ch #\Newline)
      (char= ch #\Return) (char= ch #\Page) (char= ch #\Vt)))

(defun line-break-char-p (ch)
  "True for characters that end a line. CRLF is one break, not two:
   callers must skip the LF of a CRLF pair."
  (or (char= ch #\Newline) (char= ch #\Return)))

(defun skip-whitespace-and-newlines (text offset)
  "Skip whitespace and at most one line break after OFFSET."
  (let ((i offset) (len (length text)))
    (loop while (< i len)
          do (let ((ch (char text i)))
               (cond
                 ((char= ch #\Space) (incf i))
                 ((char= ch #\Tab) (incf i))
                 ((char= ch #\Page) (incf i))
                 ((char= ch #\Vt) (incf i))
                 ((line-break-char-p ch)
                  (incf i)
                  ;; CRLF: consume the LF as part of the same break
                  (when (and (char= ch #\Return)
                             (< i len) (char= (char text i) #\Newline))
                    (incf i))
                  (return))
                 (t (return)))))
    i))

(defun find-node-at-offset (node offset)
  "Find the deepest node containing OFFSET (0-indexed).
   When offset is in whitespace before a form, returns that form."
  (when (and node
             (node-start node) (node-end node)
             (>= offset (node-start node))
             (< offset (node-end node)))
    (let ((best node) (next-nearest nil))
      (dolist (child (node-children node))
        (when (nodep child)
          (let ((child-start (node-start child))
                (child-end (node-end child)))
            (when (and child-start child-end
                       (>= offset child-start)
                       (< offset child-end))
              ;; Child contains offset exactly - recurse into it
              (let ((found (find-node-at-offset child offset)))
                (when found
                  (setf best found)
                  (return))))
            ;; Track nearest child that starts after offset (whitespace before form)
            (when (and child-start (> child-start offset))
              (when (or (null next-nearest)
                        (< child-start (node-start next-nearest)))
                (setf next-nearest child))))))
      ;; If no child contained the offset, return nearest next form
      (if (and (eq best node) next-nearest)
          next-nearest
          best))))

(defun find-node-at-offset-all (node offset)
  "Find ALL nodes containing OFFSET (0-indexed), from outermost to innermost."
  (when (and node
             (node-start node) (node-end node)
             (>= offset (node-start node))
             (< offset (node-end node)))
    (cons node
          (loop for child in (node-children node)
                when (nodep child)
                append (find-node-at-offset-all child offset)))))

(defun extract-nodes-in-range (node start-offset end-offset)
  "Find nodes that overlap [start-offset, end-offset)."
  (when (and node (node-start node) (node-end node))
    (let ((node-start (node-start node))
          (node-end (node-end node)))
      (cond
        ;; Node is completely outside range
        ((or (<= node-end start-offset) (>= node-start end-offset))
         nil)
        ;; Node is completely inside range
        ((and (>= node-start start-offset) (<= node-end end-offset))
         (list node))
        ;; Node overlaps range - check children
        (t
         (let ((children (node-children node)))
           (if children
               (loop for child in children
                     when (nodep child)
                     append (extract-nodes-in-range child start-offset end-offset))
               (list node))))))))


;;; --- Position-based queries ---

(defun find-form-starting-at (ast text line col)
  "Find the smallest form whose source STARTS exactly at LINE, COL.
   Returns NIL when nothing starts there — no nearest-match guessing.
   Destructive operations default to this so a wrong position fails
   loudly instead of silently editing an adjacent form."
  (let* ((target-offset (cl-toolkit-ast:offset-to-line-col-inverse text line col))
         (best nil))
    (labels ((walk (node)
               (when (and (nodep node)
                          (node-start node) (node-end node)
                          (= target-offset (node-start node)))
                 (when (or (null best)
                           (< (- (node-end node) (node-start node))
                              (- (node-end best) (node-start best))))
                   (setf best node)))
               (dolist (child (node-children node))
                 (walk child))))
      ;; never return the root itself
      (dolist (top (list-top-level ast))
        (walk top))
      best)))

(defun find-form-at (ast text line col)
  "Find the form to operate on at the given LINE and COL (0-indexed).
   Finds the smallest form that contains the target offset and whose start
   line is at or before LINE. This means the cursor can be anywhere inside
   a form and it will target that form, not drill into subforms.
   Never returns the root node (the outermost AST node spanning the entire
   file) — that would cause replace to destroy the entire file."
  (let* ((target-offset (cl-toolkit-ast:offset-to-line-col-inverse text line col))
         (all-nodes (find-node-at-offset-all ast target-offset))
         (best nil))
    ;; Among all nodes containing the offset, find the one with the
    ;; latest start line (but not after target line). If multiple nodes
    ;; start on the same line, prefer the innermost (smallest) one.
    ;; Skip the first node (root/outermost) to prevent data loss.
    (dolist (node (rest all-nodes))
      (when (and (node-start node) (node-end node)
                 (>= target-offset (node-start node))
                 (< target-offset (node-end node)))
        (multiple-value-bind (node-line node-col)
            (cl-toolkit-ast:offset-to-line-col text (node-start node))
          (declare (ignore node-col))
          (when (<= node-line line)
            (if (null best)
                (setf best node)
                (multiple-value-bind (best-line best-col)
                    (cl-toolkit-ast:offset-to-line-col text (node-start best))
                  (declare (ignore best-col))
                  ;; Prefer the node with the LATER start line (closer to target)
                  ;; as that's the more specific form. If same line, prefer
                  ;; the one with larger start offset (innermost on that line).
                  (when (or (> node-line best-line)
                            (and (= node-line best-line)
                                 (> (node-start node) (node-start best))))
                    (setf best node))))))))
    best))

;;; --- Range extraction ---

(defun extract-range (ast text start-line start-col end-line end-col)
  "Extract a range of source text as an AST subtree.
   Returns a list of nodes that overlap the given range (0-indexed)."
  (let ((start-offset (cl-toolkit-ast:offset-to-line-col-inverse text start-line start-col))
        (end-offset (cl-toolkit-ast:offset-to-line-col-inverse text end-line end-col)))
    (extract-nodes-in-range ast start-offset end-offset)))

;;; --- Validation ---

(defun validate (ast)
  "Validate an AST. Returns a plist with :balanced, :errors, :warnings."
  (let ((errors nil)
        (warnings nil))
    (labels ((check-node (node depth)
               (when (nodep node)
                 (when (node-error-p node)
                   (push (list (node-line node) (node-col node)
                               (node-value node))
                         errors))
                 (when (node-list-p node)
                   (let ((children (node-children node)))
                     (when (null children)
                       (push (list (node-line node) (node-col node)
                                   "Empty list")
                             warnings))
                     (dolist (child children)
                       (check-node child (1+ depth))))))))
      (check-node ast 0))
    (list :balanced (null errors)
          :errors (nreverse errors)
          :warnings (nreverse warnings))))

;;; --- Top-level forms ---

(defun list-top-level (ast)
  "List all top-level forms in the AST. Returns list of nodes."
  (if (node-list-p ast)
      (node-children ast)
      (list ast)))

;;; ============================================================
;;; Modification Operations
;;; ============================================================

(defun parse-for-edit (text recovery)
  "Parse TEXT for editing operations. When RECOVERY is T, use error recovery."
  (if recovery
      (cl-toolkit-grammar::parse-with-recovery text)
      (cl-toolkit-grammar::parse-lisp-source text)))

(defun find-top-level-by-name (text name &key recovery)
  "Find the first top-level form whose name matches NAME.
   NAME is compared case-insensitively against the first symbol in each form.
   Returns the node, or NIL if not found."
  (let* ((ast (parse-for-edit text recovery))
         (forms (list-top-level ast)))
    (loop for form in forms
          for form-name = (node-form-name form)
          when (and form-name
                    (string-equal form-name name))
            return form)))

(defun delete-node-from-text (text node)
  "Delete NODE's source range from TEXT. Handles whitespace cleanup."
  (let ((start (node-start node))
        (end (node-end node)))
    (unless (and start end)
      (error "Node has no position information"))
    ;; Include trailing whitespace/newline
    (let ((actual-end (skip-whitespace-and-newlines text end)))
      (concatenate 'string
                   (subseq text 0 start)
                   (subseq text actual-end)))))

(defun delete-form-at (text line col &key recovery)
  "Delete the form at LINE, COL (0-indexed) from TEXT.
   When RECOVERY is T, use error recovery parser.
   Returns the modified source string."
  (let* ((ast (parse-for-edit text recovery))
         (node (find-form-at ast text line col)))
    (unless node
      (error "No form found at line ~a, col ~a" line col))
    (delete-node-from-text text node)))

(defun delete-top-level-at (text index &key recovery)
  "Delete the top-level form at INDEX (0-based) from TEXT.
   When RECOVERY is T, use error recovery parser.
   Returns the modified source string."
  (let* ((ast (parse-for-edit text recovery))
         (forms (list-top-level ast)))
    (when (or (< index 0) (>= index (length forms)))
      (error "Index ~a out of range (0-~a)" index (1- (length forms))))
    (delete-node-from-text text (nth index forms))))

(defun delete-last-top-level (text &key recovery)
  "Delete the last top-level form from TEXT.
   Returns the modified source string."
  (let* ((ast (parse-for-edit text recovery))
         (forms (list-top-level ast)))
    (when (null forms)
      (error "No top-level forms found"))
    (delete-node-from-text text (first (last forms)))))

(defun parse-multi-forms (text &key recovery)
  "Parse TEXT which may contain multiple top-level forms.
   Returns a list of (start end) pairs for each form."
  (let* ((ast (parse-for-edit text recovery))
         (forms (list-top-level ast)))
    (mapcar (lambda (f) (list (node-start f) (node-end f))) forms)))

(defun replace-top-level-at (text index new-code &key recovery)
  "Replace the top-level form at INDEX (0-based) with NEW-CODE in TEXT.
   NEW-CODE may contain multiple top-level forms.
   When RECOVERY is T, use error recovery parser.
   Returns the modified source string."
  (let* ((ast (parse-for-edit text recovery))
         (forms (list-top-level ast)))
    (when (or (< index 0) (>= index (length forms)))
      (error "Index ~a out of range (0-~a)" index (1- (length forms))))
    (let ((node (nth index forms)))
      (let ((start (node-start node))
            (end (node-end node)))
        (concatenate 'string
                     (subseq text 0 start)
                     new-code
                     (subseq text end))))))

(defun replace-last-top-level (text new-code &key recovery)
  "Replace the last top-level form in TEXT with NEW-CODE.
   Returns the modified source string."
  (let* ((ast (parse-for-edit text recovery))
         (forms (list-top-level ast)))
    (when (null forms)
      (error "No top-level forms found"))
    (let ((node (first (last forms))))
      (let ((start (node-start node))
            (end (node-end node)))
        (concatenate 'string
                     (subseq text 0 start)
                     new-code
                     (subseq text end))))))

;;; ============================================================
;;; Indentation Helpers (for --pretty)
;;; ============================================================

(defun detect-form-indentation (text node)
  "Detect the leading indentation of NODE in TEXT.
   Returns the number of leading spaces."
  (let* ((start (node-start node))
         (line-start (let ((i start))
                       (loop while (and (> i 0)
                                        (char/= (char text (1- i)) #\Newline))
                             do (decf i))
                       i)))
    ;; Count leading spaces
    (let ((indent 0))
      (loop for i from line-start below start
            when (char= (char text i) #\Space)
              do (incf indent)
            else do (return))
      indent)))

(defun indent-code-by (code additional-indent)
  "Add ADDITIONAL-INDENT spaces to every non-blank line of CODE.
   Blank lines stay empty to avoid trailing whitespace."
  (if (<= additional-indent 0)
      code
      (let ((prefix (make-string additional-indent :initial-element #\Space)))
        (with-output-to-string (out)
          (loop for line-start = 0
                then (if (< line-end (length code))
                         (1+ line-end)
                         nil)
                while line-start
                for line-end = (or (position #\Newline code :start line-start)
                                   (length code))
                for line-text = (subseq code line-start line-end)
                do (unless (= (length (string-trim '(#\Space #\Tab) line-text)) 0)
                     (write-string prefix out))
                   (write-string line-text out)
                   (when (< line-end (length code))
                     (write-char #\Newline out)))))))

(defun indent-continuation-lines (code additional-indent)
  "Add ADDITIONAL-INDENT spaces to every non-blank continuation line.
   The first line keeps its own leading whitespace — the splice point
   already accounts for the original form's base indentation."
  (if (<= additional-indent 0)
      code
      (let ((prefix (make-string additional-indent :initial-element #\Space)))
        (with-output-to-string (out)
          (loop for line-start = 0
                then (if (< line-end (length code))
                         (1+ line-end)
                         nil)
                while line-start
                for line-end = (or (position #\Newline code :start line-start)
                                   (length code))
                for line-text = (subseq code line-start line-end)
                do (when (and (not (zerop line-start))
                              (> (length (string-trim '(#\Space #\Tab) line-text)) 0))
                     (write-string prefix out))
                   (write-string line-text out)
                   (when (< line-end (length code))
                     (write-char #\Newline out)))))))

(defun replace-form-pretty (text node new-code)
  "Replace NODE with NEW-CODE, preserving original indentation.
   The replacement's first line lands exactly where the original started;
   continuation lines are shifted by the original form's base indent so
   relative structure survives. Splicing at NODE-START (after the original
   leading whitespace) plus first-line-shift would double-indent."
  (let* ((start (node-start node))
         (end (node-end node))
         (indent (detect-form-indentation text node))
         (indented-code (indent-continuation-lines new-code indent)))
    (concatenate 'string
                 (subseq text 0 start)
                 indented-code
                 (subseq text end))))

;;; ============================================================
;;; Batch Operations
;;; ============================================================

(defun split-jammed-top-level (text &key recovery)
  "Insert a newline between any two adjacent top-level forms that share
   a line. Purely additive whitespace repair — no reindentation, so the
   diff stays minimal even on files where full `format' would rewrite
   everything."
  (let* ((ast (parse-for-edit text recovery))
         (forms (list-top-level ast))
         (positions '()))
    (loop for (a b) on forms
          while b
          do (let ((gap (subseq text (node-end a) (node-start b))))
               (unless (find #\Newline gap)
                 (push (node-start b) positions))))
    (let ((result text))
      ;; splice from the back so earlier offsets stay valid
      (dolist (pos (sort positions #'>))
        (setf result (concatenate 'string
                                  (subseq result 0 pos)
                                  (string #\Newline)
                                  (subseq result pos))))
      result)))

(defun node-source-text (text node)
  "Return the exact source substring TEXT that NODE spans."
  (subseq text (node-start node) (node-end node)))

(defun source-of-top-level (text &key name index end recovery)
  "Return the exact source text of one top-level form in TEXT.
   Target it by NAME, INDEX, or the last form with END."
  (let ((node (cond
                (end
                 (first (last (list-top-level (parse-for-edit text recovery)))))
                (name
                 (find-top-level-by-name text name :recovery recovery))
                (index
                 (top-level-node-at text index :recovery recovery)))))
    (when node
      (node-source-text text node))))

(defun node-span (n)
  "Length of NODE source span, or most-positive-fixnum when unbounded."
  (if (and (node-start n) (node-end n))
      (- (node-end n) (node-start n))
      most-positive-fixnum))

(defun smallest-node (nodes)
  "Node with smallest span in NODES, or NIL."
  (let ((best nil) (best-len most-positive-fixnum))
    (dolist (n nodes best)
      (let ((len (node-span n)))
        (when (< len best-len)
          (setf best n best-len len))))))

(defun find-subform-matching (node text snippet)
  "Find the smallest form rooted at NODE whose source matches SNIPPET.
   NODE itself counts (whole-host match); otherwise smallest descendant.
   Exact trimmed match is preferred over a contains-match; among equals
   the smallest span wins. Returns NIL when nothing matches."
  (let ((exact nil)
        (contains nil))
    (labels ((consider (n)
               (when (and (node-start n) (node-end n))
                 (let* ((raw (node-source-text text n))
                        (trimmed (string-trim (list #\Space #\Tab #\Newline #\Return) raw)))
                   (cond
                     ((string= trimmed snippet) (push n exact))
                     ((search snippet raw) (push n contains))))))
             (collect (n)
               (consider n)
               (dolist (child (node-children n))
                 (collect child))))
      (collect node)
      (cond
        (exact (smallest-node exact))
        (contains (smallest-node contains))
        (t nil)))))

(defun node-at-path (text node path)
  "Follow a slash-separated child-index PATH (e.g. \"3/0/1\") from NODE.
   Returns the deepest node, or NIL when any step is out of range
   (including malformed/non-numeric segments — never signals)."
  (declare (ignore text))
  (when (or (null node) (null path) (= (length path) 0))
    (return-from node-at-path nil))
  (let ((current node))
    (dolist (seg (split-string-on-char path #\/) current)
      (let ((step (ignore-errors (parse-integer seg))))
        (when (or (null step) (< step 0))
          (return-from node-at-path nil))
        (let ((kids (and current (node-children current))))
          (unless (and kids (< step (length kids)))
            (return-from node-at-path nil))
          (setf current (nth step kids)))))))

(defun split-string-on-char (string sep-char)
  "Split STRING on SEP-CHAR, keeping empty segments."
  (let ((parts nil) (start 0))
    (loop for pos = (position sep-char string :start start)
          do (push (subseq string start (or pos (length string))) parts)
             (if pos (setf start (1+ pos)) (loop-finish))
          finally (return (nreverse parts)))))
(defun count-text-occurrences (text snippet)
  "Return (values count first-offset) of literal SNIPPET occurrences in TEXT."
  (let ((count 0) (first-offset nil) (pos 0))
    (loop while (and snippet (> (length snippet) 0))
          for found = (search snippet text :start2 pos)
          while found
          do (incf count)
             (unless first-offset (setf first-offset found))
             (setf pos (+ found (max 1 (length snippet)))))
    (values count (or first-offset -1))))

(defun find-subform-matching-exact (node text snippet)
  "Smallest form rooted at NODE whose trimmed source equals SNIPPET.
   Like FIND-SUBFORM-MATCHING but never falls back to contains-matches —
   anchor verification must not escalate silently. NODE itself counts."
  (let ((snippet (string-trim '(#\Space #\Tab #\Newline #\Return) snippet))
        (exact nil))
    (labels ((collect (n)
               (when (and (node-start n) (node-end n)
                          (string= (string-trim '(#\Space #\Tab #\Newline #\Return)
                                                (node-source-text text n))
                                   snippet))
                 (push n exact))
               (dolist (child (node-children n))
                 (collect child))))
      (collect node)
      (when exact
        (first (sort exact #'< :key #'(lambda (n) (- (node-end n) (node-start n)))))))))

(defun net-depth-delta (old-text new-text)
  "Net paren-depth change of replacing OLD-TEXT with NEW-TEXT, using the
   reader-aware balance scanner (char literals, strings, comments count
   correctly). Zero means the replacement preserves scope structure."
  (- (getf (analyze-balance new-text) :final-depth)
     (getf (analyze-balance old-text) :final-depth)))

(defun duplicate-top-level-nodes (text &key recovery)
  "Group top-level NODES by byte-identical source. Returns a list of
   node groups, longest-first occurrence order preserved within groups.
   The stray-(in-package x4) smell is the motivating case."
  (let* ((ast (parse-for-edit text recovery))
         (by-source (make-hash-table :test #'equal)))
    (dolist (node (list-top-level ast))
      (push node (gethash (node-source-text text node) by-source nil)))
    (loop for nodes being the hash-values of by-source
          when (> (length nodes) 1)
            collect (sort (copy-list nodes) #'< :key #'node-start))))

(defun duplicate-top-level-forms (text &key recovery)
  "Return groups ((offset1 offset2 ...) ...) of top-level forms with
   byte-identical source — the stray-(in-package x4) smell."
  (mapcar (lambda (nodes) (mapcar #'node-start nodes))
          (duplicate-top-level-nodes text :recovery recovery)))

;;; --- Lint diagnostics ---
;;;
;;; A lint diagnostic is a plist with stable keys so editors, CI, and
;;; agents can consume findings without scraping human text:
;;; (:rule STRING :severity KEYWORD :line INT-OR-NIL :col INT-OR-NIL
;;;  :start OFFSET-OR-NIL :end OFFSET-OR-NIL :message STRING :fix STRING-OR-NIL).
;;; Severities, weakest-last: :error breaks the build, :warning and
;;; :portability deserve attention, :style and :info are advisory.

(defparameter *lint-severities* '(:error :warning :portability :style :info)
  "Lint severities in increasing-advisory order.")

(defun lint-severity-rank (severity)
  "Numeric rank of SEVERITY for deterministic diagnostic ordering."
  (or (position severity *lint-severities*) (length *lint-severities*)))

(defun make-lint-diagnostic (&key rule severity line col start end message fix)
  "Build one lint diagnostic plist. RULE is a stable string id,
   SEVERITY a keyword, MESSAGE a string, FIX an optional suggestion."
  (unless (stringp rule)
    (error "Lint rule id must be a string, not ~s" rule))
  (unless (member severity *lint-severities*)
    (error "Unknown lint severity ~s" severity))
  (unless (stringp message)
    (error "Lint message must be a string, not ~s" message))
  (list :rule rule :severity severity :line line :col col
        :start start :end end :message message :fix fix))

(defun lint-diagnostic< (a b)
  "Deterministic diagnostic order: offset, span end, severity, rule, message."
  (let ((sa (or (getf a :start) most-positive-fixnum))
        (sb (or (getf b :start) most-positive-fixnum)))
    (cond ((< sa sb) t)
          ((> sa sb) nil)
          ((< (or (getf a :end) most-positive-fixnum)
              (or (getf b :end) most-positive-fixnum)) t)
          ((> (or (getf a :end) most-positive-fixnum)
              (or (getf b :end) most-positive-fixnum)) nil)
          ((< (lint-severity-rank (getf a :severity))
              (lint-severity-rank (getf b :severity))) t)
          ((> (lint-severity-rank (getf a :severity))
              (lint-severity-rank (getf b :severity))) nil)
          ((string< (getf a :rule) (getf b :rule)) t)
          ((string> (getf a :rule) (getf b :rule)) nil)
          (t (string< (getf a :message) (getf b :message))))))

(defun sort-lint-diagnostics (diagnostics)
  "Return DIAGNOSTICS in deterministic order."
  (sort (copy-list diagnostics) #'lint-diagnostic<))

(defvar *lint-rules* nil
  "Registered lint rules as ((id function doc) ...) in registration order.")

(defun register-lint-rule (id function &key doc)
  "Register a lint rule. ID is a stable string, FUNCTION takes
   (text ast) and returns diagnostics. Re-registering ID replaces it."
  (unless (stringp id)
    (error "Lint rule id must be a string, not ~s" id))
  (unless (functionp function)
    (error "Lint rule ~s must be a function, not ~s" id function))
  (setf *lint-rules*
        (append (remove id *lint-rules* :key #'first :test #'string=)
                (list (list id function (or doc "")))))
  id)

(defun lint-rule-ids ()
  "Stable lint rule ids in registration order."
  (mapcar #'first *lint-rules*))

(defun resolve-lint-rules (rules)
  "Resolve RULES (NIL = all registered) to rule ids, signaling on unknown ids."
  (let ((ids (lint-rule-ids)))
    (cond ((null rules) ids)
          (t (dolist (id rules rules)
               (unless (member id ids :test #'string=)
                 (error "Unknown lint rule ~s (known: ~{~s~^, ~})" id ids)))))))

(defun diagnostic-for-node (text node &key rule severity message fix)
  "Build a diagnostic for NODE's span, deriving line/col from TEXT."
  (let ((start (node-start node))
        (end (node-end node)))
    (multiple-value-bind (line col)
        (if start
            (cl-toolkit-ast:offset-to-line-col text start)
            (values nil nil))
      (make-lint-diagnostic :rule rule :severity severity
                            :line line :col col
                            :start start :end end
                            :message message :fix fix))))

(defun lint-error-position (message)
  "Offset after \"Position \" in a parse error MESSAGE, or NIL."
  (let ((idx (search "Position " message)))
    (when idx
      (parse-integer message :start (+ idx (length "Position "))
                     :junk-allowed t))))

(defun lint-syntax-diagnostic (text message start end)
  "One :error diagnostic for a failed parse, positioned precisely when
   the message carries an offset."
  (let* ((pos (lint-error-position message))
         (at (if (and pos (<= 0 pos) (<= pos (length text))) pos start)))
    (multiple-value-bind (line col)
        (if at
            (cl-toolkit-ast:offset-to-line-col text at)
            (values nil nil))
      (make-lint-diagnostic :rule "syntax-error" :severity :error
                            :line line :col col :start at :end end
                            :message message
                            :fix "Fix the syntax error; no other lint findings are reported until the file parses."))))

(defun lint-source (text &key rules recovery)
  "Run lint RULES (NIL = all registered) over TEXT. Returns
   (:ok BOOL :diagnostics LIST). A file that does not parse yields one
   syntax-error diagnostic and no rule findings."
  (let* ((ast (parse-for-edit text recovery))
         (ids (resolve-lint-rules rules)))
    (if (eq (node-type ast) :error)
        (list :ok nil
              :diagnostics (list (lint-syntax-diagnostic
                                  text (node-value ast)
                                  (node-start ast) (node-end ast))))
        (let ((diagnostics nil))
          (dolist (id ids)
            (let ((fn (second (assoc id *lint-rules* :test #'string=))))
              (setf diagnostics
                    (append diagnostics (funcall fn text ast)))))
          (let ((sorted (sort-lint-diagnostics diagnostics)))
            (list :ok (null sorted) :diagnostics sorted))))))

(defun lint-value-json (value)
  "Render one JSON scalar: strings escaped, NIL as null, else princ."
  (cond ((null value) "null")
        ((stringp value)
         (format nil "\"~a\"" (cl-toolkit-ast:escape-json-string value)))
        ((keywordp value)
         (format nil "\"~a\"" (string-downcase (symbol-name value))))
        (t (format nil "~a" value))))

(defun lint-diagnostic-json (diagnostic)
  "Render one lint diagnostic plist as a JSON object with stable keys."
  (format nil "{\"rule\":~a,\"severity\":~a,\"line\":~a,\"col\":~a,\"start\":~a,\"end\":~a,\"message\":~a,\"fix\":~a}"
          (lint-value-json (getf diagnostic :rule))
          (lint-value-json (getf diagnostic :severity))
          (lint-value-json (getf diagnostic :line))
          (lint-value-json (getf diagnostic :col))
          (lint-value-json (getf diagnostic :start))
          (lint-value-json (getf diagnostic :end))
          (lint-value-json (getf diagnostic :message))
          (lint-value-json (getf diagnostic :fix))))

(defun lint-diagnostics-json (result)
  "Render a LINT-SOURCE result plist as stable machine JSON."
  (format nil "{\"ok\":~a,\"diagnostics\":[~{~a~^,~}]}"
          (if (getf result :ok) "true" "false")
          (mapcar #'lint-diagnostic-json (getf result :diagnostics))))

(defun lint-duplicate-top-level-forms (text ast)
  "Diagnostics for byte-identical top-level forms. The first occurrence
   is the keeper; each later copy gets one :warning."
  (let ((by-source (make-hash-table :test #'equal))
        (diagnostics nil))
    (dolist (node (list-top-level ast))
      (push node (gethash (node-source-text text node) by-source nil)))
    (maphash (lambda (source nodes)
               (declare (ignore source))
               (let ((ordered (sort (copy-list nodes) #'< :key #'node-start)))
                 (when (> (length ordered) 1)
                   (let ((keeper (first ordered)))
                     (multiple-value-bind (line col)
                         (cl-toolkit-ast:offset-to-line-col
                          text (node-start keeper))
                       (dolist (node (rest ordered))
                         (push (diagnostic-for-node
                                text node
                                :rule "duplicate-top-level"
                                :severity :warning
                                :message (format nil "Duplicate top-level form (first copy at line ~a, col ~a)"
                                                 line col)
                                :fix "Delete this duplicate or keep only one copy.")
                               diagnostics)))))))
             by-source)
    (sort-lint-diagnostics diagnostics)))

(register-lint-rule "duplicate-top-level" #'lint-duplicate-top-level-forms
                    :doc "Byte-identical top-level forms.")

(defun walk-lint-nodes (node function)
  "Call FUNCTION on NODE and every descendant, pre-order."
  (when (nodep node)
    (funcall function node)
    (dolist (child (node-children node))
      (walk-lint-nodes child function))))

(defun lint-sharp-underscore-dispatch (text ast)
  "Flag `#_` tokens. This SBCL build has no dispatch function for `#_`,
   so it signals a reader error; the toolkit reads `#_` as an ordinary
   symbol. That is a portability divergence worth surfacing, not a
   silent acceptance."
  (let ((diagnostics nil))
    (walk-lint-nodes
     ast
     (lambda (node)
       (when (and (eq (node-type node) :symbol)
                    (node-start node) (node-end node))
         (let ((source (ignore-errors (node-source-text text node))))
           (when (and (stringp source)
                      (>= (length source) 2)
                      (char= (char source 0) #\#)
                      (char= (char source 1) #\_))
             (push (diagnostic-for-node
                    text node
                    :rule "sharp-underscore-dispatch"
                    :severity :portability
                    :message "The `#_` dispatch macro has no reader function here; the toolkit reads it as a symbol, which diverges from this SBCL build."
                    :fix "Avoid `#_` in portable sources, or gate the file on a reader that defines it.")
                   diagnostics))))))
    (sort-lint-diagnostics diagnostics)))

(defun lint-skipped-conditional-branch (text ast)
  "Note feature-conditional branches the reader never reads. A skipped
   target is stored as a symbol beginning where the feature ends, so it
   is recognizable without re-reading the branch."
  (let ((diagnostics nil))
    (walk-lint-nodes
     ast
     (lambda (node)
       (when (node-list-p node)
         (let ((children (node-children node)))
           (when (and (= (length children) 3)
                      (nodep (first children))
                      (nodep (second children))
                      (nodep (third children))
                      (eq (node-type (first children)) :symbol)
                      (member (node-name (first children)) '("#+" "#-")
                              :test #'string=)
                      (eq (node-type (third children)) :symbol)
                      (eql (node-start (third children))
                           (node-end (second children))))
             (push (diagnostic-for-node
                    text node
                    :rule "skipped-conditional-branch"
                    :severity :info
                    :message "A feature-conditional branch was skipped without validation (the reader never reads the absent branch)."
                    :fix "If this branch must be checked, lint it under the corresponding feature.")
                   diagnostics))))))
    (sort-lint-diagnostics diagnostics)))

(register-lint-rule "sharp-underscore-dispatch" #'lint-sharp-underscore-dispatch
                    :doc "The `#_` dispatch macro, which this SBCL build rejects.")
(register-lint-rule "skipped-conditional-branch" #'lint-skipped-conditional-branch
                    :doc "Feature-conditional branches skipped without validation.")

(defparameter *lint-defining-heads*
  '("defun" "defvar" "defparameter" "defmacro" "defgeneric" "defclass"
    "defstruct" "deftype" "define-compiler-macro" "defsetf"
    "define-setf-expander" "defpackage" "test")
  "Heads whose second child names the thing being defined. DEFMETHOD is
   deliberately absent: same-name methods with different specializers
   are overloading, not redefinition.")

(defun lint-definition-key (node)
  "If NODE is a defining top-level form, its (head . name) key with
   both parts upcased for case-insensitive comparison. Otherwise NIL."
  (when (node-list-p node)
    (let ((children (node-children node)))
      (when (and (>= (length children) 2)
                 (nodep (first children))
                 (nodep (second children))
                 (eq (node-type (first children)) :symbol)
                 (eq (node-type (second children)) :symbol)
                 (member (node-name (first children))
                         *lint-defining-heads*
                         :test #'string-equal))
        (cons (string-upcase (node-name (first children)))
              (string-upcase (node-name (second children))))))))

(defun lint-redefinition-check (text seen node)
  "Check NODE against the SEEN definition table. Records first-seen
   definitions in SEEN. Returns a diagnostic when NODE redefines a
   (head . name) pair with a different body, else NIL. Byte-identical
   copies return NIL: duplicate-top-level owns those."
  (let ((key (lint-definition-key node)))
    (when key
      (let ((prev (gethash key seen)))
        (cond ((null prev)
               (setf (gethash key seen) node)
               nil)
              ((string= (node-source-text text prev)
                        (node-source-text text node))
               nil)
              (t
               (multiple-value-bind (line col)
                   (cl-toolkit-ast:offset-to-line-col text (node-start prev))
                 (diagnostic-for-node
                  text node
                  :rule "redefined-top-level"
                  :severity :warning
                  :message (format nil "~a is redefined here (first defined at line ~a, col ~a)"
                                   (cdr key) line col)
                  :fix "Remove the stale definition or rename one of them."))))))))

(defun lint-redefined-top-level (text ast)
  "Flag a top-level definition whose (head . name) was already defined
   above with a different body."
  (let ((seen (make-hash-table :test #'equal))
        (diagnostics nil))
    (dolist (node (list-top-level ast))
      (let ((hit (lint-redefinition-check text seen node)))
        (when hit
          (push hit diagnostics))))
    (sort-lint-diagnostics diagnostics)))

(register-lint-rule "redefined-top-level" #'lint-redefined-top-level
                    :doc "Same (head . name) defined twice with different bodies.")
(defun find-forms-containing (text snippet &key recovery)
  "Return a list of (index node) pairs for top-level forms in TEXT
   whose source contains SNIPPET. Empty snippets match everything,
   so they are rejected."
  (when (or (null snippet) (= (length snippet) 0))
    (error "Snippet must not be empty"))
  (let ((ast (parse-for-edit text recovery)))
    (loop for node in (list-top-level ast)
          for i from 0
          when (search snippet (node-source-text text node))
            collect (cons i node))))

(defun top-level-node-at (text index &key recovery)
  "Return the INDEX-th top-level node of TEXT, signaling an error if out of range."
  (let* ((ast (parse-for-edit text recovery))
         (forms (list-top-level ast)))
    (when (or (< index 0) (>= index (length forms)))
      (error "Index ~a out of range (0-~a)" index (1- (length forms))))
    (nth index forms)))

(defun splice-replacement (text node new-code)
  "Replace the region NODE spans in TEXT with NEW-CODE."
  (concatenate 'string
               (subseq text 0 (node-start node))
               new-code
               (subseq text (node-end node))))

(defun top-level-node-by-name (text edit &optional recovery)
  "Resolve the :name of EDIT to a top-level node or signal."
  (or (find-top-level-by-name text (getf edit :name) :recovery recovery)
      (error "No top-level form named '~a'" (getf edit :name))))

(defun edit-replace-name (text edit &optional recovery)
  "Apply a :replace-name EDIT to TEXT."
  (let ((node (top-level-node-by-name text edit recovery)))
    (if (getf edit :pretty)
        (replace-form-pretty text node (getf edit :code))
        (splice-replacement text node (getf edit :code)))))

(defun edit-delete-name (text edit &optional recovery)
  "Apply a :delete-name EDIT to TEXT."
  (delete-node-from-text text (top-level-node-by-name text edit recovery)))

(defun edit-insert-after-name (text edit &optional recovery)
  "Apply an :insert-after-name EDIT to TEXT (newline-separated)."
  (let* ((node (top-level-node-by-name text edit recovery))
         (end (node-end node))
         (code (getf edit :code)))
    (concatenate 'string
                 (subseq text 0 end)
                 (string #\Newline)
                 code
                 (subseq text end))))

(defun edit-replace-index (text edit &optional recovery)
  "Apply a :replace-index EDIT to TEXT."
  (let ((node (top-level-node-at text (getf edit :index) :recovery recovery)))
    (if (getf edit :pretty)
        (replace-form-pretty text node (getf edit :code))
        (splice-replacement text node (getf edit :code)))))

(defun edit-replace-position (text edit &optional recovery)
  "Apply a :replace-position EDIT to TEXT."
  (let* ((ast (parse-for-edit text recovery))
         (node (find-form-at ast text (getf edit :line) (getf edit :col))))
    (unless node
      (error "No form found at line ~a, col ~a" (getf edit :line) (getf edit :col)))
    (if (getf edit :pretty)
        (replace-form-pretty text node (getf edit :code))
        (splice-replacement text node (getf edit :code)))))

(defun edit-delete-index (text edit &optional recovery)
  "Apply a :delete-index EDIT to TEXT."
  (delete-node-from-text text (top-level-node-at text (getf edit :index) :recovery recovery)))

(defun edit-insert-after-index (text edit &optional recovery)
  "Apply an :insert-after-index EDIT to TEXT."
  (let* ((node (top-level-node-at text (getf edit :index) :recovery recovery))
         (end (node-end node))
         (code (getf edit :code)))
    (concatenate 'string
                 (subseq text 0 end)
                 code
                 (subseq text end))))

(defun edit-replace-match (text edit &optional recovery)
  "Apply a :replace-match EDIT to TEXT — unique subform anywhere,
   same ambiguity policy as --match targeting (:first/:occurrence).
   Contains-level matches refuse unless :allow-fuzzy is set."
  (multiple-value-bind (node host fuzzy)
      (find-subform-globally text (getf edit :match)
                             :match-exact (getf edit :match-exact)
                             :first (getf edit :first)
                             :occurrence (getf edit :occurrence)
                             :recovery recovery)
    (declare (ignore host))
    (when (and fuzzy (not (getf edit :allow-fuzzy)))
      (error "Match for ~s is contains-level; refine the snippet or pass :allow-fuzzy"
             (getf edit :match)))
    (splice-replacement text node (getf edit :code))))

(defun edit-delete-match (text edit &optional recovery)
  "Apply a :delete-match EDIT to TEXT — remove the unique matched subform."
  (multiple-value-bind (node host fuzzy)
      (find-subform-globally text (getf edit :match)
                             :match-exact (getf edit :match-exact)
                             :first (getf edit :first)
                             :occurrence (getf edit :occurrence)
                             :recovery recovery)
    (declare (ignore host))
    (when (and fuzzy (not (getf edit :allow-fuzzy)))
      (error "Match for ~s is contains-level; refine the snippet or pass :allow-fuzzy"
             (getf edit :match)))
    (delete-node-from-text text node)))

(defun apply-single-edit (text edit &key recovery)
  "Apply a single EDIT plist to TEXT.
   EDIT is a plist with :operation, :code, and either :name, :match,
   :index, or :line/:col. Returns the modified text."
  (case (getf edit :operation)
    (:replace-name (edit-replace-name text edit recovery))
    (:delete-name (edit-delete-name text edit recovery))
    (:insert-after-name (edit-insert-after-name text edit recovery))
    (:replace-match (edit-replace-match text edit recovery))
    (:delete-match (edit-delete-match text edit recovery))
    (:replace-index (edit-replace-index text edit recovery))
    (:replace-position (edit-replace-position text edit recovery))
    (:delete-index (edit-delete-index text edit recovery))
    (:insert-after-index (edit-insert-after-index text edit recovery))
    (t (error "Unknown operation: ~a" (getf edit :operation)))))

(defun apply-batch-edits (text edits &key recovery)
  "Apply a list of EDIT plists to TEXT.
   Order: name-based edits first (self-describing), then index-based
   edits from highest to lowest (prevents index shifting), then
   position-based edits. Returns the final modified text."
  (let* ((name-edits (remove-if-not (lambda (e) (getf e :name)) edits))
         (index-edits (remove-if (lambda (e) (getf e :name)) edits))
         (with-index (remove-if-not (lambda (e) (getf e :index)) index-edits))
         (position-edits (remove-if (lambda (e) (getf e :index)) index-edits))
         (sorted-index (sort (copy-list with-index)
                             (lambda (a b) (> (getf a :index) (getf b :index))))))
    (reduce (lambda (current-text edit)
              (apply-single-edit current-text edit :recovery recovery))
            (append name-edits sorted-index position-edits)
            :initial-value text)))

(defun insert-form-at (text line col new-code &key recovery)
  "Insert NEW-CODE before the form at LINE, COL (0-indexed) in TEXT.
   If we're inside a symbol, finds the containing list.
   When RECOVERY is T, use error recovery parser.
   Returns the modified source string."
   (let* ((ast (parse-for-edit text recovery))
          (node (find-form-at ast text line col)))
    (if node
        (let ((insert-before (node-start node)))
          ;; If we're at a symbol, check if there's a containing list at the same line
          ;; that starts at an earlier column (the opening paren)
          (when (not (node-list-p node))
            ;; We're inside a symbol, find the parent list
            (labels ((find-parent (n target)
                       (when (and (node-children n) (not (eq n target)))
                         (dolist (child (node-children n))
                           (when (eq child target)
                             (return n))
                           (let ((result (find-parent child target)))
                             (when result (return result)))))))
              (let ((parent (find-parent ast node)))
                (when (and parent
                           (node-list-p parent)
                           (< (node-start parent) insert-before)
                           (<= insert-before (node-end parent)))
                  (setf insert-before (node-start parent))))))
          ;; Insert before the form
          (concatenate 'string
                       (subseq text 0 insert-before)
                       new-code
                       (subseq text insert-before)))
        ;; No form found — insert at end
        (concatenate 'string text new-code))))

(defun append-form-at (text line col new-code &key recovery)
  "Append NEW-CODE after the form at LINE, COL (0-indexed) in TEXT.
   When RECOVERY is T, use error recovery parser.
   Returns the modified source string."
   (let* ((ast (parse-for-edit text recovery))
          (node (find-form-at ast text line col)))
    (if node
        (let* ((node-end (node-end node)))
          ;; Insert after the form
          (concatenate 'string
                       (subseq text 0 node-end)
                       new-code
                       (subseq text node-end)))
        ;; No form found — append to end
        (concatenate 'string text new-code))))

(defun insert-form-end (text new-code &key validate)
  "Insert NEW-CODE at the end of TEXT.
   When VALIDATE is T, check that NEW-CODE parses as valid Lisp.
   Returns the modified source string."
  (when validate
    (let ((ast (cl-toolkit-grammar::parse-lisp-source new-code)))
      (when (eq (node-type ast) :error)
        (error "Invalid Lisp syntax in new code: ~a" (node-value ast)))))
  (let ((code (if (and (> (length new-code) 0)
                       (char= (char new-code (1- (length new-code))) #\Newline))
                  new-code
                  (concatenate 'string new-code (string #\Newline)))))
    ;; empty (or whitespace-only) host: don't lead with a blank line
    (if (= (length (string-trim '(#\Space #\Tab #\Newline #\Return #\Page) text)) 0)
        code
        (concatenate 'string
                     ;; right-trim only: leading blank lines/comments are
                     ;; part of the file and must survive appends.
                     (string-right-trim '(#\Space #\Tab #\Newline #\Return #\Page) text)
                     (string #\Newline)
                     code))))

(defun replace-form-at (text line col new-code &key recovery)
  "Replace the form at LINE, COL (0-indexed) with NEW-CODE in TEXT.
   When RECOVERY is T, use error recovery parser.
   Returns the modified source string."
   (let* ((ast (parse-for-edit text recovery))
          (node (find-form-at ast text line col)))
    (unless node
      (error "No form found at line ~a, col ~a" line col))
    (let ((start (node-start node))
          (end (node-end node)))
      (concatenate 'string
                   (subseq text 0 start)
                   new-code
                   (subseq text end)))))

(defun move-find-nodes (text from-line from-col to-line to-col recovery)
  "Find source and destination nodes for move operation."
  (let* ((ast (parse-for-edit text recovery))
         (from-node (find-form-at ast text from-line from-col))
         (to-node (find-form-at ast text to-line to-col)))
    (unless from-node
      (error "No form found at line ~a, col ~a" from-line from-col))
    (unless to-node
      (error "No form found at line ~a, col ~a" to-line to-col))
    ;; If we found a symbol, get its parent list
    (labels ((find-parent (n target)
               (when (and (node-children n) (not (eq n target)))
                 (dolist (child (node-children n))
                   (when (eq child target) (return n))
                   (let ((result (find-parent child target)))
                     (when result (return result)))))))
      (when (not (node-list-p from-node))
        (let ((parent (find-parent ast from-node)))
          (when parent (setf from-node parent))))
      (when (not (node-list-p to-node))
        (let ((parent (find-parent ast to-node)))
           (when parent (setf to-node parent))))
      ;; Promote to-node to be a sibling of from-node.
      ;; If the user's destination coordinates point inside a nested expression
      ;; (e.g., inside setf or error clause), walk up to the form that is
      ;; actually a sibling of the source.
      (let ((from-parent (find-parent ast from-node)))
        (when from-parent
          (let ((to-parent (find-parent ast to-node)))
            (unless (eq from-parent to-parent)
              ;; Walk up from to-node to find a child of from-parent
              (labels ((find-child-of-parent (n)
                         (let ((p (find-parent ast n)))
                           (cond
                             ((null p) nil)
                             ((eq p from-parent) n)
                             (t (find-child-of-parent p))))))
                (let ((promoted (find-child-of-parent to-node)))
                  (when promoted
                    (setf to-node promoted)))))))))
    ;; Validate: source and dest must not be ancestors of each other
    (labels ((ancestor-p (ancestor descendant)
               (when (node-children ancestor)
                 (dolist (child (node-children ancestor))
                   (when (eq child descendant) (return t))
                   (when (and (nodep child) (ancestor-p child descendant))
                     (return t))))))
      (when (ancestor-p from-node to-node)
        (error "Cannot move a form into itself (destination is inside source)"))
      (when (ancestor-p to-node from-node)
        (error "Cannot move a form into itself (source is inside destination)")))
    (values from-node to-node)))

(defun move-compute-regions (text from-node to-node)
  "Compute deletion and insertion regions for move operation."
  (let* ((from-start (node-start from-node))
         (from-end (node-end from-node))
         (to-start (node-start to-node))
         (to-end (node-end to-node))
         (form-text (subseq text from-start from-end))
         (del-end (skip-whitespace-and-newlines text from-end))
         (insert-at (if (< from-end to-start)
                        ;; Source is before destination: insert after destination
                        to-end
                        ;; Source is after destination: insert before destination
                        to-start)))
    (values form-text del-end insert-at)))

(defun preceding-line-start (text pos)
  "Return the offset of the first character of the line containing POS.
   Walks back to just after the preceding newline (or start of text)."
  (let ((i pos))
    (loop while (and (> i 0)
                     (char/= (char text (1- i)) #\Newline))
          do (decf i))
    i))

(defun count-leading-spaces (text pos)
  "Count leading spaces on the line containing POS."
  (let ((line-start (preceding-line-start text pos))
        (i 0))
    (loop while (and (< (+ line-start i) (length text))
                     (char= (char text (+ line-start i)) #\Space))
          do (incf i))
    i))

(defun count-leading-newlines (text pos)
  "Count consecutive newlines starting at POS in TEXT."
  (let ((count 0))
    (loop for i from pos below (length text)
          while (char= (char text i) #\Newline)
          do (incf count))
    count))

(defun move-form (text from-line from-col to-line to-col &key recovery)
  "Move the form at (FROM-LINE, FROM-COL) to after (TO-LINE, TO-COL).
    When RECOVERY is T, use error recovery parser.
    Returns the modified source string."
  (multiple-value-bind (from-node to-node)
      (move-find-nodes text from-line from-col to-line to-col recovery)
    ;; No-op if same node
    (when (eq from-node to-node)
      (return-from move-form text))
    (let* ((from-start (node-start from-node))
           (from-end (node-end from-node))
           (to-start (node-start to-node))
           (to-end (node-end to-node))
           (form-text (subseq text from-start from-end)))
      ;; Deletion region: include preceding newline (no blank left behind);
      ;; when deleting at offset 0 there is no preceding newline, so instead
      ;; consume one trailing newline to avoid a leading blank line.
      (let* ((at-bob (zerop from-start))
             (del-start (if at-bob 0 (1- (preceding-line-start text from-start))))
             (del-end (if at-bob
                          (skip-whitespace-and-newlines text from-end)
                          from-end))
             (deleted-len (- del-end del-start))
             (deleted-text-str (concatenate 'string
                                            (subseq text 0 del-start)
                                            (subseq text del-end)))
             ;; Destination in post-deletion coordinates (offset arithmetic,
             ;; not text search — duplicates must resolve to the right instance).
             (dest-new-start (if (<= from-end to-start)
                                 (- to-start deleted-len)
                                 to-start))
             (dest-new-end (if (<= from-end to-start)
                               (- to-end deleted-len)
                               to-end)))
        (when (or (< dest-new-start 0) (> dest-new-end (length deleted-text-str)))
          (error "Destination form not found after deletion"))
        ;; Insert after the destination form, indented like it is,
        ;; with at least one newline of separation.
        (let* ((dest-end dest-new-end)
               (indent (count-leading-spaces deleted-text-str dest-new-start))
               (indented-form (concatenate 'string
                                           (make-string indent :initial-element #\Space)
                                           form-text))
               (spacing-nls (max 1 (count-leading-newlines deleted-text-str dest-end))))
          (concatenate 'string
                       (subseq deleted-text-str 0 dest-end)
                       (make-string spacing-nls :initial-element #\Newline)
                       indented-form
                       (subseq deleted-text-str dest-end)))))))
(defun form-body-broken-p (text node)
  "Heuristic: T when the form looks structurally unformatted —
   either a continuation line starts at column 0 with an opening paren
   or atom, or the whole form sits on one line with deep nesting that
   formatting would expand."
  (let* ((src (node-source-text text node))
         (lines (split-string-on-newlines src)))
    (if (rest lines)
        (loop for line in (rest lines)
              thereis (and (> (length (string-trim '(#\Space #\Tab) line)) 0)
                           (let ((ch (char line 0)))
                             (or (char= ch #\()
                                 (alphanumericp ch)))))
        ;; single line: >= 3 open parens means deep inline nesting
        (>= (count #\( src) 3))))

(defun split-string-on-newlines (string)
  "Split STRING on #\Newline, keeping empty segments."
  (let ((parts nil)
        (start 0))
    (loop for pos = (position #\Newline string :start start)
          do (push (subseq string start (or pos (length string))) parts)
             (if pos
                 (setf start (1+ pos))
                 (loop-finish))
          finally (return (nreverse parts)))))

(defun format-minimal (text &key recovery)
  "Minimal structural repair: split jammed top-level forms, then
   reformat only those multi-line top-level forms whose continuation
   lines are unindented. Existing valid indentation is left untouched.
   NOTE: fully-single-line nested forms are flagged by
   FORM-BODY-BROKEN-P but left as-is — FORMAT-SOURCE reindents at
   existing newlines only; expanding them needs a real pretty-printer."
  (let* ((split (split-jammed-top-level text :recovery recovery))
         (ast (parse-for-edit split recovery))
         (result split))
    ;; splice from the back so earlier offsets stay valid
    (dolist (node (reverse (list-top-level ast)) result)
      (when (form-body-broken-p split node)
        (setf result
              (concatenate 'string
                           (subseq result 0 (node-start node))
                           (format-source (node-source-text split node))
                           (subseq result (node-end node))))))))

;;; ============================================================
;;; Balance Analysis (0-based lines/cols, reader-aware)
;;; ============================================================

(defun balance-record-line (line depth line-start-depth lines)
  "Record line info and return updated state."
  (push (list :line line :depth depth
              :delta (- depth line-start-depth))
        lines))

(defun balance-check-close (ch line col depth errors kind)
  "Check for unexpected closing delimiter and record error if needed.
   Returns (values new-depth new-errors)."
  (declare (ignore ch))
  (if (zerop depth)
      (values depth
              (push (list :line line :col col
                          :message (format nil "Unexpected closing ~a (depth already 0)" kind))
                    errors))
      (values (1- depth) errors)))

(defun balance-process-string (ch i text line col)
  "Process character inside string.
   Returns (values ended-p new-i new-line new-col).
   new-i is the index of the last consumed char (loop auto-increments).
   Backslash escapes the next char (including an escaped quote)."
  (cond
    ;; escape: consume both chars (unless at EOF)
    ((and (char= ch #\\)
          (< (1+ i) (length text)))
     (let ((nxt (char text (1+ i))))
       (if (char= nxt #\Newline)
           (values nil (1+ i) (1+ line) 0)
           (values nil (1+ i) line (+ col 2)))))
    ((char= ch #\Newline)
     (values nil i (1+ line) 0))
    ((char= ch #\")
     (values t i line (+ col 1)))
    (t
     (values nil i line (+ col 1)))))

(defun balance-process-hash (i text col)
  "Process # dispatch character. Returns (values new-i new-col in-block-comment).
   new-i is the value to SET i to (loop auto-increments)."
  (let ((next-i (1+ i)))
    (cond
      ;; Block comment #|
      ((and (< next-i (length text))
            (char= (char text next-i) #\|))
       (values next-i (+ col 2) t))
      ;; Character literal #\
      ((and (< next-i (length text))
            (char= (char text next-i) #\\))
       (let ((ci (+ i 2)) (cc (+ col 2)))
         (cond
           ((and (< ci (length text))
                 (alphanumericp (char text ci)))
            (loop while (and (< ci (length text))
                             (alphanumericp (char text ci)))
                  do (incf ci) (incf cc)))
           ((< ci (length text))
            (incf ci) (incf cc)))
         (values (1- ci) cc nil)))
      ;; Vector #( — let ( be processed normally so it balances with )
      ((and (< next-i (length text))
            (char= (char text next-i) #\())
       (values i (+ col 1) nil))
      ;; Generic #X: consume ONLY the # — the next char may be structural
      ;; (the #1# idiom ends in # followed by ); swallowing it undercounts
      ;; depth (alexandria tests.lisp + babel false-unbalanced field cases).
      (t (values i (+ col 1) nil)))))

(defun balance-process-normal (ch i text depth max-depth line col errors)
  "Process character in normal code mode. Returns updated state.
   Backslash escapes the next char (symbol constituent \\X, e.g. \\a):
   it is consumed as a unit so an escaped structural char (\\)) never
   touches depth. Bar opens a |...| symbol (handled by :bar mode)."
  (case ch
    (#\; (values i col depth max-depth errors :line-comment))
    (#\# (multiple-value-bind (ni nc in-block) (balance-process-hash i text col)
           (values ni nc depth max-depth errors (if in-block :block-comment :normal))))
    (#\" (values i (+ col 1) depth max-depth errors :string))
    (#\| (values i (+ col 1) depth max-depth errors :bar))
    (#\\ (if (< (1+ i) (length text))
             (if (char= (char text (1+ i)) #\Newline)
                 (values (1+ i) 0 depth max-depth errors :newline-escaped)
                 (values (1+ i) (+ col 2) depth max-depth errors :normal))
             (values i (+ col 1) depth max-depth errors :normal)))
    (#\( (let ((nd (1+ depth)))
           (values i (+ col 1) nd (max max-depth nd) errors :normal)))
    (#\) (multiple-value-bind (new-depth new-errors)
             (balance-check-close ch line col depth errors "paren")
           (values i (+ col 1) new-depth max-depth new-errors :normal)))
    (#\Newline (values i 0 depth max-depth errors :newline))
    (t (values i (1+ col) depth max-depth errors :normal))))

(defun balance-process-bar (ch i text line col)
  "Process character inside a |...| symbol. Escapes consume two chars;
   the closing bar ends the symbol. Returns (values ended-p new-i
   new-line new-col)."
  (cond
    ((and (char= ch #\\)
          (< (1+ i) (length text)))
     (let ((nxt (char text (1+ i))))
       (if (char= nxt #\Newline)
           (values nil (1+ i) (1+ line) 0)
           (values nil (1+ i) line (+ col 2)))))
    ((char= ch #\Newline)
     (values nil i (1+ line) 0))
    ((char= ch #\|)
     (values t i line (+ col 1)))
    (t
     (values nil i line (+ col 1)))))

(defun balance-dispatch-line-comment (ch i text line col depth line-start-depth lines mode)
  "Dispatch line comment mode. Newline ends it (0-based)."
  (declare (ignore text))
  (if (char= ch #\Newline)
      (progn
        (setf lines (balance-record-line line depth line-start-depth lines))
        (values i (1+ line) 0 depth depth lines :normal))
      (values i line (+ col 1) depth line-start-depth lines mode)))

(defun balance-dispatch-block-comment (ch i text line col depth line-start-depth lines mode block-depth)
  "Dispatch block comment mode with nesting. Returns updated state including BLOCK-DEPTH."
  (cond
    ((and (char= ch #\#)
          (< (1+ i) (length text))
          (char= (char text (1+ i)) #\|))
     ;; nested opener
     (values (1+ i) line (+ col 2) depth line-start-depth lines mode (1+ block-depth)))
    ((and (char= ch #\|)
          (< (1+ i) (length text))
          (char= (char text (1+ i)) #\#))
     (let ((nd (1- block-depth)))
       (if (<= nd 0)
           (values (1+ i) line (+ col 2) depth line-start-depth lines :normal 0)
           (values (1+ i) line (+ col 2) depth line-start-depth lines mode nd))))
    ((char= ch #\Newline)
     (setf lines (balance-record-line line depth line-start-depth lines))
     (values i (1+ line) 0 depth depth lines mode block-depth))
    (t
     (values i line (+ col 1) depth line-start-depth lines mode block-depth))))

(defun balance-dispatch-string (ch i text line col depth line-start-depth lines mode)
  "Dispatch string mode with correct escape handling."
  (multiple-value-bind (ended ni nl nc)
      (balance-process-string ch i text line col)
    (cond
      (ended
       (values ni nl nc depth line-start-depth lines :normal))
      ((and (= nl (1+ line)) (= nc 0))
       ;; newline (or escaped newline) inside string ends visual line
       (setf lines (balance-record-line line depth line-start-depth lines))
       (values ni nl nc depth depth lines mode))
      (t
       (values ni nl nc depth line-start-depth lines mode)))))

(defun balance-dispatch-bar (ch i text line col depth line-start-depth lines mode)
  "Dispatch |...| symbol mode. Newlines inside end the visual line
   (depth is untouched — bars are opaque to structure)."
  (multiple-value-bind (ended ni nl nc)
      (balance-process-bar ch i text line col)
    (cond
      (ended
       (values ni nl nc depth line-start-depth lines :normal))
      ((and (= nl (1+ line)) (= nc 0))
       (setf lines (balance-record-line line depth line-start-depth lines))
       (values ni nl nc depth depth lines mode))
      (t
       (values ni nl nc depth line-start-depth lines mode)))))

(defun scan-comma-errors (text)
  "Report commas that are not lexically inside a backquote.
   SBCL (and the standard) treats a comma outside a backquote as a
   reader error, so \"(a ,b)\" is invalid even though it looks like an
   ordinary list. A backquote has no closing delimiter: its extent
   runs to the end of the enclosing form, which is what the stack
   models — a new bracket level INHERITS the enclosing flag, and
   closing a bracket pops back to the outer one.
   Returns a list of (:line :col :message) plists, in source order."
  (let ((i 0) (len (length text))
        (line 0) (col 0)
        (flags '())            ; one entry per open bracket: backquote active?
        (block-depth 0)
        (mode :normal)
        (errors '()))
    (loop while (< i len) do
      (let ((ch (char text i))
            (nxt (and (< (1+ i) len) (char text (1+ i)))))
        (case mode
          (:line-comment
           (when (char= ch #\Newline)
             (setf mode :normal)
             (incf line)
             (setf col 0))
           (incf col))
          (:string
           (cond ((char= ch #\\) (incf i) (incf col))
                 ((char= ch #\")
                  (setf mode :normal)
                  (incf col))
                 ((char= ch #\Newline) (incf line) (setf col 0))
                 (t (incf col))))
          (:bar
           (cond ((char= ch #\\) (incf i) (incf col))
                 ((char= ch #\|) (setf mode :normal) (incf col))
                 ((char= ch #\Newline) (incf line) (setf col 0))
                 (t (incf col))))
          (otherwise
           (cond
             ;; Inside #| ... |# everything is literal text, commas
             ;; included: only a nested block comment matters.
             ((plusp block-depth)
              (cond ((and nxt (char= ch #\#) (char= nxt #\|))
                     (incf block-depth) (incf i) (incf col 2))
                    ((and nxt (char= ch #\|) (char= nxt #\#))
                     (decf block-depth) (incf i) (incf col 2))))
             ((char= ch #\;) (setf mode :line-comment) (incf col))
             ((char= ch #\") (setf mode :string) (incf col))
             ((char= ch #\\) (incf i) (incf col))
             ((and nxt (char= ch #\#) (char= nxt #\|))
              (incf block-depth) (incf i) (incf col 2))
             ((char= ch #\|) (setf mode :bar) (incf col))
             ;; a backquote opens a scope for the rest of the form
             ((char= ch #\`) (setf flags (cons t flags)) (incf col))
             ;; a new bracket level inherits the enclosing flag
             ;; [ ] { } are constituent characters, not delimiters
             ((char= ch #\()
              (setf flags (cons (car flags) flags))
              (incf col))
             ((char= ch #\))
              (setf flags (cdr flags))
              (incf col))
             ((and (char= ch #\,)
                   (or (null flags) (null (car flags))))
              (push (list :line line :col col
                          :message "Comma not inside a backquote")
                    errors)
              (incf col))
             ((char= ch #\Newline) (incf line) (setf col 0))
             (t (incf col))))))
      (incf i))
    (nreverse errors)))

(defun balance-dispatch-normal (ch i text line col depth max-depth line-start-depth lines errors mode)
  "Dispatch normal mode (no double col increment)."
  (declare (ignore mode))
  (multiple-value-bind (ni nc nd nmax nerrors nmode)
      (balance-process-normal ch i text depth max-depth line col errors)
    (cond
      ((or (eq nmode :newline) (eq nmode :newline-escaped))
       (setf lines (balance-record-line line nd line-start-depth lines))
       (values ni (1+ line) 0 nd nmax nd lines nerrors :normal))
      ((eq nmode :line-comment)
       (values ni line nc nd nmax line-start-depth lines nerrors :line-comment))
      ((eq nmode :block-comment)
       (values ni line nc nd nmax line-start-depth lines nerrors :block-comment))
      ((eq nmode :string)
       (values ni line nc nd nmax line-start-depth lines nerrors :string))
      ((eq nmode :bar)
       (values ni line nc nd nmax line-start-depth lines nerrors :bar))
      (t
       (values ni line nc nd nmax line-start-depth lines nerrors :normal)))))

(defun analyze-balance (text)
  "Analyze parenthesis/bracket balance in TEXT.
   Lines/cols are 0-based to match every other command.
   Returns a plist with:
     :lines - list of plists (:line :depth :delta) per source line
     :max-depth - maximum nesting depth
     :final-depth - depth at end of file (0 = balanced)
     :errors - list of error plists (:line :col :message)"
  (let ((depth 0) (max-depth 0) (line 0) (col 0)
        (line-start-depth 0) (lines nil) (errors nil)
        (mode :normal) (block-depth 0))
    (loop for i from 0 below (length text)
          for ch = (char text i)
          do (case mode
               (:line-comment
                (multiple-value-setq (i line col depth line-start-depth lines mode)
                  (balance-dispatch-line-comment ch i text line col depth line-start-depth lines mode)))
               (:block-comment
                (multiple-value-setq (i line col depth line-start-depth lines mode block-depth)
                  (balance-dispatch-block-comment ch i text line col depth line-start-depth lines mode block-depth)))
               (:string
                (multiple-value-setq (i line col depth line-start-depth lines mode)
                  (balance-dispatch-string ch i text line col depth line-start-depth lines mode)))
               (:bar
                (multiple-value-setq (i line col depth line-start-depth lines mode)
                  (balance-dispatch-bar ch i text line col depth line-start-depth lines mode)))
               (:normal
                (multiple-value-setq (i line col depth max-depth line-start-depth lines errors mode)
                  (balance-dispatch-normal ch i text line col depth max-depth line-start-depth lines errors mode))
                (when (eq mode :block-comment)
                  (setf block-depth 1)))))
    (push (list :line line :depth depth
                :delta (- depth line-start-depth))
          lines)
    (when (/= depth 0)
      (push (list :line line :col col
                  :message (format nil "Unclosed forms: depth ~a at end of file" depth))
            errors))
    (when (eq mode :block-comment)
      (push (list :line line :col col
                  :message "Unclosed block comment #| at end of file")
            errors))
    (when (eq mode :string)
      (push (list :line line :col col
                  :message "Unclosed string at end of file")
            errors))
    ;; A comma outside a backquote is a reader error; the balance walk
    ;; above tracks no lexical backquote state, so scan for it here.
    (dolist (err (scan-comma-errors text))
      (push err errors))
    (when (eq mode :bar)
      (push (list :line line :col col
                  :message "Unclosed |...| symbol at end of file")
            errors))
    (list :lines (nreverse lines)
          :max-depth max-depth
          :final-depth depth
          :errors (nreverse errors))))


;;; ============================================================
;;; Format (Reformat Source)
;;; ============================================================

(defun indent-string (depth &optional (indent-str "  "))
  "Create an indentation string for DEPTH levels."
  (make-string (* depth (length indent-str)) :initial-element #\Space))

(defun format-apply-indent (depth indent result line-pos need-indent)
  "Apply indentation if needed. Returns updated line-pos and need-indent."
  (if need-indent
      (progn
        (write-string (indent-string depth indent) result)
        (values (* depth (length indent)) nil))
      (values line-pos nil)))

(defun format-process-line-comment (ch i text result line-pos)
  "Process character inside line comment."
  (declare (ignore i text))
  (write-char ch result)
  (incf line-pos)
  (if (char= ch #\Newline)
      (values 0 t)
      (values line-pos nil)))

(defun format-process-block-comment (ch i text result line-pos)
  "Process character inside block comment. Returns (values line-pos ended-p new-i)."
  (write-char ch result)
  (incf line-pos)
  (cond
    ((and (char= ch #\|)
          (< (1+ i) (length text))
          (char= (char text (1+ i)) #\#))
     (write-char (char text (1+ i)) result)
     (incf line-pos)
     (values line-pos t (1+ i)))
    ((char= ch #\Newline)
     (values 0 nil i))
    (t (values line-pos nil i))))

(defun format-process-string (ch i text result line-pos)
  "Process character inside string. Returns (values line-pos ended-p new-i).
   Backslash consumes the next char so an escaped quote never ends the string."
  (write-char ch result)
  (incf line-pos)
  (cond
    ((and (char= ch #\\) (< (1+ i) (length text)))
     (write-char (char text (1+ i)) result)
     (incf line-pos)
     (values line-pos nil (1+ i)))
    ((char= ch #\")
     (values line-pos t i))
    (t (values line-pos nil i))))

(defun format-process-bar (ch i text result line-pos)
  "Process character inside a |...| symbol. Returns (values line-pos
   ended-p new-i). Literal span like strings: backslash consumes the next
   char, and no indentation applies inside (positions are verbatim)."
  (write-char ch result)
  (incf line-pos)
  (cond
    ((and (char= ch #\\) (< (1+ i) (length text)))
     (write-char (char text (1+ i)) result)
     (incf line-pos)
     (values line-pos nil (1+ i)))
    ((char= ch #\|)
     (values line-pos t i))
    (t (values line-pos nil i))))

(defun format-process-hash (ch i text depth indent result line-pos need-indent)
  "Process # dispatch character. Returns (values new-i new-line-pos new-need-indent new-mode).
   new-i is the value to SET i to (loop auto-increments)."
  (cond
    ;; Block comment #|
    ((and (< (1+ i) (length text))
          (char= (char text (1+ i)) #\|))
     (multiple-value-bind (lp ni) (format-apply-indent depth indent result line-pos need-indent)
       (declare (ignore ni))
       (write-char ch result) (incf lp)
       (write-char (char text (1+ i)) result) (incf i) (incf lp)
       (values i lp nil :block-comment)))
    ;; Character literal #\
    ((and (< (1+ i) (length text))
          (char= (char text (1+ i)) #\\))
     (multiple-value-bind (lp ni) (format-apply-indent depth indent result line-pos need-indent)
       (declare (ignore ni))
       (write-char ch result) (incf lp)
       (write-char (char text (1+ i)) result) (incf i) (incf lp)
       (incf i)  ; skip past \ to the character name / char itself
       (let ((name-start i))
         (loop while (and (< i (length text))
                          (alphanumericp (char text i)))
               do (write-char (char text i) result) (incf i) (incf lp))
         (when (= i name-start)
           (when (< i (length text))
             (write-char (char text i) result) (incf i) (incf lp))))
       (values (1- i) lp nil :normal)))
    ;; Other # dispatch
    (t
     (multiple-value-bind (lp ni) (format-apply-indent depth indent result line-pos need-indent)
       (declare (ignore ni))
       (write-char ch result) (incf lp)
       (values i lp nil :normal)))))

(defun format-process-open-delimiter (ch depth indent result line-pos need-indent)
  "Process opening delimiter. Returns (values new-line-pos new-need-indent new-depth)."
  (multiple-value-bind (lp ni) (format-apply-indent depth indent result line-pos need-indent)
    (declare (ignore ni))
    (write-char ch result) (incf lp)
    (values lp nil (1+ depth))))

(defun format-process-close-delimiter (ch depth indent result line-pos need-indent)
  "Process closing delimiter. Dedents first when starting a line, then writes.
   Clamps depth to 0 — unmatched close delimiters don't make depth negative."
  (let ((new-depth (max 0 (1- depth))))
    (when need-indent
      ;; closing paren aligns with its opener: indent at new (dedented) depth
      (write-string (indent-string new-depth indent) result)
      (setf line-pos (* new-depth (length indent))))
    (write-char ch result)
    (values (1+ line-pos) nil new-depth)))

(defun format-dispatch-line-comment (ch i text result line-pos mode need-indent)
  "Dispatch line comment mode. Returns updated state."
  (multiple-value-bind (lp ended)
      (format-process-line-comment ch i text result line-pos)
    (setf line-pos lp)
    (when ended (setf mode :normal need-indent t))
    (values line-pos mode need-indent)))

(defun format-dispatch-block-comment (ch i text result line-pos mode need-indent)
  "Dispatch block comment mode. Returns (values line-pos mode need-indent new-i)."
  (multiple-value-bind (lp ended ni)
      (format-process-block-comment ch i text result line-pos)
    (setf line-pos lp i ni)
    (when ended (setf mode :normal))
    (when (char= ch #\Newline)
      (setf line-pos 0 need-indent t))
    (values line-pos mode need-indent i)))

(defun format-dispatch-string (ch i text result line-pos mode)
  "Dispatch string mode. Returns (values line-pos mode new-i)."
  (multiple-value-bind (lp ended ni)
      (format-process-string ch i text result line-pos)
    (setf line-pos lp i ni)
    (when ended (setf mode :normal))
    (values line-pos mode i)))

(defun format-dispatch-space (ch i text depth indent result line-pos need-indent)
  "Dispatch whitespace character. Returns updated state."
  (declare (ignore ch depth indent))
  (unless need-indent
    (write-char #\Space result) (incf line-pos)
    (loop while (and (< (1+ i) (length text))
                     (member (char text (1+ i)) '(#\Space #\Tab)))
          do (incf i)))
  (values line-pos need-indent i))

(defun format-dispatch-hash (ch i text depth indent result line-pos need-indent mode)
  "Dispatch # character. Returns updated state.
   Passes NINDENT through: after emitting #.. we're mid-line, so a
   following space is real (dropping it ate the gap in |# (a))."
  (multiple-value-bind (ni lp nindent nmode)
      (format-process-hash ch i text depth indent result line-pos need-indent)
    (values ni lp nindent (or nmode mode))))

(defun format-dispatch-delimiter (ch depth indent result line-pos need-indent openp)
  "Dispatch delimiter character. OPENP is T for open, NIL for close.
   Returns (values new-line-pos new-need-indent new-depth)."
  (if openp
      (format-process-open-delimiter ch depth indent result line-pos need-indent)
      (format-process-close-delimiter ch depth indent result line-pos need-indent)))

(defun format-dispatch-semicolon (depth indent result line-pos need-indent)
  "Dispatch semicolon: apply indent, write ;, enter line-comment mode.
   Returns (values new-line-pos new-need-indent)."
  (multiple-value-bind (lp ni) (format-apply-indent depth indent result line-pos need-indent)
    (setf line-pos lp need-indent ni)
    (write-char #\; result) (incf line-pos)
    (values line-pos need-indent)))

(defun format-dispatch-quote (depth indent result line-pos need-indent)
  "Dispatch double-quote: apply indent, write \", enter string mode.
   Returns (values new-line-pos new-need-indent)."
  (multiple-value-bind (lp ni) (format-apply-indent depth indent result line-pos need-indent)
    (setf line-pos lp need-indent ni)
    (write-char #\" result) (incf line-pos)
    (values line-pos need-indent)))

(defun format-dispatch-bar (ch i text result line-pos mode)
  "Dispatch |...| symbol mode. Returns (values line-pos mode new-i)."
  (multiple-value-bind (lp ended ni)
      (format-process-bar ch i text result line-pos)
    (setf line-pos lp i ni)
    (when ended (setf mode :normal))
    (values line-pos mode i)))

(defun format-dispatch-backslash (depth indent result line-pos need-indent i text)
  "Dispatch backslash escape in normal code: apply indent, write \\ plus
   the next char, skip both — an escaped delimiter (\\)) never touches
   depth. Returns (values new-i new-line-pos new-need-indent)."
  (multiple-value-bind (lp ni) (format-apply-indent depth indent result line-pos need-indent)
    (setf line-pos lp need-indent ni)
    (write-char #\\ result) (incf line-pos)
    (if (< (1+ i) (length text))
        (progn (write-char (char text (1+ i)) result) (incf line-pos)
               (values (1+ i) line-pos need-indent))
        (values i line-pos need-indent))))

(defun format-dispatch-pipe (depth indent result line-pos need-indent)
  "Dispatch | in normal code: apply indent, write |, enter bar mode.
   Returns (values new-line-pos new-need-indent)."
  (multiple-value-bind (lp ni) (format-apply-indent depth indent result line-pos need-indent)
    (setf line-pos lp need-indent ni)
    (write-char #\| result) (incf line-pos)
    (values line-pos need-indent)))

(defun format-dispatch-normal (ch i text depth indent result line-pos need-indent mode)
  "Dispatch normal mode character. Returns (values i line-pos need-indent mode depth)."
  (case ch
    ((#\Space #\Tab)
     (multiple-value-setq (line-pos need-indent i)
       (format-dispatch-space ch i text depth indent result line-pos need-indent))
     (values i line-pos need-indent mode depth))
    (#\Newline
     (write-char ch result) (setf line-pos 0 need-indent t)
     (values i line-pos need-indent mode depth))
    (#\;
     (multiple-value-setq (line-pos need-indent)
       (format-dispatch-semicolon depth indent result line-pos need-indent))
     (setf mode :line-comment)
     (values i line-pos need-indent mode depth))
    (#\#
     (multiple-value-setq (i line-pos need-indent mode)
       (format-dispatch-hash ch i text depth indent result line-pos need-indent mode))
     (values i line-pos need-indent mode depth))
    (#\"
     (multiple-value-setq (line-pos need-indent)
       (format-dispatch-quote depth indent result line-pos need-indent))
     (setf mode :string)
     (values i line-pos need-indent mode depth))
    (#\|
     (multiple-value-setq (line-pos need-indent)
       (format-dispatch-pipe depth indent result line-pos need-indent))
     (setf mode :bar)
     (values i line-pos need-indent mode depth))
    (#\\
     (multiple-value-setq (i line-pos need-indent)
       (format-dispatch-backslash depth indent result line-pos need-indent i text))
     (values i line-pos need-indent mode depth))
    ((#\()
     (multiple-value-bind (lp ni d)
         (format-dispatch-delimiter ch depth indent result line-pos need-indent t)
       (setf line-pos lp need-indent ni depth d)
       (values i line-pos need-indent mode depth)))
    ((#\))
     (multiple-value-bind (lp ni d)
         (format-dispatch-delimiter ch depth indent result line-pos need-indent nil)
       (setf line-pos lp need-indent ni depth d)
       (values i line-pos need-indent mode depth)))
    (t
     (multiple-value-bind (lp ni) (format-apply-indent depth indent result line-pos need-indent)
       (setf line-pos lp need-indent ni)
       (write-char ch result) (incf line-pos)
       (values i line-pos need-indent mode depth)))))

(defun format-source (text &key (indent "  ") (max-width 80))
  "Reformat Lisp source TEXT with consistent indentation.
   INDENT is the string used for one level of indentation (default two spaces).
   Returns the reformatted source string."
  (declare (ignore max-width))
  (let ((depth 0) (line-pos 0) (need-indent t)
        (mode :normal)
        (result (make-string-output-stream)))
    (loop for i from 0 below (length text)
          for ch = (char text i)
          do (case mode
               (:line-comment
                (multiple-value-setq (line-pos mode need-indent)
                  (format-dispatch-line-comment ch i text result line-pos mode need-indent)))
               (:block-comment
                (multiple-value-setq (line-pos mode need-indent i)
                  (format-dispatch-block-comment ch i text result line-pos mode need-indent)))
               (:string
                (multiple-value-setq (line-pos mode i)
                  (format-dispatch-string ch i text result line-pos mode)))
               (:bar
                (multiple-value-setq (line-pos mode i)
                  (format-dispatch-bar ch i text result line-pos mode)))
                (:normal
                 (multiple-value-setq (i line-pos need-indent mode depth)
                   (format-dispatch-normal ch i text depth indent result line-pos need-indent mode)))))
    (get-output-stream-string result)))

;;; --- Undo/Redo support (simple approach) ---

(defun apply-edit (text edit)
  "Apply an EDIT operation to TEXT.
   EDIT is a plist with :operation and parameters.
   Returns the modified source string."
  (let ((op (getf edit :operation)))
    (case op
      (:delete
       (let ((line (getf edit :line))
             (col (getf edit :col)))
         (delete-form-at text line col)))
      (:delete-index
       (let ((index (getf edit :index)))
         (delete-top-level-at text index)))
      (:insert
       (let ((line (getf edit :line))
             (col (getf edit :col))
             (code (getf edit :code))
             (after (getf edit :after)))
         (declare (ignore after))
         (insert-form-at text line col code)))
      (:insert-end
       (let ((code (getf edit :code)))
         (insert-form-end text code)))
      (:replace
       (let ((line (getf edit :line))
             (col (getf edit :col))
             (code (getf edit :code)))
         (replace-form-at text line col code)))
      (:move
       (let ((from-line (getf edit :from-line))
             (from-col (getf edit :from-col))
             (to-line (getf edit :to-line))
             (to-col (getf edit :to-col)))
         (move-form text from-line from-col to-line to-col)))
      (t (error "Unknown operation: ~a" op)))))
