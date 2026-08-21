(in-package #:cl-toolkit)

;;; ============================================================
;;; High-level parser API
;;; ============================================================

(defun parse-file (path)
  "Parse a Lisp file at PATH. Returns AST root node."
  (let ((text (with-open-file (stream path :direction :input :if-does-not-exist nil)
                (when stream
                  (let ((content (make-string (file-length stream))))
                    (read-sequence content stream)
                    content)))))
    (unless text
      (error "Cannot read file: ~a" path))
    (let ((ast (cl-toolkit-grammar::parse-lisp-source text)))
      (setf (getf ast :source) (namestring path))
      ast)))

(defun parse-string (text)
  "Parse a string of Lisp code. Returns AST root node."
  (cl-toolkit-grammar::parse-lisp-source text))

;;; --- Helper functions (defined before use) ---

(defun skip-whitespace-and-newlines (text offset)
  "Skip whitespace and at most one newline after OFFSET."
  (let ((i offset) (len (length text)))
    (loop while (< i len)
          do (let ((ch (char text i)))
               (cond
                 ((or (char= ch #\Space) (char= ch #\Tab))
                  (incf i))
                 ((char= ch #\Newline)
                  (incf i)
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
  "Add ADDITIONAL-INDENT spaces to every line of CODE."
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
                do (write-string prefix out)
                   (write-string (subseq code line-start line-end) out)
                   (when (< line-end (length code))
                     (write-char #\Newline out)))))))

(defun replace-form-pretty (text node new-code)
  "Replace NODE with NEW-CODE, preserving original indentation.
   Detects the indentation of the original form and applies it to
   the new code's first line and adjusts subsequent lines relative to that."
  (let* ((start (node-start node))
         (end (node-end node))
         (indent (detect-form-indentation text node))
         (indented-code (indent-code-by new-code indent)))
    (concatenate 'string
                 (subseq text 0 start)
                 indented-code
                 (subseq text end))))

;;; ============================================================
;;; Batch Operations
;;; ============================================================

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

(defun find-subform-matching (node text snippet)
  "Find the smallest descendant form of NODE whose source matches SNIPPET.
   Exact trimmed match is preferred over a contains-match; among equals
   the smallest span wins. Returns NIL when nothing matches."
  (let ((exact nil)
        (contains nil))
    (labels ((collect (n)
               (dolist (child (node-children n))
                 (let* ((raw (node-source-text text child))
                        (trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) raw)))
                   (cond
                     ((string= trimmed snippet)
                      (push child exact))
                     ((search snippet raw)
                      (push child contains)))
                   (collect child)))))
      (collect node)
      (labels ((smallest (nodes)
                 (first (sort nodes #'<
                              :key #'(lambda (n) (- (node-end n) (node-start n)))))))
        (cond
          (exact (smallest exact))
          (contains (smallest contains))
          (t nil))))))

(defun find-forms-containing (text snippet &key recovery)
  "Return a list of (index node) pairs for top-level forms in TEXT
   whose source contains SNIPPET."
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

(defun apply-single-edit (text edit &key recovery)
  "Apply a single EDIT plist to TEXT.
   EDIT is a plist with :operation, :code, and either :index or :line/:col.
   Returns the modified text."
  (case (getf edit :operation)
    (:replace-index (edit-replace-index text edit recovery))
    (:replace-position (edit-replace-position text edit recovery))
    (:delete-index (edit-delete-index text edit recovery))
    (:insert-after-index (edit-insert-after-index text edit recovery))
    (t (error "Unknown operation: ~a" (getf edit :operation)))))

(defun apply-batch-edits (text edits &key recovery)
  "Apply a list of EDIT plists to TEXT.
   Sorts index-based edits from highest to lowest to prevent index shifting.
   Position-based edits (line/col) are applied after index edits.
   Returns the final modified text."
  (let* ((index-edits (remove-if-not (lambda (e) (getf e :index)) edits))
         (position-edits (remove-if (lambda (e) (getf e :index)) edits))
         (sorted-index (sort (copy-list index-edits) (lambda (a b)
                                                      (> (getf a :index) (getf b :index))))))
    (reduce (lambda (current-text edit)
              (apply-single-edit current-text edit :recovery recovery))
            (append sorted-index position-edits)
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
    (concatenate 'string
                 (string-trim '(#\Space #\Tab #\Newline) text)
                 (string #\Newline)
                 code)))

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
           (form-text (subseq text from-start from-end)))
      ;; Step 1: Delete the source form's entire line.
      ;; del-start includes the preceding newline so no blank line remains;
      ;; del-end stops at the form's end (trailing parens belong to the parent).
      (let* ((del-start (if (plusp from-start)
                            (1- (preceding-line-start text from-start))
                            0))
             (deleted-text-str (concatenate 'string
                                            (subseq text 0 del-start)
                                            (subseq text from-end)))
             ;; Step 2: Find where the destination landed after deletion
             (dest-text (subseq text (node-start to-node) (node-end to-node)))
             (dest-pos (search dest-text deleted-text-str)))
        (unless dest-pos
          (error "Destination form not found after deletion"))
        ;; Insert after the destination form, indented like it is,
        ;; with at least one newline of separation.
        (let* ((dest-end (+ dest-pos (length dest-text)))
               (indent (count-leading-spaces deleted-text-str dest-pos))
               (indented-form (concatenate 'string
                                           (make-string indent :initial-element #\Space)
                                           form-text))
               (spacing-nls (max 1 (count-leading-newlines deleted-text-str dest-end))))
          (concatenate 'string
                       (subseq deleted-text-str 0 dest-end)
                       (make-string spacing-nls :initial-element #\Newline)
                       indented-form
                       (subseq deleted-text-str dest-end)))))))
;;; ============================================================
;;; Balance Analysis
;;; ============================================================

(defun balance-record-line (line depth line-start-depth lines)
  "Record line info and return updated state."
  (push (list :line line :depth depth
              :delta (- depth line-start-depth))
        lines))

(defun balance-check-close (ch line col depth errors kind)
  "Check for unexpected closing delimiter and record error if needed.
   Returns (values new-depth new-errors)."
  (if (zerop depth)
      (values depth
              (push (list :line line :col col
                          :message (format nil "Unexpected closing ~a (depth already 0)" kind))
                    errors))
      (values (1- depth) errors)))

(defun balance-process-line-comment (ch line col depth line-start-depth lines)
  "Process character inside line comment. Returns updated state."
  (if (char= ch #\Newline)
      (values t (balance-record-line line depth line-start-depth lines) (1+ line) 1)
      (values nil lines line (1+ col))))

(defun balance-process-block-comment (ch i text line col)
  "Process character inside block comment. Returns updated state."
  (let ((new-i i) (new-line line) (new-col (1+ col)) (ended nil))
    (when (and (char= ch #\|)
               (< (1+ i) (length text))
               (char= (char text (1+ i)) #\#))
      (setf ended t new-i (+ i 2) new-col (+ col 2)))
    (when (char= ch #\Newline)
      (setf new-line (1+ line) new-col 1))
    (values ended new-i new-line new-col)))

(defun balance-process-string (ch i text line col)
  "Process character inside string. Returns updated state."
  (let ((new-i i) (new-line line) (new-col (1+ col)) (ended nil))
    (when (char= ch #\Newline)
      (setf new-line (1+ line) new-col 1))
    (when (char= ch #\\)
      (setf new-i (+ i 2) new-col (+ col 2)))
    (when (char= ch #\")
      (setf ended t))
    (values ended new-i new-line new-col)))

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
      (t (values next-i (1+ col) nil)))))

(defun balance-process-normal (ch i text depth max-depth line col errors)
  "Process character in normal code mode. Returns updated state."
  (case ch
    (#\; (values i col depth max-depth errors :line-comment))
    (#\# (multiple-value-bind (ni nc in-block) (balance-process-hash i text col)
           (values ni nc depth max-depth errors (if in-block :block-comment :normal))))
    (#\" (values i (1+ col) depth max-depth errors :string))
    (#\( (incf depth)
         (values i (1+ col) depth (max max-depth depth) errors :normal))
    (#\) (multiple-value-bind (new-depth new-errors)
             (balance-check-close ch line col depth errors "paren")
           (values i (1+ col) new-depth max-depth new-errors :normal)))
    (#\[ (incf depth)
         (values i (1+ col) depth (max max-depth depth) errors :normal))
    (#\] (multiple-value-bind (new-depth new-errors)
             (balance-check-close ch line col depth errors "bracket")
           (values i (1+ col) new-depth max-depth new-errors :normal)))
    (#\{ (incf depth)
         (values i (1+ col) depth (max max-depth depth) errors :normal))
    (#\} (multiple-value-bind (new-depth new-errors)
             (balance-check-close ch line col depth errors "brace")
           (values i (1+ col) new-depth max-depth new-errors :normal)))
    (#\Newline (values i 1 depth max-depth errors :newline))
    (t (values i (1+ col) depth max-depth errors :normal))))

(defun balance-dispatch-line-comment (ch i text line col depth line-start-depth lines mode)
  "Dispatch line comment mode. Returns updated state values."
  (multiple-value-bind (ended new-lines new-line new-col)
      (balance-process-line-comment ch line col depth line-start-depth lines)
    (when ended
      (setf lines new-lines line-start-depth depth)
      (incf line) (setf col 1)
      (setf mode :normal))
    (incf col)
    (values i line col depth line-start-depth lines mode)))

(defun balance-dispatch-block-comment (ch i text line col depth line-start-depth lines mode)
  "Dispatch block comment mode. Returns updated state values."
  (multiple-value-bind (ended ni nl nc)
      (balance-process-block-comment ch i text line col)
    (when ended (setf i ni))
    (when (char= ch #\Newline)
      (setf line (1+ line) col 1))
    (incf col)
    (values i line col depth line-start-depth lines mode)))

(defun balance-dispatch-string (ch i text line col depth line-start-depth lines mode)
  "Dispatch string mode. Returns updated state values."
  (multiple-value-bind (ended ni nl nc)
      (balance-process-string ch i text line col)
    (when ended (setf i ni mode :normal))
    (when (char= ch #\Newline)
      (setf line (1+ line) col 1))
    (incf col)
    (values i line col depth line-start-depth lines mode)))

(defun balance-dispatch-normal (ch i text line col depth max-depth line-start-depth lines errors mode)
  "Dispatch normal mode. Returns updated state values."
  (multiple-value-bind (ni nc nd nmax nerrors nmode)
      (balance-process-normal ch i text depth max-depth line col errors)
    (setf i ni col nc depth nd max-depth nmax errors nerrors)
    (when (eq nmode :line-comment) (setf mode :line-comment))
    (when (eq nmode :block-comment) (setf mode :block-comment))
    (when (eq nmode :string) (setf mode :string))
    (when (eq nmode :newline)
      (push (list :line line :depth depth
                  :delta (- depth line-start-depth))
            lines)
      (setf line-start-depth depth)
      (incf line) (setf col 1))
    (incf col)
    (values i line col depth max-depth line-start-depth lines errors mode)))

(defun analyze-balance (text)
  "Analyze parenthesis/bracket balance in TEXT.
   Returns a plist with:
     :lines - list of plists (:line :depth :delta) per source line
     :max-depth - maximum nesting depth
     :final-depth - depth at end of file (0 = balanced)
     :errors - list of error plists (:line :col :message)"
  (let ((depth 0) (max-depth 0) (line 1) (col 1)
        (line-start-depth 0) (lines nil) (errors nil)
        (mode :normal))
    (loop for i from 0 below (length text)
          for ch = (char text i)
          do (case mode
               (:line-comment
                (multiple-value-setq (i line col depth line-start-depth lines mode)
                  (balance-dispatch-line-comment ch i text line col depth line-start-depth lines mode)))
               (:block-comment
                (multiple-value-setq (i line col depth line-start-depth lines mode)
                  (balance-dispatch-block-comment ch i text line col depth line-start-depth lines mode)))
               (:string
                (multiple-value-setq (i line col depth line-start-depth lines mode)
                  (balance-dispatch-string ch i text line col depth line-start-depth lines mode)))
               (:normal
                (multiple-value-setq (i line col depth max-depth line-start-depth lines errors mode)
                  (balance-dispatch-normal ch i text line col depth max-depth line-start-depth lines errors mode)))))
    (push (list :line line :depth depth
                :delta (- depth line-start-depth))
          lines)
    (when (/= depth 0)
      (push (list :line line :col col
                  :message (format nil "Unclosed forms: depth ~a at end of file" depth))
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
  "Process character inside block comment."
  (write-char ch result)
  (incf line-pos)
  (cond
    ((and (char= ch #\|)
          (< (1+ i) (length text))
          (char= (char text (1+ i)) #\#))
     (write-char (char text (1+ i)) result)
     (incf i) (incf line-pos)
     (values line-pos t))
    ((char= ch #\Newline)
     (values 0 nil))
    (t (values line-pos nil))))

(defun format-process-string (ch i text result line-pos)
  "Process character inside string."
  (write-char ch result)
  (incf line-pos)
  (cond
    ((and (char= ch #\\) (< (1+ i) (length text)))
     (write-char (char text (1+ i)) result)
     (incf i) (incf line-pos)
     (values line-pos nil))
    ((char= ch #\")
     (values line-pos t))
    (t (values line-pos nil))))

(defun format-process-hash (ch i text depth indent result line-pos need-indent)
  "Process # dispatch character. Returns (values new-i new-line-pos new-need-indent new-mode).
   new-i is the value to SET i to (loop auto-increments)."
  (cond
    ;; Block comment #|
    ((and (< (1+ i) (length text))
          (char= (char text (1+ i)) #\|))
     (multiple-value-bind (lp ni) (format-apply-indent depth indent result line-pos need-indent)
       (write-char ch result) (incf lp)
       (write-char (char text (1+ i)) result) (incf i) (incf lp)
       (values i lp nil :block-comment)))
    ;; Character literal #\
    ((and (< (1+ i) (length text))
          (char= (char text (1+ i)) #\\))
     (multiple-value-bind (lp ni) (format-apply-indent depth indent result line-pos need-indent)
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
       (write-char ch result) (incf lp)
       (values i lp nil :normal)))))

(defun format-process-open-delimiter (ch depth indent result line-pos need-indent)
  "Process opening delimiter. Returns (values new-line-pos new-need-indent new-depth)."
  (multiple-value-bind (lp ni) (format-apply-indent depth indent result line-pos need-indent)
    (write-char ch result) (incf lp)
    (values lp nil (1+ depth))))

(defun format-process-close-delimiter (ch depth indent result line-pos need-indent)
  "Process closing delimiter. Returns (values new-line-pos new-need-indent new-depth).
   Clamps depth to 0 — unmatched close delimiters don't make depth negative."
  (let ((new-depth (max 0 (1- depth))))
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
  "Dispatch block comment mode. Returns updated state."
  (multiple-value-bind (lp ended)
      (format-process-block-comment ch i text result line-pos)
    (setf line-pos lp)
    (when ended (setf mode :normal))
    (when (char= ch #\Newline)
      (setf line-pos 0 need-indent t))
    (values line-pos mode need-indent)))

(defun format-dispatch-string (ch i text result line-pos mode)
  "Dispatch string mode. Returns updated state."
  (multiple-value-bind (lp ended)
      (format-process-string ch i text result line-pos)
    (setf line-pos lp)
    (when ended (setf mode :normal))
    (values line-pos mode)))

(defun format-dispatch-space (ch i text depth indent result line-pos need-indent)
  "Dispatch whitespace character. Returns updated state."
  (unless need-indent
    (write-char #\Space result) (incf line-pos)
    (loop while (and (< (1+ i) (length text))
                     (member (char text (1+ i)) '(#\Space #\Tab)))
          do (incf i)))
  (values line-pos need-indent i))

(defun format-dispatch-hash (ch i text depth indent result line-pos need-indent mode)
  "Dispatch # character. Returns updated state."
  (multiple-value-bind (ni lp nindent nmode)
      (format-process-hash ch i text depth indent result line-pos need-indent)
    (values ni lp need-indent (or nmode mode))))

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
    ((#\[ #\{ #\()
     (multiple-value-bind (lp ni d)
         (format-dispatch-delimiter ch depth indent result line-pos need-indent t)
       (setf line-pos lp need-indent ni depth d)
       (values i line-pos need-indent mode depth)))
    ((#\] #\} #\))
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
                (multiple-value-setq (line-pos mode need-indent)
                  (format-dispatch-block-comment ch i text result line-pos mode need-indent)))
               (:string
                (multiple-value-setq (line-pos mode)
                  (format-dispatch-string ch i text result line-pos mode)))
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
