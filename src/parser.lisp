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
  "Find the form to operate on at the given LINE and COL (1-indexed).
   Finds the smallest form that contains the target offset and whose start
   line is at or before LINE. This means the cursor can be anywhere inside
   a form and it will target that form, not drill into subforms."
  (let* ((target-offset (cl-toolkit-ast:offset-to-line-col-inverse text line col))
         (all-nodes (find-node-at-offset-all ast target-offset))
         (best nil))
    ;; Among all nodes containing the offset, find the one with the
    ;; earliest start line (but not after target line). If multiple nodes
    ;; start on the same line, prefer the innermost (smallest) one.
    (dolist (node all-nodes)
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
   Returns a list of nodes that overlap the given range."
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
  "Delete the form at LINE, COL (1-indexed) from TEXT.
   When RECOVERY is T, use error recovery parser.
   Returns the modified source string."
  (let* ((ast (parse-for-edit text recovery))
         (node (find-form-at ast text line col)))
    (unless node
      (error "No form found at line ~a, col ~a" line col))
    (let ((node-line (node-line node))
          (node-col (node-col node)))
      (when (and node-line node-col)
        (format *error-output* "Deleting form at line ~a, col ~a (depth ~a)~%"
                node-line node-col (node-type node))))
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

(defun insert-form-at (text line col new-code &key after recovery)
  "Insert NEW-CODE at LINE, COL (1-indexed) in TEXT.
   If AFTER is nil, insert before; if AFTER is t, insert after.
   When RECOVERY is T, use error recovery parser.
   Returns the modified source string."
  (let* ((ast (parse-for-edit text recovery))
         (node (find-form-at ast text line col)))
    (unless node
      (error "No form found at line ~a, col ~a" line col))
    (let ((node-line (node-line node))
          (node-col (node-col node)))
      (when (and node-line node-col)
        (format *error-output* "~a form at line ~a, col ~a: ~a~%"
                (if after "Inserting after" "Inserting before")
                node-line node-col
                (or (node-name node) (node-type node)))))
    (let ((insert-at (if after (node-end node) (node-start node))))
      ;; Normalize new-code: ensure trailing newline
      (let ((code (if (and (> (length new-code) 0)
                           (char= (char new-code (1- (length new-code))) #\Newline))
                      new-code
                      (concatenate 'string new-code (string #\Newline)))))
        (concatenate 'string
                     (subseq text 0 insert-at)
                     code
                     (subseq text insert-at))))))

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
  "Replace the form at LINE, COL (1-indexed) with NEW-CODE in TEXT.
   When RECOVERY is T, use error recovery parser.
   Returns the modified source string."
  (let* ((ast (parse-for-edit text recovery))
         (node (find-form-at ast text line col)))
    (unless node
      (error "No form found at line ~a, col ~a" line col))
    (let ((node-line (node-line node))
          (node-col (node-col node)))
      (when (and node-line node-col)
        (format *error-output* "Replacing form at line ~a, col ~a: ~a~%"
                node-line node-col
                (or (node-name node) (node-type node)))))
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
    (values from-node to-node)))

(defun move-log-positions (from-node to-node)
  "Log the source and destination positions for move operation."
  (let ((fl (node-line from-node)) (fc (node-col from-node))
        (tl (node-line to-node))    (tc (node-col to-node)))
    (when (and fl fc)
      (format *error-output* "Moving form from line ~a, col ~a~%" fl fc))
    (when (and tl tc)
      (format *error-output* "  to after line ~a, col ~a~%" tl tc))))

(defun move-compute-regions (text from-node to-node)
  "Compute deletion and insertion regions for move operation."
  (let* ((from-start (node-start from-node))
         (from-end (node-end from-node))
         (to-start (node-start to-node))
         (to-end (node-end to-node))
         (form-text (subseq text from-start from-end))
         (del-end (skip-whitespace-and-newlines text from-end))
         (delete-len (- del-end from-start))
         (insert-at (if (< from-end to-start)
                        (- to-end delete-len)
                        to-start)))
    (values form-text del-end insert-at)))

(defun move-form (text from-line from-col to-line to-col &key recovery)
  "Move the form at (FROM-LINE, FROM-COL) to after (TO-LINE, TO-COL).
   When RECOVERY is T, use error recovery parser.
   Returns the modified source string."
  (multiple-value-bind (from-node to-node)
      (move-find-nodes text from-line from-col to-line to-col recovery)
    (move-log-positions from-node to-node)
    (multiple-value-bind (form-text del-end insert-at)
        (move-compute-regions text from-node to-node)
      (let ((insert-code (if (and (> (length form-text) 0)
                                 (char= (char form-text (1- (length form-text))) #\Newline))
                             form-text
                             (concatenate 'string form-text (string #\Newline)))))
        (let ((after-delete (concatenate 'string
                                         (subseq text 0 (node-start from-node))
                                         (subseq text del-end))))
          (setf insert-at (max 0 (min insert-at (length after-delete))))
          (concatenate 'string
                       (subseq after-delete 0 insert-at)
                       insert-code
                       (subseq after-delete insert-at)))))))

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
      (incf line) (setf col 1))
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
    (when ended (setf i ni))
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
  "Process closing delimiter. Returns (values new-line-pos new-need-indent new-depth)."
  (let ((new-depth (1- depth)))
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
    (values ni lp need-indent mode)))

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
         (insert-form-at text line col code :after after)))
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
