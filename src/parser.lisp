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
  "Find the deepest node containing OFFSET (0-indexed)."
  (when (and node
             (node-start node) (node-end node)
             (>= offset (node-start node))
             (< offset (node-end node)))
    (let ((best node))
      (dolist (child (node-children node))
        (when (nodep child)
          (let ((found (find-node-at-offset child offset)))
            (when found
              (setf best found)))))
      best)))

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
  "Find the innermost form containing the given LINE and COL (1-indexed).
   Returns the deepest AST node at that position."
  (let ((target-offset (cl-toolkit-ast:offset-to-line-col-inverse text line col)))
     (find-node-at-offset ast target-offset)))

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

(defun move-form (text from-line from-col to-line to-col &key recovery)
  "Move the form at (FROM-LINE, FROM-COL) to after (TO-LINE, TO-COL).
   When RECOVERY is T, use error recovery parser.
   Returns the modified source string."
  (let* ((ast (parse-for-edit text recovery))
         (from-node (find-form-at ast text from-line from-col))
         (to-node (find-form-at ast text to-line to-col)))
    (unless from-node
      (error "No form found at line ~a, col ~a" from-line from-col))
    (unless to-node
      (error "No form found at line ~a, col ~a" to-line to-col))
    (let ((from-node-line (node-line from-node))
          (from-node-col (node-col from-node))
          (to-node-line (node-line to-node))
          (to-node-col (node-col to-node)))
      (when (and from-node-line from-node-col)
        (format *error-output* "Moving form from line ~a, col ~a~%"
                from-node-line from-node-col))
      (when (and to-node-line to-node-col)
        (format *error-output* "  to after line ~a, col ~a~%"
                to-node-line to-node-col)))
    (let* ((from-start (node-start from-node))
           (from-end (node-end from-node))
           (to-start (node-start to-node))
           (to-end (node-end to-node))
           ;; Extract the form text
           (form-text (subseq text from-start from-end))
           ;; Find how much whitespace/newlines to delete after the form
           (del-end (skip-whitespace-and-newlines text from-end))
           (delete-len (- del-end from-start))
           ;; Determine insert position based on relative positions
           (insert-at (if (< from-end to-start)
                          ;; Source is before destination: insert after destination
                          ;; Account for deletion shifting positions left
                          (- to-end delete-len)
                          ;; Source is after destination: insert before destination
                          to-start))
           ;; Ensure form has a trailing newline
           (insert-code (if (and (> (length form-text) 0)
                                (char= (char form-text (1- (length form-text))) #\Newline))
                            form-text
                            (concatenate 'string form-text (string #\Newline)))))
      ;; Delete from original position, then insert at new position
      (let ((after-delete (concatenate 'string
                                       (subseq text 0 from-start)
                                       (subseq text del-end))))
        ;; Clamp insert-at to valid range based on actual deleted text
        (setf insert-at (max 0 (min insert-at (length after-delete))))
        (concatenate 'string
                     (subseq after-delete 0 insert-at)
                     insert-code
                     (subseq after-delete insert-at))))))

;;; ============================================================
;;; Balance Analysis
;;; ============================================================

(defun analyze-balance (text)
  "Analyze parenthesis/bracket balance in TEXT.
   Returns a plist with:
     :lines - list of plists (:line :depth :delta) per source line
     :max-depth - maximum nesting depth
     :final-depth - depth at end of file (0 = balanced)
     :errors - list of error plists (:line :col :message)"
  (let ((depth 0)
        (max-depth 0)
        (in-string nil)
        (in-line-comment nil)
        (in-block-comment nil)
        (line 1)
        (col 1)
        (line-start-depth 0)
        (lines nil)
        (errors nil))
    (loop for i from 0 below (length text)
          for ch = (char text i)
          do (cond
               ;; Inside a line comment - skip until newline
               (in-line-comment
                (when (char= ch #\Newline)
                  (setf in-line-comment nil)
                  ;; Record line info
                  (push (list :line line :depth depth
                              :delta (- depth line-start-depth))
                        lines)
                  (setf line-start-depth depth)
                  (incf line)
                  (setf col 1))
                (incf col))

               ;; Inside a block comment - skip until |#
               (in-block-comment
                (when (and (char= ch #\|)
                           (< (1+ i) (length text))
                           (char= (char text (1+ i)) #\#))
                  (setf in-block-comment nil)
                  (incf i)  ; skip the #
                  (incf col))
                (when (char= ch #\Newline)
                  (incf line)
                  (setf col 1))
                (incf col))

               ;; Inside a string
               (in-string
                (when (char= ch #\Newline)
                  (incf line)
                  (setf col 1))
                (when (char= ch #\\)
                  (incf i)  ; skip escaped char
                  (incf col))
                (when (char= ch #\")
                  (setf in-string nil))
                (incf col))

               ;; Normal code
               (t
                (case ch
                   (#\;
                    (setf in-line-comment t))
                   (#\#
                    (cond
                      ((and (< (1+ i) (length text))
                            (char= (char text (1+ i)) #\|))
                       (setf in-block-comment t)
                       (incf i))  ; skip the |
                      ((and (< (1+ i) (length text))
                            (char= (char text (1+ i)) #\\))
                       (incf i)  ; skip the \, now i points to char after \
                       (incf col)
                       ;; Skip character literal: name or single char
                       (cond
                         ;; Named char: #\Space, #\Newline, etc.
                         ((and (< i (length text))
                               (alphanumericp (char text i)))
                          (loop while (and (< i (length text))
                                           (alphanumericp (char text i)))
                                do (incf i) (incf col)))
                         ;; Single char: #\a, #\(, #\), etc.
                         ((< i (length text))
                          (incf i)
                          (incf col))))
                      (t (incf col))))
                  (#\"
                   (setf in-string t))
                  (#\(
                   (incf depth)
                   (when (> depth max-depth)
                     (setf max-depth depth)))
                  (#\)
                   (when (zerop depth)
                     ;; Depth went negative - unexpected closing paren
                     (push (list :line line :col col
                                 :message "Unexpected closing paren (depth already 0)")
                           errors))
                   (decf depth))
                  (#\[
                   (incf depth)
                   (when (> depth max-depth)
                     (setf max-depth depth)))
                  (#\]
                   (when (zerop depth)
                     (push (list :line line :col col
                                 :message "Unexpected closing bracket (depth already 0)")
                           errors))
                   (decf depth))
                  (#\{
                   (incf depth)
                   (when (> depth max-depth)
                     (setf max-depth depth)))
                  (#\}
                   (when (zerop depth)
                     (push (list :line line :col col
                                 :message "Unexpected closing brace (depth already 0)")
                           errors))
                   (decf depth))
                   (#\Newline
                    (push (list :line line :depth depth
                                :delta (- depth line-start-depth))
                          lines)
                    (setf line-start-depth depth)
                    (incf line)
                    (setf col 1)))
                (incf col)))
          ;; Record line at end of text
          finally (push (list :line line :depth depth
                              :delta (- depth line-start-depth))
                        lines))
    ;; Check final depth
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

(defun format-source (text &key (indent "  ") (max-width 80))
  "Reformat Lisp source TEXT with consistent indentation.
   INDENT is the string used for one level of indentation (default two spaces).
   Returns the reformatted source string."
  (let ((depth 0)
        (in-string nil)
        (in-line-comment nil)
        (in-block-comment nil)
        (result (make-string-output-stream))
        (line-pos 0)
        (need-indent t))
    (loop for i from 0 below (length text)
          for ch = (char text i)
          do (cond
               ;; Line comment - copy until newline
               (in-line-comment
                (write-char ch result)
                (incf line-pos)
                (when (char= ch #\Newline)
                  (setf in-line-comment nil)
                  (setf line-pos 0)
                  (setf need-indent t)))

               ;; Block comment - copy until |#
               (in-block-comment
                (write-char ch result)
                (incf line-pos)
                (when (and (char= ch #\|)
                           (< (1+ i) (length text))
                           (char= (char text (1+ i)) #\#))
                  (setf in-block-comment nil)
                  (write-char (char text (1+ i)) result)
                  (incf i)
                  (incf line-pos))
                (when (char= ch #\Newline)
                  (setf line-pos 0)
                  (setf need-indent t)))

               ;; String - copy verbatim
               (in-string
                (write-char ch result)
                (incf line-pos)
                (when (and (char= ch #\\) (< (1+ i) (length text)))
                  (write-char (char text (1+ i)) result)
                  (incf i)
                  (incf line-pos))
                (when (char= ch #\")
                  (setf in-string nil)))

               ;; Normal code
               (t
                (case ch
                   ;; Whitespace
                   ((#\Space #\Tab)
                    (if need-indent
                        ;; At line start: skip (indentation handled elsewhere)
                        nil
                        ;; Mid-line: collapse to single space
                        (progn
                          (write-char #\Space result)
                          (incf line-pos)
                          ;; Skip additional whitespace
                          (loop while (and (< (1+ i) (length text))
                                           (member (char text (1+ i)) '(#\Space #\Tab)))
                                do (incf i)))))
                  (#\Newline
                   (write-char ch result)
                   (setf line-pos 0)
                   (setf need-indent t))
                  ;; Comments
                  (#\;
                   (when need-indent
                     (write-string (indent-string depth indent) result)
                     (setf line-pos (* depth (length indent)))
                     (setf need-indent nil))
                   (write-char ch result)
                   (incf line-pos)
                   (setf in-line-comment t))
                   (#\#
                    (cond
                      ;; Block comment #|
                      ((and (< (1+ i) (length text))
                            (char= (char text (1+ i)) #\|))
                       (when need-indent
                         (write-string (indent-string depth indent) result)
                         (setf line-pos (* depth (length indent)))
                         (setf need-indent nil))
                       (write-char ch result)
                       (incf line-pos)
                       (write-char (char text (1+ i)) result)
                       (incf i)
                       (incf line-pos)
                       (setf in-block-comment t))
                      ;; Character literal #\
                      ((and (< (1+ i) (length text))
                            (char= (char text (1+ i)) #\\))
                       (when need-indent
                         (write-string (indent-string depth indent) result)
                         (setf line-pos (* depth (length indent)))
                         (setf need-indent nil))
                       (write-char ch result)
                       (incf line-pos)
                       (write-char (char text (1+ i)) result)
                       (incf i)
                       (incf line-pos)
                       ;; Skip char name or single char
                       (let ((start-i i))
                         (loop while (and (< i (length text))
                                          (alphanumericp (char text i)))
                               do (write-char (char text i) result)
                                  (incf i)
                                  (incf line-pos))
                         (when (= i start-i)
                           (when (< i (length text))
                             (write-char (char text i) result)
                             (incf i)
                             (incf line-pos)))))
                      ;; Other # dispatch
                      (t
                       (when need-indent
                         (write-string (indent-string depth indent) result)
                         (setf line-pos (* depth (length indent)))
                         (setf need-indent nil))
                       (write-char ch result)
                       (incf line-pos))))
                  ;; String start
                  (#\"
                   (when need-indent
                     (write-string (indent-string depth indent) result)
                     (setf line-pos (* depth (length indent)))
                     (setf need-indent nil))
                   (write-char ch result)
                   (incf line-pos)
                   (setf in-string t))
                  ;; Opening delimiters
                  (#\(
                   (when need-indent
                     (write-string (indent-string depth indent) result)
                     (setf line-pos (* depth (length indent)))
                     (setf need-indent nil))
                   (write-char ch result)
                   (incf line-pos)
                   (incf depth))
                  (#\[
                   (when need-indent
                     (write-string (indent-string depth indent) result)
                     (setf line-pos (* depth (length indent)))
                     (setf need-indent nil))
                   (write-char ch result)
                   (incf line-pos)
                   (incf depth))
                  (#\{
                   (when need-indent
                     (write-string (indent-string depth indent) result)
                     (setf line-pos (* depth (length indent)))
                     (setf need-indent nil))
                   (write-char ch result)
                   (incf line-pos)
                   (incf depth))
                  ;; Closing delimiters
                  (#\)
                   (decf depth)
                   (when need-indent
                     (write-string (indent-string depth indent) result)
                     (setf line-pos (* depth (length indent)))
                     (setf need-indent nil))
                   (write-char ch result)
                   (incf line-pos))
                  (#\]
                   (decf depth)
                   (when need-indent
                     (write-string (indent-string depth indent) result)
                     (setf line-pos (* depth (length indent)))
                     (setf need-indent nil))
                   (write-char ch result)
                   (incf line-pos))
                  (#\}
                   (decf depth)
                   (when need-indent
                     (write-string (indent-string depth indent) result)
                     (setf line-pos (* depth (length indent)))
                     (setf need-indent nil))
                   (write-char ch result)
                   (incf line-pos))
                  ;; Everything else
                  (t
                   (when need-indent
                     (write-string (indent-string depth indent) result)
                     (setf line-pos (* depth (length indent)))
                     (setf need-indent nil))
                    (write-char ch result)
                    (incf line-pos))))))
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
