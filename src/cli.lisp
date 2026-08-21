(in-package #:cl-toolkit)

;;; ============================================================
;;; CLI Utilities
;;; ============================================================

(defun read-file-to-string (path)
  "Read entire file into a string."
  (with-open-file (stream path :direction :input :if-does-not-exist nil)
    (unless stream
      (format *error-output* "Cannot read file: ~a~%" path)
      (clingon:exit 1))
    (let ((content (make-string (file-length stream))))
      (read-sequence content stream)
      content)))

(defun read-stdin ()
  "Read all input from stdin."
  (with-output-to-string (out)
    (loop for line = (read-line *standard-input* nil nil)
          while line
          do (write-string line out)
             (terpri out))))

(defun read-input (cmd)
  "Read input from --code, --file, or stdin (in that order)."
  (let ((code (clingon:getopt cmd :code))
        (file (clingon:getopt cmd :file)))
    (cond
      (code code)
      (file (read-file-to-string file))
      (t (read-stdin)))))

(defun output-json (node)
  "Write node as JSON to stdout."
  (cl-toolkit-ast::node-to-json node *standard-output*)
  (terpri))

(defun generate-unified-diff (old-text new-text file-path)
  "Generates a unified diff string between OLD-TEXT and NEW-TEXT."
  (let ((tmp-old (asdf:system-relative-pathname :cl-toolkit "tmp-old.txt"))
        (tmp-new (asdf:system-relative-pathname :cl-toolkit "tmp-new.txt")))
    (unwind-protect
         (progn
           (with-open-file (s tmp-old :direction :output :if-exists :supersede)
             (write-string old-text s))
           (with-open-file (s tmp-new :direction :output :if-exists :supersede)
             (write-string new-text s))
           (multiple-value-bind (diff-output err-output exit-code)
               (uiop:run-program (list "diff" "-u" 
                                       "--label" (format nil "a/~A" file-path)
                                       "--label" (format nil "b/~A" file-path)
                                       (namestring tmp-old)
                                       (namestring tmp-new))
                                 :ignore-error-status t
                                 :output :string)
             (declare (ignore err-output exit-code))
             (if (string= diff-output "")
                 nil
                 diff-output)))
      (when (probe-file tmp-old) (delete-file tmp-old))
      (when (probe-file tmp-new) (delete-file tmp-new)))))

(defun output-edit-result (text &optional error-msg)
  "Output a JSON edit result."
  (if error-msg
      (format *standard-output* "{\"success\":false,\"error\":\"~a\"}~%"
              (cl-toolkit-ast::escape-json-string error-msg))
      (format *standard-output* "{\"success\":true,\"source\":\"~a\"}~%"
              (cl-toolkit-ast::escape-json-string text))))

(defun write-result-to-file (file result &optional quiet)
  "Write RESULT source to FILE. Creates FILE.bak backup first."
  (let ((source (if (stringp result)
                    result
                    (getf result :source))))
    (unless source
      (error "No source in result"))
    (let ((bak (concatenate 'string file ".bak")))
      (when (probe-file file)
        (when (probe-file bak)
          (delete-file bak))
        (rename-file file bak))
      (with-open-file (stream file :direction :output
                                   :if-exists :supersede
                                   :if-does-not-exist :create)
        (write-string source stream))
      (unless quiet
        (format *error-output* "Wrote ~a (backup: ~a)~%" file bak)))))

(defun preview-edit (original-text result file)
  "Show diff preview of changes without writing."
  (let ((diff (generate-unified-diff original-text result file)))
    (if diff
        (format *standard-output* "~a" diff)
        (format *standard-output* "No changes made.~%"))))

;;; ============================================================
;;; Shared Edit Helpers
;;; ============================================================

(defun deliver-edit-result (result original-text file preview write quiet)
  "Dispatch RESULT to preview, in-place write, or stdout.
   PREVIEW and WRITE require FILE; exits with an error otherwise."
  (cond
    (preview
     (if file
         (preview-edit original-text result file)
         (progn
           (format *error-output* "Error: --preview requires --file~%")
           (clingon:exit 1))))
    (write
     (if file
         (let ((diff (generate-unified-diff original-text result file)))
           (write-result-to-file file result quiet)
           (if diff
               (format *standard-output* "~a" diff)
               (format *standard-output* "No changes made.~%")))
         (progn
           (format *error-output* "Error: --write requires --file~%")
           (clingon:exit 1))))
    (t (format *standard-output* "~a" result))))

(defun validate-new-code (code &optional skip)
  "Validate that CODE parses as Lisp unless SKIP is true.
   Exits with an error on invalid input."
  (when (and (not skip) code)
    (let ((input-ast (cl-toolkit-grammar::parse-lisp-source code)))
      (when (eq (node-type input-ast) :error)
        (format *error-output* "Input code validation failed: ~a~%"
                (node-value input-ast))
        (clingon:exit 1)))))

(defun validate-edited-source (result recovery &optional skip)
  "Validate that RESULT parses as Lisp unless SKIP is true.
   RECOVERY selects the error-recovery parser. Exits with an error on failure."
  (when (and (not skip) result (> (length result) 0))
    (let ((result-ast (if recovery
                          (cl-toolkit-grammar::parse-with-recovery result)
                          (cl-toolkit-grammar::parse-lisp-source result))))
      (when (eq (node-type result-ast) :error)
        (format *error-output* "Result validation failed: ~a~%"
                (node-value result-ast))
        (clingon:exit 1)))))

(defmacro with-edit-context ((cmd &key code-key) &body body)
  "Bind the standard edit-command options from CMD and run BODY.
   CODE-KEY names an extra option bound to CODE (e.g. :replace-code).
   Bound variables: line col index name end pretty code write preview quiet
   recovery no-validate-input no-validate-result file text original-text."
  `(let* ((line (clingon:getopt ,cmd :line))
          (col (clingon:getopt ,cmd :col))
          (index (clingon:getopt ,cmd :index))
          (name (clingon:getopt ,cmd :name))
          (end (clingon:getopt ,cmd :end))
          (pretty (clingon:getopt ,cmd :pretty))
          ,@(when code-key `((code (clingon:getopt ,cmd ,code-key))))
          (write (clingon:getopt ,cmd :write))
          (preview (clingon:getopt ,cmd :preview))
          (quiet (clingon:getopt ,cmd :quiet))
          (recovery (clingon:getopt ,cmd :recovery))
          (no-validate-input (clingon:getopt ,cmd :no-validate-input))
          (no-validate-result (clingon:getopt ,cmd :no-validate-result))
          (file (clingon:getopt ,cmd :file))
          (text (read-input ,cmd))
          (original-text (when file (read-file-to-string file))))
     ,@body))

;;; ============================================================
;;; Shared Options
;;; ============================================================

(defun make-preview-option ()
  (clingon:make-option :flag :long-name "preview"
                       :description "Show diff preview without writing"
                       :key :preview))

(defun make-recovery-option ()
  "Create the --recovery option."
  (clingon:make-option :flag
                       :long-name "recovery"
                       :description "Use error recovery parser"
                       :key :recovery))

(defun make-quiet-option ()
  "Create the --quiet option."
  (clingon:make-option :flag
                       :long-name "quiet"
                       :description "Suppress informational output"
                       :key :quiet))

(defun make-write-option ()
  "Create the --write option for in-place editing."
  (clingon:make-option :flag
                       :long-name "write"
                       :description "Write result back to file"
                       :key :write))

;;; ============================================================
;;; Parse Command
;;; ============================================================

(defun parse/handler (cmd)
  (let* ((recovery (clingon:getopt cmd :recovery))
         (parser (if recovery
                     #'cl-toolkit-grammar::parse-with-recovery
                     #'cl-toolkit-grammar::parse-lisp-source))
         (text (read-input cmd))
         (ast (funcall parser text)))
    (output-json ast)))

(defun parse/command ()
  (clingon:make-command
   :name "parse"
   :usage "[FILE | --code CODE]"
   :description "Parse file or inline code to JSON AST"
   :long-description "Parse a Lisp source file and output its AST as JSON. ~
                      If no file is given, reads from stdin."
   :options (list
             (make-recovery-option)
             (clingon:make-option :string
                                  :long-name "code"
                                  :description "Parse inline code string"
                                  :key :code)
             (clingon:make-option :string
                                  :long-name "file"
                                  :short-name #\f
                                  :description "File to parse"
                                  :key :file))
   :handler #'parse/handler
   :examples '(("Parse a file:" . "cl-toolkit parse myfile.lisp")
               ("Parse with recovery:" . "cl-toolkit parse --recovery myfile.lisp")
               ("Parse inline code:" . "cl-toolkit parse --code '(+ 1 2)'"))))

;;; ============================================================
;;; Find Command
;;; ============================================================

(defun find/handler (cmd)
  (let* ((line (clingon:getopt cmd :line))
         (col (clingon:getopt cmd :col))
         (text (read-input cmd))
         (ast (cl-toolkit-grammar::parse-lisp-source text))
         (found (find-form-at ast text line col)))
    (if found
        (output-json found)
        (progn
          (format *error-output* "No form found at line ~a, col ~a~%" line col)
          (clingon:exit 1)))))

(defun find/command ()
  (clingon:make-command
   :name "find"
   :usage "(-f FILE | --code CODE) -l LINE -c COL"
   :description "Find form at position and return as JSON"
   :options (list
             (clingon:make-option :string
                                  :long-name "file"
                                  :short-name #\f
                                  :description "File to search"
                                  :key :file)
             (clingon:make-option :string
                                  :long-name "code"
                                  :description "Inline code to search"
                                  :key :code)
             (clingon:make-option :integer
                                  :long-name "line"
                                  :short-name #\l
                                  :description "Line number (1-indexed)"
                                  :required t
                                  :key :line)
             (clingon:make-option :integer
                                  :long-name "col"
                                  :short-name #\c
                                  :description "Column number (1-indexed)"
                                  :required t
                                  :key :col))
   :handler #'find/handler
   :examples '(("Find form at line 5, col 2:" . "cl-toolkit find -f myfile.lisp -l 5 -c 2"))))

;;; ============================================================
;;; Extract Command
;;; ============================================================

(defun extract/handler (cmd)
  (let* ((line1 (clingon:getopt cmd :line1))
         (col1 (clingon:getopt cmd :col1))
         (line2 (clingon:getopt cmd :line2))
         (col2 (clingon:getopt cmd :col2))
         (text (read-input cmd))
         (ast (cl-toolkit-grammar::parse-lisp-source text))
         (forms (extract-range ast text line1 col1 line2 col2)))
    (format *standard-output* "[")
    (loop for form in forms
          for i from 0
          do (unless (zerop i) (format *standard-output* ","))
             (cl-toolkit-ast::node-to-json form *standard-output*))
    (format *standard-output* "]~%")))

(defun extract/command ()
  (clingon:make-command
   :name "extract"
   :usage "(-f FILE | --code CODE) --line1 L1 --col1 C1 --line2 L2 --col2 C2"
   :description "Extract forms in range as JSON array"
   :options (list
             (clingon:make-option :string :long-name "file" :short-name #\f
                                  :description "File" :key :file)
             (clingon:make-option :string :long-name "code"
                                  :description "Inline code" :key :code)
             (clingon:make-option :integer :long-name "line1" :short-name #\l
                                  :description "Start line" :required t :key :line1)
             (clingon:make-option :integer :long-name "col1" :short-name #\a
                                  :description "Start col" :required t :key :col1)
             (clingon:make-option :integer :long-name "line2" :short-name #\m
                                  :description "End line" :required t :key :line2)
             (clingon:make-option :integer :long-name "col2" :short-name #\b
                                  :description "End col" :required t :key :col2))
   :handler #'extract/handler))

;;; ============================================================
;;; Validate Command
;;; ============================================================

(defun validate/handler (cmd)
  (let* ((recovery (clingon:getopt cmd :recovery-flag))
         (text (read-input cmd))
         (ast (if recovery
                  (cl-toolkit-grammar::parse-with-recovery text)
                  (cl-toolkit-grammar::parse-lisp-source text)))
         (result (validate ast)))
    (format *standard-output* "{")
    (format *standard-output* "\"balanced\":~a"
            (if (getf result :balanced) "true" "false"))
    (format *standard-output* ",\"errors\":[")
    (loop for err in (getf result :errors)
          for i from 0
          do (unless (zerop i) (format *standard-output* ","))
             (format *standard-output*
                     "{\"line\":~a,\"col\":~a,\"message\":\"~a\"}"
                     (if (first err) (format nil "~a" (first err)) "null")
                     (if (second err) (format nil "~a" (second err)) "null")
                     (cl-toolkit-ast::escape-json-string (third err))))
    (format *standard-output* "],\"warnings\":[")
    (loop for warn in (getf result :warnings)
          for i from 0
          do (unless (zerop i) (format *standard-output* ","))
             (format *standard-output*
                     "{\"line\":~a,\"col\":~a,\"message\":\"~a\"}"
                     (if (first warn) (format nil "~a" (first warn)) "null")
                     (if (second warn) (format nil "~a" (second warn)) "null")
                     (cl-toolkit-ast::escape-json-string (third warn))))
    (format *standard-output* "]~%}~%")))

(defun validate/command ()
  (clingon:make-command
   :name "validate"
   :usage "(-f FILE | --code CODE)"
   :description "Validate source and report errors/warnings as JSON"
   :options (list
             (clingon:make-option :string :long-name "file" :short-name #\f
                                  :description "File to validate" :key :file)
             (clingon:make-option :string :long-name "code"
                                  :description "Inline code to validate" :key :code)
             (clingon:make-option :flag :long-name "recovery"
                                  :description "Use error recovery parser" :key :recovery-flag))
   :handler #'validate/handler))

;;; ============================================================
;;; Top-Level Command
;;; ============================================================

(defun top-level/handler (cmd)
  (let* ((names (clingon:getopt cmd :names))
         (text (read-input cmd))
         (ast (cl-toolkit-grammar::parse-lisp-source text))
         (forms (list-top-level ast)))
    (if names
        ;; Concise name listing with indices
        (loop for form in forms
              for i from 0
              do (multiple-value-bind (line col)
                     (cl-toolkit-ast:offset-to-line-col text (cl-toolkit-ast:node-start form))
                   (let ((name (cl-toolkit-ast:node-form-name form)))
                     (format *standard-output* "~a: ~a  [line ~a, col ~a]~%"
                             i (or name "?") (1+ line) col))))
        ;; Existing JSON array output
        (progn
          (format *standard-output* "[")
          (loop for form in forms
                for i from 0
                do (unless (zerop i) (format *standard-output* ","))
                   (cl-toolkit-ast::node-to-json form *standard-output*))
          (format *standard-output* "]~%")))))

(defun top-level/command ()
  (clingon:make-command
   :name "top-level"
   :usage "(-f FILE | --code CODE) [--names]"
   :description "List top-level forms as JSON array, or with --names show form names with indices"
   :options (list
             (clingon:make-option :string :long-name "file" :short-name #\f
                                  :description "File to list" :key :file)
             (clingon:make-option :string :long-name "code"
                                  :description "Inline code" :key :code)
             (clingon:make-option :flag :long-name "names"
                                  :description "List form names with indices"
                                  :key :names))
   :handler #'top-level/handler))

;;; ============================================================
;;; Balance Command
;;; ============================================================

(defun balance/handler (cmd)
  (let* ((text (read-input cmd))
         (result (analyze-balance text)))
    (format *standard-output* "{")
    (format *standard-output* "\"max_depth\":~a" (getf result :max-depth))
    (format *standard-output* ",\"final_depth\":~a" (getf result :final-depth))
    (format *standard-output* ",\"balanced\":~a"
            (if (= (getf result :final-depth) 0) "true" "false"))
    (format *standard-output* ",\"lines\":[")
    (loop for line-info in (getf result :lines)
          for i from 0
          do (unless (zerop i) (format *standard-output* ","))
             (format *standard-output*
                     "{\"line\":~a,\"depth\":~a,\"delta\":~a}"
                     (getf line-info :line)
                     (getf line-info :depth)
                     (getf line-info :delta)))
    (format *standard-output* "]")
    (format *standard-output* ",\"errors\":[")
    (loop for err in (getf result :errors)
          for i from 0
          do (unless (zerop i) (format *standard-output* ","))
             (format *standard-output*
                     "{\"line\":~a,\"col\":~a,\"message\":\"~a\"}"
                     (getf err :line)
                     (getf err :col)
                     (cl-toolkit-ast::escape-json-string (getf err :message))))
    (format *standard-output* "]")
    (format *standard-output* "}~%")))

(defun balance/command ()
  (clingon:make-command
   :name "balance"
   :usage "--file FILE | --code CODE"
   :description "Analyze parenthesis/bracket balance"
   :options (list
             (clingon:make-option :string :long-name "file" :short-name #\f
                                  :description "File to analyze" :key :file)
             (clingon:make-option :string :long-name "code"
                                  :description "Inline code to analyze" :key :code))
   :handler #'balance/handler))

;;; ============================================================
;;; Format Command
;;; ============================================================

(defun format/handler (cmd)
  (let* ((indent (clingon:getopt cmd :indent))
         (write (clingon:getopt cmd :write))
         (quiet (clingon:getopt cmd :quiet))
         (file (clingon:getopt cmd :file))
         (text (read-input cmd))
         (original-text (when file (read-file-to-string file)))
         (formatted (format-source text :indent indent)))
    (deliver-edit-result formatted original-text file nil write quiet)))

(defun format/command ()
  (clingon:make-command
   :name "format"
   :usage "--file FILE | --code CODE"
   :description "Reformat source with consistent indentation"
   :options (list
             (clingon:make-option :string :long-name "file" :short-name #\f
                                  :description "File to format" :key :file)
             (clingon:make-option :string :long-name "code"
                                   :description "Inline code to format" :key :code)
              (clingon:make-option :string :long-name "indent"
                                   :description "Indentation string (default: two spaces)"
                                   :initial-value "  "
                                   :key :indent)
              (make-write-option)
              (make-quiet-option))
    :handler #'format/handler))

;;; ============================================================
;;; Delete Command
;;; ============================================================

(defun delete/handler (cmd)
  (with-edit-context (cmd)
    (handler-case
        (let ((result
                (cond
                  (end
                   (delete-last-top-level text :recovery recovery))
                  (name
                   (let ((node (find-top-level-by-name text name :recovery recovery)))
                     (unless node
                       (format *error-output* "Error: No top-level form named '~a'~%" name)
                       (clingon:exit 1))
                     (delete-node-from-text text node)))
                  (index
                   (delete-top-level-at text index :recovery recovery))
                  ((and line col)
                   (delete-form-at text line col :recovery recovery))
                  (t
                   (format *error-output* "Error: --end, --name, --index, or --line/--col required~%")
                   (clingon:exit 1)))))
          (validate-edited-source result recovery no-validate-result)
          (deliver-edit-result result original-text file preview write quiet))
      (error (c)
        (output-edit-result nil (format nil "~a" c))))))

(defun delete-form/command ()
  (clingon:make-command
   :name "delete-form"
   :usage "(-f FILE | --code CODE) (--end | --name NAME | --index N | --line L --col C)"
   :description "Delete form at end of file, by name, by index, or by position"
   :long-description "Delete a form from the source. ~
                      Use --end to delete the last top-level form, --name to delete ~
                      by form name, --index to delete the N-th top-level form, ~
                      or --line/--col to delete by position. ~
                      Use --preview to see changes before applying. ~
                      Validates result by default; use --no-validate-result to skip."
   :options (list
              (clingon:make-option :string :long-name "file" :short-name #\f
                                   :description "File to edit" :key :file)
              (clingon:make-option :string :long-name "code"
                                   :description "Inline code" :key :code)
              (clingon:make-option :integer :long-name "line" :short-name #\l
                                   :description "Line number" :key :line)
              (clingon:make-option :integer :long-name "col" :short-name #\c
                                   :description "Column number" :key :col)
              (clingon:make-option :integer :long-name "index"
                                   :description "Top-level form index (0-based)" :key :index)
              (clingon:make-option :string :long-name "name" :short-name #\n
                                   :description "Top-level form name" :key :name)
              (clingon:make-option :flag :long-name "end"
                                   :description "Delete the last top-level form" :key :end)
              (make-write-option)
              (make-preview-option)
              (make-quiet-option)
               (make-recovery-option)
               (clingon:make-option :flag :long-name "no-validate-input"
                                    :description "Skip input code validation"
                                    :key :no-validate-input)
               (clingon:make-option :flag :long-name "no-validate-result"
                                    :description "Skip result validation"
                                    :key :no-validate-result))
    :handler #'delete/handler))

;;; ============================================================
;;; Insert-at Command (simple text insertion at position)
;;; ============================================================

(defun insert-at/handler (cmd)
  "Insert text at cursor position without form logic."
  (with-edit-context (cmd :code-key :insert-code)
    (unless (and line col code)
      (format *error-output* "Error: --line, --col, and --insert are required~%")
      (clingon:exit 1))
    ;; Find the offset from line/col
    (let ((offset (cl-toolkit-ast::offset-to-line-col-inverse text line col)))
      (when (null offset)
        (format *error-output* "Error: Invalid position (~a, ~a)~%" line col)
        (clingon:exit 1))
      ;; Insert text at offset
      (let ((result (concatenate 'string
                                 (subseq text 0 offset)
                                 code
                                 (subseq text offset))))
        (deliver-edit-result result original-text file preview write quiet)))))

(defun insert-at/command ()
  (clingon:make-command
   :name "insert"
   :usage "(-f FILE | --code CODE) --insert CODE --line L --col C"
   :description "Insert text at cursor position"
   :long-description "Insert code at the given cursor position. Unlike insert-form, ~
                      this simply inserts text at the position without form detection."
   :options (list
             (clingon:make-option :string :long-name "file" :short-name #\f
                                  :description "File to edit" :key :file)
             (clingon:make-option :string :long-name "source"
                                  :description "Source code to edit" :key :source)
             (clingon:make-option :integer :long-name "line" :short-name #\l
                                  :description "Line number" :required t :key :line)
             (clingon:make-option :integer :long-name "col" :short-name #\c
                                  :description "Column number" :required t :key :col)
             (clingon:make-option :string :long-name "insert" :short-name #\i
                                  :description "Code to insert" :required t :key :insert-code)
              (make-write-option)
              (make-preview-option)
              (make-quiet-option))
   :handler #'insert-at/handler))

;;; ============================================================
;;; Insert Command (form-level insertion)
;;; ============================================================

(defun insert/handler (cmd)
  (with-edit-context (cmd :code-key :insert-code)
    (validate-new-code code no-validate-input)
    (handler-case
        (let ((result
                (if (and line col code)
                    (insert-form-at text line col code :recovery recovery)
                    (progn
                      (format *error-output* "Error: --line/--col/--insert required~%")
                      (clingon:exit 1)))))
          (validate-edited-source result recovery no-validate-result)
          (deliver-edit-result result original-text file preview write quiet))
      (error (c)
        (output-edit-result nil (format nil "~a" c))))))

(defun insert-form/command ()
  (clingon:make-command
   :name "insert-form"
   :usage "(-f FILE | --code CODE) --insert CODE --line L --col C"
    :description "Insert code before a form, or at end of file"
   :long-description "Insert code at the given position. Validates both input ~
                      and result by default. Use --no-validate-input or ~
                      --no-validate-result to skip specific validations."
   :options (list
              (clingon:make-option :string :long-name "file" :short-name #\f
                                   :description "File to edit" :key :file)
              (clingon:make-option :string :long-name "source"
                                   :description "Source code to edit" :key :source)
              (clingon:make-option :integer :long-name "line" :short-name #\l
                                   :description "Line number" :key :line)
              (clingon:make-option :integer :long-name "col" :short-name #\c
                                   :description "Column number" :key :col)
               (clingon:make-option :string :long-name "insert" :short-name #\i
                                    :description "Code to insert" :key :insert-code)
               (make-write-option)
              (make-preview-option)
              (make-quiet-option)
              (make-recovery-option)
              (clingon:make-option :flag :long-name "no-validate-input"
                                   :description "Skip input code validation"
                                   :key :no-validate-input)
              (clingon:make-option :flag :long-name "no-validate-result"
                                   :description "Skip result validation"
                                   :key :no-validate-result))
    :handler #'insert/handler))

;;; ============================================================
;;; Append Form Command
;;; ============================================================

(defun append/handler (cmd)
  (with-edit-context (cmd :code-key :insert-code)
    (unless code
      (format *error-output* "Error: --insert is required~%")
      (clingon:exit 1))
    (when (and (not end) (not name) (not (and line col)))
      (format *error-output* "Error: --end, --name, or --line/--col required~%")
      (clingon:exit 1))
    (validate-new-code code no-validate-input)
    (handler-case
        (let ((result
                (cond
                  (end
                   (insert-form-end text code))
                  (name
                   (let ((node (find-top-level-by-name text name :recovery recovery)))
                     (unless node
                       (error "No top-level form named '~a'" name))
                     (let ((node-end (node-end node)))
                       (concatenate 'string
                                    (subseq text 0 node-end)
                                    (string #\Newline)
                                    code
                                    (subseq text node-end)))))
                  (t
                   (append-form-at text line col code :recovery recovery)))))
          (validate-edited-source result recovery no-validate-result)
          (deliver-edit-result result original-text file preview write quiet))
      (error (c)
        (output-edit-result nil (format nil "~a" c))))))

(defun append-form/command ()
  (clingon:make-command
   :name "append-form"
   :usage "(-f FILE | --code CODE) --insert CODE (--end | --name NAME | --line L --col C)"
   :description "Insert code after a form, at end of file, or after a named form"
   :long-description "Insert code after the form at the given position. ~
                      Use --end to append at end of file. ~
                      Use --name to append after a specific top-level form by name."
   :options (list
              (clingon:make-option :string :long-name "file" :short-name #\f
                                   :description "File to edit" :key :file)
              (clingon:make-option :string :long-name "source"
                                   :description "Source code to edit" :key :source)
              (clingon:make-option :integer :long-name "line" :short-name #\l
                                   :description "Line number" :key :line)
              (clingon:make-option :integer :long-name "col" :short-name #\c
                                   :description "Column number" :key :col)
              (clingon:make-option :string :long-name "name" :short-name #\n
                                   :description "Append after named form" :key :name)
              (clingon:make-option :flag :long-name "end"
                                   :description "Append at end of file" :key :end)
               (clingon:make-option :string :long-name "insert" :short-name #\i
                                    :description "Code to insert" :required t :key :insert-code)
               (make-write-option)
              (make-preview-option)
              (make-quiet-option)
              (make-recovery-option)
              (clingon:make-option :flag :long-name "no-validate-input"
                                   :description "Skip input code validation"
                                   :key :no-validate-input)
              (clingon:make-option :flag :long-name "no-validate-result"
                                   :description "Skip result validation"
                                   :key :no-validate-result))
   :handler #'append/handler))

;;; ============================================================
;;; Replace Command
;;; ============================================================

(defun replace-node-with-code (text node code pretty)
  "Replace NODE in TEXT with CODE, using pretty indentation when PRETTY is T."
  (if pretty
      (replace-form-pretty text node code)
      (let ((start (node-start node))
            (end (node-end node)))
        (concatenate 'string
                     (subseq text 0 start)
                     code
                     (subseq text end)))))

(defun replace/handler (cmd)
  (with-edit-context (cmd :code-key :replace-code)
    (unless code
      (format *error-output* "Error: --replace is required~%")
      (clingon:exit 1))
    (unless (or (and line col) index name end)
      (format *error-output* "Error: --end, --name, --index, or --line/--col required~%")
      (clingon:exit 1))
    (validate-new-code code no-validate-input)
    (handler-case
        (let ((result
                (cond
                  (end
                   (let* ((ast (parse-for-edit text recovery))
                          (node (first (last (list-top-level ast)))))
                     (unless node
                       (error "No top-level forms found"))
                     (replace-node-with-code text node code pretty)))
                  (name
                   (let ((node (find-top-level-by-name text name :recovery recovery)))
                     (unless node
                       (error "No top-level form named '~a'" name))
                     (replace-node-with-code text node code pretty)))
                  (index
                   (let* ((ast (parse-for-edit text recovery))
                          (node (nth index (list-top-level ast))))
                     (unless node
                       (error "Index ~a out of range" index))
                     (replace-node-with-code text node code pretty)))
                  (t
                   (let* ((ast (parse-for-edit text recovery))
                          (node (find-form-at ast text line col)))
                     (unless node
                       (error "No form found at line ~a, col ~a" line col))
                     (replace-node-with-code text node code pretty))))))
          (validate-edited-source result recovery no-validate-result)
          (deliver-edit-result result original-text file preview write quiet))
      (error (c)
        (output-edit-result nil (format nil "~a" c))))))

(defun replace-form/command ()
  (clingon:make-command
   :name "replace-form"
   :usage "(-f FILE | --code CODE) (--end | --name NAME | --index N | --line L --col C) --replace CODE [--pretty]"
   :description "Replace form at end of file, by name, by index, or by position"
   :long-description "Replace a form at the given position, by top-level index, by name, ~
                      or the last top-level form. ~
                      Use --pretty to preserve indentation. ~
                      Use --preview to see changes before applying. ~
                      Validates both input and result by default. ~
                      Use --no-validate-input or --no-validate-result to skip specific validations."
   :options (list
             (clingon:make-option :string :long-name "file" :short-name #\f
                                  :description "File to edit" :key :file)
             (clingon:make-option :string :long-name "source"
                                  :description "Source code to edit" :key :source)
             (clingon:make-option :integer :long-name "line" :short-name #\l
                                  :description "Line number" :key :line)
             (clingon:make-option :integer :long-name "col" :short-name #\c
                                  :description "Column number" :key :col)
             (clingon:make-option :integer :long-name "index"
                                  :description "Top-level form index (0-based)" :key :index)
             (clingon:make-option :string :long-name "name" :short-name #\n
                                  :description "Top-level form name" :key :name)
             (clingon:make-option :flag :long-name "end"
                                  :description "Replace the last top-level form" :key :end)
              (clingon:make-option :string :long-name "replace" :short-name #\r
                                   :description "Replacement code" :required t :key :replace-code)
             (clingon:make-option :flag :long-name "pretty"
                                  :description "Preserve indentation of replaced form"
                                  :key :pretty)
             (make-write-option)
             (make-preview-option)
             (make-quiet-option)
             (make-recovery-option)
             (clingon:make-option :flag :long-name "no-validate-input"
                                  :description "Skip input code validation"
                                  :key :no-validate-input)
             (clingon:make-option :flag :long-name "no-validate-result"
                                  :description "Skip result validation"
                                  :key :no-validate-result))
   :handler #'replace/handler))

;;; ============================================================
;;; Move Command
;;; ============================================================

(defun move/handler (cmd)
  (with-edit-context (cmd)
    (let ((from-line (clingon:getopt cmd :from-line))
          (from-col (clingon:getopt cmd :from-col))
          (to-line (clingon:getopt cmd :to-line))
          (to-col (clingon:getopt cmd :to-col)))
      (handler-case
          (let ((result (move-form text from-line from-col to-line to-col
                                   :recovery recovery)))
            (deliver-edit-result result original-text file preview write quiet))
        (error (c)
          (output-edit-result nil (format nil "~a" c)))))))

(defun move-form/command ()
  (clingon:make-command
   :name "move-form"
   :usage "(-f FILE | --code CODE) --from-line L1 --from-col C1 --to-line L2 --to-col C2"
   :description "Move form from (L1,C1) to after (L2,C2)"
   :options (list
             (clingon:make-option :string :long-name "file" :short-name #\f
                                  :description "File to edit" :key :file)
             (clingon:make-option :string :long-name "source"
                                  :description "Source code to edit" :key :source)
             (clingon:make-option :integer :long-name "from-line" :short-name #\l
                                  :description "Source line" :required t :key :from-line)
             (clingon:make-option :integer :long-name "from-col" :short-name #\a
                                  :description "Source col" :required t :key :from-col)
             (clingon:make-option :integer :long-name "to-line" :short-name #\m
                                  :description "Dest line" :required t :key :to-line)
             (clingon:make-option :integer :long-name "to-col" :short-name #\b
                                  :description "Dest col" :required t :key :to-col)
              (make-write-option)
              (make-preview-option)
              (make-quiet-option)
              (make-recovery-option)
              (clingon:make-option :flag :long-name "no-validate-input"
                                   :description "Skip input code validation"
                                   :key :no-validate-input)
              (clingon:make-option :flag :long-name "no-validate-result"
                                   :description "Skip result validation"
                                   :key :no-validate-result))
   :handler #'move/handler))

;;; ============================================================
;;; Batch Replace Command
;;; ============================================================

(defun batch-replace/handler (cmd)
  (with-edit-context (cmd)
    (let ((edits-json (clingon:getopt cmd :edits)))
      (unless edits-json
        (format *error-output* "Error: --edits is required~%")
        (clingon:exit 1))
      (handler-case
          (let* ((edits-list (cl-json:decode-json-from-string edits-json))
                 (edit-plists (mapcar (lambda (e)
                                        (let ((op-str (cdr (assoc :operation e)))
                                              (code (cdr (assoc :code e)))
                                              (index-val (cdr (assoc :index e)))
                                              (line-val (cdr (assoc :line e)))
                                              (col-val (cdr (assoc :col e))))
                                          (list :operation (if op-str
                                                               (intern (string-upcase op-str) :keyword)
                                                               :replace-index)
                                                :code code
                                                :index index-val
                                                :line line-val
                                                :col col-val
                                                :pretty pretty)))
                                      edits-list))
                 (result (apply-batch-edits text edit-plists :recovery recovery)))
            (validate-edited-source result recovery no-validate-result)
            (deliver-edit-result result original-text file preview write quiet))
        (error (c)
          (format *error-output* "Error: ~a~%" c)
          (clingon:exit 1))))))

(defun batch-replace/command ()
  (clingon:make-command
   :name "batch-replace"
   :usage "(-f FILE | --code CODE) --edits JSON [--pretty]"
   :description "Apply multiple edits in one command"
   :long-description "Apply a batch of edits from a JSON array. Each edit specifies ~
                      an :operation (:replace-index, :replace-position, :delete-index, ~
                      or :insert-after-index), :code, and position (:index or :line/:col). ~
                      Edits are applied sequentially."
   :options (list
             (clingon:make-option :string :long-name "file" :short-name #\f
                                  :description "File to edit" :key :file)
             (clingon:make-option :string :long-name "code"
                                  :description "Inline code" :key :code)
             (clingon:make-option :string :long-name "edits" :short-name #\e
                                  :description "JSON array of edits" :required t :key :edits)
             (clingon:make-option :flag :long-name "pretty"
                                  :description "Preserve indentation for all replacements"
                                  :key :pretty)
              (make-write-option)
              (make-preview-option)
              (make-quiet-option)
              (make-recovery-option)
              (clingon:make-option :flag :long-name "no-validate-input"
                                   :description "Skip input code validation"
                                   :key :no-validate-input)
              (clingon:make-option :flag :long-name "no-validate-result"
                                   :description "Skip result validation"
                                   :key :no-validate-result))
   :handler #'batch-replace/handler))

;;; ============================================================
;;; Help / Version Subcommands
;;; ============================================================

(defun version/handler (cmd)
  (declare (ignore cmd))
  (format *standard-output* "cl-toolkit 0.0.1.0~%"))

(defun version/command ()
  (clingon:make-command
   :name "version"
   :usage ""
   :description "Print version and exit"
   :handler #'version/handler))

(defun help/handler (cmd)
  (clingon:print-usage-and-exit cmd t))

(defun help/command ()
  (clingon:make-command
   :name "help"
   :usage "[COMMAND]"
   :description "Show help for a command"
   :handler #'help/handler))

;;; ============================================================
;;; Top-Level Command Tree
;;; ============================================================

(defun cl-toolkit/top-level/command ()
  "Returns the top-level cl-toolkit command."
  (clingon:make-command
   :name "cl-toolkit"
   :version "0.0.1.0"
   :description "Lisp code parser for structural analysis and editing"
   :long-description "A CLI tool for parsing, querying, and editing Lisp source code ~
                      using structural AST operations. Supports standard and ~
                      error-recovery parsing modes."
   :authors '("cl-agent-validate")
   :license "MIT"
   :handler (lambda (cmd) (clingon:print-usage-and-exit cmd t))
   :sub-commands (list
                   (parse/command)
                   (find/command)
                   (extract/command)
                   (validate/command)
                   (top-level/command)
                   (balance/command)
                   (format/command)
                   (delete-form/command)
                   (insert-form/command)
                   (append-form/command)
                    (replace-form/command)
                    (batch-replace/command)
                    (insert-at/command)
                   (move-form/command)
                   (help/command)
                   (version/command))))

;;; ============================================================
;;; Entry Points
;;; ============================================================

(defun main ()
  "CLI entry point for interactive use."
  (let ((app (cl-toolkit/top-level/command)))
    (clingon:run app)))

(defun cl-toolkit-main ()
  "Entry point for ASDF program-op."
  (let ((app (cl-toolkit/top-level/command)))
    (if (null (uiop:command-line-arguments))
        (clingon:print-usage-and-exit app t)
        (clingon:run app))))
