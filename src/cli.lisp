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

(defun output-json (node)
  "Write node as JSON to stdout."
  (cl-toolkit-ast::node-to-json node *standard-output*)
  (terpri))

(defun output-edit-result (text &optional error-msg)
  "Output a JSON edit result."
  (if error-msg
      (format *standard-output* "{\"success\":false,\"error\":\"~a\"}~%"
              (cl-toolkit-ast::escape-json-string error-msg))
      (format *standard-output* "{\"success\":true,\"source\":\"~a\"}~%"
              (cl-toolkit-ast::escape-json-string text))))

(defun write-result-to-file (file result)
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
      (format *error-output* "Wrote ~a (backup: ~a)~%" file bak))))

;;; ============================================================
;;; Shared Options
;;; ============================================================

(defun make-recovery-option ()
  "Create the --recovery option."
  (clingon:make-option :flag
                       :long-name "recovery"
                       :description "Use error recovery parser"
                       :key :recovery))

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
         (file (clingon:getopt cmd :file))
         (code (clingon:getopt cmd :code))
         (parser (if recovery
                     #'cl-toolkit-grammar::parse-with-recovery
                     #'cl-toolkit-grammar::parse-lisp-source)))
    (cond
      (code
       (let ((ast (funcall parser code)))
         (output-json ast)))
      (file
       (let* ((text (read-file-to-string file))
              (ast (funcall parser text)))
         (setf (getf ast :source) file)
         (output-json ast)))
      (t
       (let* ((text (with-output-to-string (out)
                      (loop for line = (read-line *standard-input* nil nil)
                            while line
                            do (write-string line out)
                               (terpri out))))
              (ast (funcall parser text)))
         (output-json ast))))))

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
  (let* ((file (clingon:getopt cmd :file))
         (line (clingon:getopt cmd :line))
         (col (clingon:getopt cmd :col)))
    (unless file
      (format *error-output* "Error: --file is required~%")
      (clingon:exit 1))
    (let* ((text (read-file-to-string file))
           (ast (cl-toolkit-grammar::parse-lisp-source text))
           (found (find-form-at ast text line col)))
      (if found
          (output-json found)
          (progn
            (format *error-output* "No form found at line ~a, col ~a~%" line col)
            (clingon:exit 1))))))

(defun find/command ()
  (clingon:make-command
   :name "find"
   :usage "--file FILE --line LINE --col COL"
   :description "Find form at position and output as JSON"
   :options (list
             (clingon:make-option :string
                                  :long-name "file"
                                  :short-name #\f
                                  :description "File to search"
                                  :required t
                                  :key :file)
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
  (let* ((file (clingon:getopt cmd :file))
         (line1 (clingon:getopt cmd :line1))
         (col1 (clingon:getopt cmd :col1))
         (line2 (clingon:getopt cmd :line2))
         (col2 (clingon:getopt cmd :col2)))
    (unless file
      (format *error-output* "Error: --file is required~%")
      (clingon:exit 1))
    (let* ((text (read-file-to-string file))
           (ast (cl-toolkit-grammar::parse-lisp-source text))
           (forms (extract-range ast text line1 col1 line2 col2)))
      (format *standard-output* "[")
      (loop for form in forms
            for i from 0
            do (unless (zerop i) (format *standard-output* ","))
               (cl-toolkit-ast::node-to-json form *standard-output*))
      (format *standard-output* "]~%"))))

(defun extract/command ()
  (clingon:make-command
   :name "extract"
   :usage "--file FILE --line1 L1 --col1 C1 --line2 L2 --col2 C2"
   :description "Extract forms in range as JSON array"
   :options (list
             (clingon:make-option :string :long-name "file" :short-name #\f
                                  :description "File" :required t :key :file)
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
  (let ((file (clingon:getopt cmd :file))
        (recovery (clingon:getopt cmd :recovery-flag)))
    (unless file
      (format *error-output* "Error: --file is required~%")
      (clingon:exit 1))
    (let* ((text (read-file-to-string file))
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
      (format *standard-output* "]~%}~%"))))

(defun validate/command ()
  (clingon:make-command
   :name "validate"
   :usage "--file FILE"
   :description "Validate file and report errors/warnings as JSON"
   :options (list
             (clingon:make-option :string :long-name "file" :short-name #\f
                                  :description "File to validate" :required t :key :file)
             (clingon:make-option :flag :long-name "recovery"
                                  :description "Use error recovery parser" :key :recovery-flag))
   :handler #'validate/handler))

;;; ============================================================
;;; Top-Level Command
;;; ============================================================

(defun top-level/handler (cmd)
  (let ((file (clingon:getopt cmd :file)))
    (unless file
      (format *error-output* "Error: --file is required~%")
      (clingon:exit 1))
    (let* ((text (read-file-to-string file))
           (ast (cl-toolkit-grammar::parse-lisp-source text))
           (forms (list-top-level ast)))
      (format *standard-output* "[")
      (loop for form in forms
            for i from 0
            do (unless (zerop i) (format *standard-output* ","))
               (cl-toolkit-ast::node-to-json form *standard-output*))
      (format *standard-output* "]~%"))))

(defun top-level/command ()
  (clingon:make-command
   :name "top-level"
   :usage "--file FILE"
   :description "List top-level forms as JSON array"
   :options (list
             (clingon:make-option :string :long-name "file" :short-name #\f
                                  :description "File to list" :required t :key :file))
   :handler #'top-level/handler))

;;; ============================================================
;;; Balance Command
;;; ============================================================

(defun balance/handler (cmd)
  (let* ((file (clingon:getopt cmd :file))
         (code (clingon:getopt cmd :code))
         (text (cond
                 (code code)
                 (file (read-file-to-string file))
                 (t (format *error-output* "Error: --file or --code required~%")
                    (clingon:exit 1)))))
    (let ((result (analyze-balance text)))
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
      (format *standard-output* "}~%"))))

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
  (let* ((file (clingon:getopt cmd :file))
         (code (clingon:getopt cmd :code))
         (indent (clingon:getopt cmd :indent))
         (write (clingon:getopt cmd :write))
         (text (cond
                 (code code)
                 (file (read-file-to-string file))
                 (t (format *error-output* "Error: --file or --code required~%")
                    (clingon:exit 1)))))
    (let ((formatted (format-source text :indent indent)))
      (if write
          (progn
            (write-result-to-file file formatted)
            (output-edit-result formatted))
          (output-edit-result formatted)))))

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
             (make-write-option))
   :handler #'format/handler))

;;; ============================================================
;;; Delete Command
;;; ============================================================

(defun delete/handler (cmd)
  (let* ((file (clingon:getopt cmd :file))
         (line (clingon:getopt cmd :line))
         (col (clingon:getopt cmd :col))
         (index (clingon:getopt cmd :index))
         (write (clingon:getopt cmd :write))
         (recovery (clingon:getopt cmd :recovery)))
    (unless file
      (format *error-output* "Error: --file is required~%")
      (clingon:exit 1))
    (let ((text (read-file-to-string file)))
      (handler-case
          (let ((result
                  (cond
                    (index
                     (delete-top-level-at text index :recovery recovery))
                    ((and line col)
                     (delete-form-at text line col :recovery recovery))
                    (t
                     (format *error-output* "Error: --line/--col or --index required~%")
                     (clingon:exit 1)))))
            (when write
              (write-result-to-file file result))
            (output-edit-result result))
        (error (c)
          (output-edit-result nil (format nil "~a" c)))))))

(defun delete/command ()
  (clingon:make-command
   :name "delete"
   :usage "--file FILE (--line L --col C | --index N)"
   :description "Delete form at position or by index"
   :long-description "Delete a form from the source file. ~
                      Use --line/--col to delete by position, or --index to delete ~
                      the N-th top-level form."
   :options (list
             (clingon:make-option :string :long-name "file" :short-name #\f
                                  :description "File to edit" :required t :key :file)
             (clingon:make-option :integer :long-name "line" :short-name #\l
                                  :description "Line number" :key :line)
             (clingon:make-option :integer :long-name "col" :short-name #\c
                                  :description "Column number" :key :col)
             (clingon:make-option :integer :long-name "index"
                                  :description "Top-level form index (0-based)" :key :index)
             (make-write-option)
             (make-recovery-option))
   :handler #'delete/handler))

;;; ============================================================
;;; Insert Command
;;; ============================================================

(defun insert/handler (cmd)
  (let* ((file (clingon:getopt cmd :file))
         (line (clingon:getopt cmd :line))
         (col (clingon:getopt cmd :col))
         (code (clingon:getopt cmd :code))
         (at-end (clingon:getopt cmd :at-end))
         (after (clingon:getopt cmd :after))
         (write (clingon:getopt cmd :write))
         (recovery (clingon:getopt cmd :recovery))
         (validate (clingon:getopt cmd :validate-flag)))
    (unless file
      (format *error-output* "Error: --file is required~%")
      (clingon:exit 1))
    (let ((text (read-file-to-string file)))
      (handler-case
          (let ((result
                  (cond
                    (at-end
                     (insert-form-end text code :validate validate))
                    ((and line col code)
                     (insert-form-at text line col code
                                     :after after :recovery recovery))
                    (t
                     (format *error-output* "Error: --line/--col/--code or --at-end required~%")
                     (clingon:exit 1)))))
            (when write
              (write-result-to-file file result))
            (output-edit-result result))
        (error (c)
          (output-edit-result nil (format nil "~a" c)))))))

(defun insert/command ()
  (clingon:make-command
   :name "insert"
   :usage "--file FILE --code CODE (--line L --col C | --at-end)"
   :description "Insert code before/after a form, or at end of file"
   :options (list
             (clingon:make-option :string :long-name "file" :short-name #\f
                                  :description "File to edit" :required t :key :file)
             (clingon:make-option :integer :long-name "line" :short-name #\l
                                  :description "Line number" :key :line)
             (clingon:make-option :integer :long-name "col" :short-name #\c
                                  :description "Column number" :key :col)
             (clingon:make-option :string :long-name "code" :short-name #\C
                                  :description "Code to insert" :key :code)
             (clingon:make-option :flag :long-name "at-end"
                                  :description "Insert at end of file" :key :at-end)
             (clingon:make-option :flag :long-name "after"
                                  :description "Insert after the form (default: before)" :key :after)
             (make-write-option)
             (make-recovery-option)
             (clingon:make-option :flag :long-name "validate"
                                  :description "Validate the inserted code"
                                  :key :validate-flag))
   :handler #'insert/handler))

;;; ============================================================
;;; Replace Command
;;; ============================================================

(defun replace/handler (cmd)
  (let* ((file (clingon:getopt cmd :file))
         (line (clingon:getopt cmd :line))
         (col (clingon:getopt cmd :col))
         (code (clingon:getopt cmd :code))
         (write (clingon:getopt cmd :write))
         (recovery (clingon:getopt cmd :recovery)))
    (unless (and file line col code)
      (format *error-output* "Error: --file, --line, --col, and --code are required~%")
      (clingon:exit 1))
    (let ((text (read-file-to-string file)))
      (handler-case
          (let ((result (replace-form-at text line col code :recovery recovery)))
            (when write
              (write-result-to-file file result))
            (output-edit-result result))
        (error (c)
          (output-edit-result nil (format nil "~a" c)))))))

(defun replace/command ()
  (clingon:make-command
   :name "replace"
   :usage "--file FILE --line L --col C --code CODE"
   :description "Replace form at position with new code"
   :options (list
             (clingon:make-option :string :long-name "file" :short-name #\f
                                  :description "File to edit" :required t :key :file)
             (clingon:make-option :integer :long-name "line" :short-name #\l
                                  :description "Line number" :required t :key :line)
             (clingon:make-option :integer :long-name "col" :short-name #\c
                                  :description "Column number" :required t :key :col)
             (clingon:make-option :string :long-name "code" :short-name #\C
                                  :description "Replacement code" :required t :key :code)
             (make-write-option)
             (make-recovery-option))
   :handler #'replace/handler))

;;; ============================================================
;;; Move Command
;;; ============================================================

(defun move/handler (cmd)
  (let* ((file (clingon:getopt cmd :file))
         (from-line (clingon:getopt cmd :from-line))
         (from-col (clingon:getopt cmd :from-col))
         (to-line (clingon:getopt cmd :to-line))
         (to-col (clingon:getopt cmd :to-col))
         (write (clingon:getopt cmd :write))
         (recovery (clingon:getopt cmd :recovery)))
    (unless file
      (format *error-output* "Error: --file is required~%")
      (clingon:exit 1))
    (let ((text (read-file-to-string file)))
      (handler-case
          (let ((result (move-form text from-line from-col to-line to-col
                                   :recovery recovery)))
            (if write
                (write-result-to-file file result)
                (output-edit-result result)))
        (error (c)
          (output-edit-result nil (format nil "~a" c)))))))

(defun move/command ()
  (clingon:make-command
   :name "move"
   :usage "--file FILE --from-line L1 --from-col C1 --to-line L2 --to-col C2"
   :description "Move form from (L1,C1) to after (L2,C2)"
   :options (list
             (clingon:make-option :string :long-name "file" :short-name #\f
                                  :description "File to edit" :required t :key :file)
             (clingon:make-option :integer :long-name "from-line" :short-name #\l
                                  :description "Source line" :required t :key :from-line)
             (clingon:make-option :integer :long-name "from-col" :short-name #\a
                                  :description "Source col" :required t :key :from-col)
             (clingon:make-option :integer :long-name "to-line" :short-name #\m
                                  :description "Dest line" :required t :key :to-line)
             (clingon:make-option :integer :long-name "to-col" :short-name #\b
                                  :description "Dest col" :required t :key :to-col)
             (make-write-option)
             (make-recovery-option))
   :handler #'move/handler))

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
                  (delete/command)
                  (insert/command)
                  (replace/command)
                  (move/command)
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
