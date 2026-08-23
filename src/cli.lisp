(in-package #:cl-toolkit)

;;; ============================================================
;;; CLI Utilities
;;; ============================================================

(defun read-file-to-string (path)
  "Read entire file into a string.
   Handles named pipes / process substitution (<(...)): those report a
   bogus FILE-LENGTH of zero or signal, so fall back to reading until
   EOF instead of preallocating."
  (with-open-file (stream path :direction :input :if-does-not-exist nil)
    (unless stream
      (format *error-output* "Cannot read file: ~a~%" path)
      (clingon:exit 1))
    (let ((len (ignore-errors (file-length stream))))
      (if (and len (plusp len))
          (let ((content (make-string len)))
            (read-sequence content stream)
            content)
          ;; pipes, FIFOs, /dev/fd/N — read to EOF
          (with-output-to-string (out)
            (let ((buf (make-string 4096)))
              (loop for n = (read-sequence buf stream)
                    do (write-sequence buf out :end n)
                    while (= n (length buf)))))))))

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
  "Output a JSON edit result.
   Failure reports EXIT 1 — consumers can trust the exit code instead of
   grepping output for \"error\" (diffs legitimately contain that word)."
  (if error-msg
      (progn
        (format *standard-output* "{\"success\":false,\"error\":\"~a\"}~%"
                (cl-toolkit-ast::escape-json-string error-msg))
        ;; NOTE: clingon:exit unwinds via handler; flush first.
        (finish-output *standard-output*)
        (clingon:exit 1))
      (format *standard-output* "{\"success\":true,\"source\":\"~a\"}~%"
              (cl-toolkit-ast::escape-json-string text))))

(defun resolve-file-path (file)
  "Normalize FILE to an absolute path against the current working directory.
   Prevents cwd-dependent bugs when relative paths are passed between
   invocations or used for backup naming."
  (when file
    (namestring (merge-pathnames file))))

;;; Backup policy for --write. Defaults keep the single rolling FILE.bak.
(defvar *no-backup* nil)
(defvar *backup-dir* nil)

(defun backup-path-for (file)
  "Return the rolling backup pathname for FILE."
  (concatenate 'string file ".bak"))

(defun timestamped-backup-path (file dir)
  "Return DIR/STEM.YYYYMMDD-HHMMSS.bak for FILE."
  (let* ((dir-str (namestring dir))
         (dir-str (if (char= (char dir-str (1- (length dir-str))) #\/)
                      dir-str
                      (concatenate 'string dir-str "/")))
         (name (pathname-name file))
         (type (pathname-type file))
         (stamp (multiple-value-bind (s mi h d mo y)
                    (get-decoded-time)
                  (format nil "~4d~2,'0d~2,'0d-~2,'0d~2,'0d~2,'0d" y mo d h mi s))))
    (concatenate 'string
                 dir-str
                 name
                 "."
                 stamp
                 (when type ".")
                 (or type "lisp")
                 ".bak")))

(defun write-result-to-file (file result &optional quiet)
  "Write RESULT source to FILE. Backup behavior per policy:
   default = rolling FILE.bak; --backup-dir adds a timestamped snapshot;
   --no-backup skips backups entirely.
   Refuses to write when FILE does not exist — --write edits, never creates."
  (let ((source (if (stringp result)
                    result
                    (getf result :source))))
    (unless source
      (error "No source in result"))
    (unless (probe-file file)
      (error "Cannot write: file does not exist: ~a" file))
    ;; Timestamped snapshot captures the PREVIOUS content, before rename.
    (when *backup-dir*
      (ensure-directories-exist *backup-dir*)
      (let ((snapshot (timestamped-backup-path file *backup-dir*)))
        (with-open-file (in file :direction :input :element-type '(unsigned-byte 8))
          (with-open-file (out snapshot :direction :output
                                        :if-exists :supersede
                                        :element-type '(unsigned-byte 8))
            (loop for byte = (read-byte in nil nil)
                  while byte
                  do (write-byte byte out))))))
    (let ((bak (backup-path-for file)))
      (cond (*no-backup*)
            (t
             (when (probe-file bak)
               (delete-file bak))
             (rename-file file bak)))
      (with-open-file (stream file :direction :output
                                   :if-exists :supersede
                                   :if-does-not-exist :create)
        (write-string source stream))
      (unless quiet
        (cond (*no-backup*
               (format *error-output* "Wrote ~a (no backup)~%" file))
              (*backup-dir*
               (format *error-output* "Wrote ~a (backup: ~a, snapshot in ~a)~%"
                       file bak *backup-dir*))
              (t
               (format *error-output* "Wrote ~a (backup: ~a)~%" file bak)))))))

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
     (when file
       (let ((old-l (count #\Newline original-text))
             (new-l (count #\Newline result)))
         (if (string= original-text result)
             (format *error-output* "Preview stats: ~a -> ~a lines (no changes)~%"
                     old-l new-l)
             (format *error-output* "Preview stats: ~a -> ~a lines (~@a bytes)~%"
                     old-l new-l (- (length result) (length original-text))))))
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
          ,@(when code-key
              `((raw-code (clingon:getopt ,cmd ,code-key))
                ;; Code input modes: value "-" reads stdin, --code-file
                ;; reads from a file — sidesteps shell quoting (#' etc).
                (code-file (clingon:getopt ,cmd :code-file))
                (code (cond
                        ((and raw-code (string= raw-code "-")) (read-stdin))
                        ((and (null raw-code) code-file)
                         (read-file-to-string (resolve-file-path code-file)))
                        (t raw-code)))))
          (write (clingon:getopt ,cmd :write))
          (preview (clingon:getopt ,cmd :preview))
          (quiet (clingon:getopt ,cmd :quiet))
          (recovery (clingon:getopt ,cmd :recovery))
          (no-validate-input (clingon:getopt ,cmd :no-validate-input))
          (no-validate-result (clingon:getopt ,cmd :no-validate-result))
          (match (clingon:getopt ,cmd :match))
          (nearest (clingon:getopt ,cmd :nearest))
          (contains-arg (clingon:getopt ,cmd :contains))
          (match-exact (clingon:getopt ,cmd :match-exact))
          (allow-multi-forms (clingon:getopt ,cmd :allow-multi-forms))
          (backup-dir (clingon:getopt ,cmd :backup-dir))
          (no-backup (clingon:getopt ,cmd :no-backup))
          (file (resolve-file-path (clingon:getopt ,cmd :file)))
          (text (read-input ,cmd))
          (original-text (when file (read-file-to-string file))))
     ;; backup policy for this invocation
     (let ((*no-backup* no-backup)
           (*backup-dir* (when backup-dir (resolve-file-path backup-dir))))
       (declare (special *no-backup* *backup-dir*))
       ,@body)))

(defun single-line-preview (text node &optional (max-chars 60))
  "First MAX-CHARS of NODE's source, newlines/tabs collapsed to spaces."
  (let* ((src (node-source-text text node))
         (flat (with-output-to-string (out)
                 (loop for ch across src
                       do (write-char (if (member ch '(#\Newline #\Tab #\Return))
                                          #\Space
                                          ch)
                                      out))))
         (trimmed (string-left-trim " " flat)))
    (if (> (length trimmed) max-chars)
        (concatenate 'string (subseq trimmed 0 max-chars) "...")
        trimmed)))

(defun unique-containing-top-level (text snippet recovery)
  "Return the single top-level form whose source contains SNIPPET.
   Errors on zero or multiple matches — ambiguity must be resolved by
   refining the snippet or switching to an explicit --index."
  (let ((pairs (find-forms-containing text snippet :recovery recovery)))
    (case (length pairs)
      (0 (error "No top-level form contains ~s" snippet))
      (1 (cdr (first pairs)))
      (t (error "Ambiguous target: ~a top-level forms contain ~s (indices ~{~a~^, ~}) -- refine the snippet or use --index"
                (length pairs) snippet (mapcar #'car pairs))))))

(defun notify-target (verb node text)
  "Report to stderr which form VERB targets: name (when known), resolved
   line/col, and a source preview. Printed even under --quiet: anonymous
   siblings look identical by name alone, and this announcement is the
   last line of defense against wrong-form writes."
  (multiple-value-bind (line col)
      (cl-toolkit-ast::offset-to-line-col text (node-start node))
    (let ((form-name (node-form-name node))
          (preview (single-line-preview text node)))
      (format *error-output* "~a form ~a [line ~a, col ~a] ~s~%"
              verb
              (if form-name (format nil "'~a'" form-name) "<anonymous>")
              line col
              preview))))

;;; ============================================================
;;; Shared Options
;;; ============================================================

(defun make-nearest-option ()
  (clingon:make-option :flag
                       :long-name "nearest"
                       :description "Allow nearest-match position resolution (destructive ops are exact by default)"
                       :key :nearest))

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
        ;; Enrich with resolved 0-based line/col so callers can verify
        ;; that the position they asked for maps to the form they expect.
        (multiple-value-bind (l c)
            (cl-toolkit-ast::offset-to-line-col text (node-start found))
          (let ((copy (copy-list found)))
            (setf (getf copy :line) l
                  (getf copy :col) c)
            (output-json copy)))
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
         (preview-chars (clingon:getopt cmd :preview-chars))
         (text (read-input cmd))
         (ast (cl-toolkit-grammar::parse-lisp-source text))
         (forms (list-top-level ast)))
    (if names
        ;; Concise name listing with indices.
        ;; Line numbers are 0-based — identical semantics as --line args,
        ;; so displayed values can be passed back verbatim. --preview-chars
        ;; appends a source excerpt so look-alike siblings are tellable.
        (loop for form in forms
              for i from 0
              do (multiple-value-bind (line col)
                      (cl-toolkit-ast:offset-to-line-col text (cl-toolkit-ast:node-start form))
                    (let ((name (cl-toolkit-ast:node-form-name form)))
                      (format *standard-output* "~a: ~a  [line ~a, col ~a]~@[  ~s~]~%"
                              i (or name "?") line col
                              (when (and preview-chars (> preview-chars 0))
                                (single-line-preview text form preview-chars))))))
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
   :description "List top-level forms as JSON array, or with --names show form names with indices (line/col are 0-based, LSP-style)"
   :options (list
             (clingon:make-option :string :long-name "file" :short-name #\f
                                  :description "File to list" :key :file)
             (clingon:make-option :string :long-name "code"
                                  :description "Inline code" :key :code)
             (clingon:make-option :integer :long-name "preview-chars"
                                  :description "With --names: append first N chars of each form's source"
                                  :key :preview-chars)
             (clingon:make-option :flag :long-name "names"
                                  :description "List form names with indices"
                                  :key :names))
   :handler #'top-level/handler))

;;; ============================================================
;;; Balance Command
;;; ============================================================

(defun balance/handler (cmd)
  (let* ((text (read-input cmd))
         (expect-delta (clingon:getopt cmd :expect-delta))
         (result (analyze-balance text)))
    ;; --expect-delta N: assert the fragment's net depth contribution.
    ;; A balanced whole file has delta 0; a wrap/insertion fragment
    ;; fragment carries its own nonzero delta (e.g. +1 for one opener).
    (when expect-delta
      (unless (= (getf result :final-depth) expect-delta)
        (format *error-output*
                "Depth check failed: final depth ~a but --expect-delta ~a~%"
                (getf result :final-depth) expect-delta)
        (clingon:exit 1)))
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
                                  :description "Inline code to analyze" :key :code)
             (clingon:make-option :integer :long-name "expect-delta"
                                  :description "Fail unless the text's net depth contribution equals N (fragment wrap checks)"
                                  :key :expect-delta))
   :handler #'balance/handler))

;;; ============================================================
;;; Format Command
;;; ============================================================

(defun format/handler (cmd)
  (with-edit-context (cmd)
    (let ((canonical (clingon:getopt cmd :canonical))
          (indent (clingon:getopt cmd :indent)))
      (handler-case
          (let ((formatted
                  (if canonical
                      (format-source text :indent indent)
                      (format-minimal text :recovery recovery))))
            (when (and (not canonical) (not quiet))
              (format *error-output* "Minimal format (jams + broken-indentation only); pass --canonical for whole-file restyle~%"))
            (deliver-edit-result formatted original-text file preview write quiet))
        (error (c)
          (output-edit-result nil (format nil "~a" c)))))))

(defun format/command ()
  (clingon:make-command
   :name "format"
   :usage "--file FILE | --code CODE"
   :description "Reformat source with consistent indentation"
   :options (list
             (clingon:make-option :flag :long-name "canonical"
                                  :description "Whole-file restyle (default is minimal: jams + broken indentation)"
                                  :key :canonical)
             (clingon:make-option :string :long-name "file" :short-name #\f
                                  :description "File to format" :key :file)
             (clingon:make-option :string :long-name "code"
                                   :description "Inline code to format" :key :code)
              (clingon:make-option :string :long-name "indent"
                                   :description "Indentation string (default: two spaces)"
                                   :initial-value "  "
                                   :key :indent)
              (make-write-option)
(clingon:make-option :string :long-name "backup-dir"
                                   :description "Also save timestamped pre-edit snapshots here"
                                   :key :backup-dir)
              (clingon:make-option :flag :long-name "no-backup"
                                   :description "Skip the rolling .bak backup on write"
                                   :key :no-backup)
              (make-quiet-option))
    :handler #'format/handler))

;;; ============================================================
;;; Delete Command
;;; ============================================================

(defun delete/handler (cmd)
  (with-edit-context (cmd)
    (handler-case
        (let* (target-node
               (result
                (cond
                  (end
                   (let ((node (first (last (list-top-level (parse-for-edit text recovery))))))
                     (setf target-node node)
                     (delete-last-top-level text :recovery recovery)))
                  (name
                   (let ((node (find-top-level-by-name text name :recovery recovery)))
                     (unless node
                       (format *error-output* "Error: No top-level form named '~a'~%" name)
                       (clingon:exit 1))
                     (setf target-node node)
                     (delete-node-from-text text node)))
                  (index
                   (let ((node (top-level-node-at text index :recovery recovery)))
                     (setf target-node node)
                     (delete-node-from-text text node)))
                  (contains-arg
                   (let ((node (unique-containing-top-level text contains-arg recovery)))
                     (setf target-node node)
                     (delete-node-from-text text node)))
                  ((and line col)
                   (let ((ast (parse-for-edit text recovery)))
                     (setf target-node
                           (if nearest
                               (find-form-at ast text line col)
                               (find-form-starting-at ast text line col)))
                     (unless target-node
                       (if nearest
                           (error "No form found at line ~a, col ~a" line col)
                           (error "No form starts exactly at line ~a, col ~a -- pass --nearest for containment match" line col)))
                     (delete-node-from-text text target-node)))
                  (t
                   (format *error-output* "Error: --end, --name, --index, --contains, or --line/--col required~%")
                   (clingon:exit 1)))))
          (validate-edited-source result recovery no-validate-result)
          (when target-node
            (notify-target "Deleting" target-node text))
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
              (make-nearest-option)
              (clingon:make-option :string :long-name "contains"
                                   :description "Target the unique top-level form whose source contains this snippet"
                                   :key :contains)
              (make-write-option)
(clingon:make-option :string :long-name "backup-dir"
                                   :description "Also save timestamped pre-edit snapshots here"
                                   :key :backup-dir)
              (clingon:make-option :flag :long-name "no-backup"
                                   :description "Skip the rolling .bak backup on write"
                                   :key :no-backup)
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
                                  :description "Code to insert (or omit and use --code-file)" :key :insert-code)
             (clingon:make-option :string :long-name "code-file"
                                  :description "Read code from file (\"-\" on --insert reads stdin)"
             :key :code-file)
              (make-write-option)
(clingon:make-option :string :long-name "backup-dir"
                                   :description "Also save timestamped pre-edit snapshots here"
                                   :key :backup-dir)
              (clingon:make-option :flag :long-name "no-backup"
                                   :description "Skip the rolling .bak backup on write"
                                   :key :no-backup)
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
               (clingon:make-option :string :long-name "code-file"
                                    :description "Read code from file (\"-\" on --insert reads stdin)"
               :key :code-file)
               (make-write-option)
(clingon:make-option :string :long-name "backup-dir"
                                    :description "Also save timestamped pre-edit snapshots here"
                                    :key :backup-dir)
               (clingon:make-option :flag :long-name "no-backup"
                                    :description "Skip the rolling .bak backup on write"
                                    :key :no-backup)
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
        (let* (target-node
               (result
                (cond
                  (end
                   (let ((ast (parse-for-edit text recovery)))
                     (setf target-node (first (last (list-top-level ast))))
                     (insert-form-end text code)))
                  (name
                   (let ((node (find-top-level-by-name text name :recovery recovery)))
                     (unless node
                       (error "No top-level form named '~a'" name))
                     (setf target-node node)
                     (let ((node-end (node-end node)))
                       (concatenate 'string
                                    (subseq text 0 node-end)
                                    (string #\Newline)
                                    code
                                    (subseq text node-end)))))
                  (t
                   (append-form-at text line col code :recovery recovery)))))
          (validate-edited-source result recovery no-validate-result)
          (when target-node
            (notify-target "Appending after" target-node text))
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
                                    :description "Code to insert (or omit and use --code-file)" :key :insert-code)
               (clingon:make-option :string :long-name "code-file"
                                    :description "Read code from file (\"-\" on --insert reads stdin)"
               :key :code-file)
               (make-write-option)
(clingon:make-option :string :long-name "backup-dir"
                                    :description "Also save timestamped pre-edit snapshots here"
                                    :key :backup-dir)
               (clingon:make-option :flag :long-name "no-backup"
                                    :description "Skip the rolling .bak backup on write"
                                    :key :no-backup)
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

(defun resolve-replace-target (text node match &key match-exact)
  "Narrow NODE to its smallest descendant whose source matches MATCH.
   Returns (values node fuzzy-p); FUZZY-P is T when a contains-match was
   used. With a nil MATCH returns NODE unchanged.
   MATCH-EXACT forbids the contains fallback and fails with guidance
   instead of escalating silently."
  (if (null match)
      (values node nil)
      (let ((exact (find-subform-matching-exact node text match)))
        (cond
          (exact (values exact nil))
          (match-exact
           (error "No exact subform matching ~s inside target form (--match-exact forbids contains-fallback). Matching is literal source text: #'x will not match (function x)"
                  match))
          ((find-subform-matching node text match)
           (values (find-subform-matching node text match) t))
          (t
           (error "No subform matching ~s inside target form. Matching is literal source text: #'x will not match (function x)"
                  match))))))

(defun replace/handler (cmd)
  (with-edit-context (cmd :code-key :replace-code)
    (let ((delete-match (clingon:getopt cmd :delete-match)))
      ;; --delete-match removes the --match subform; replacement is empty.
      (when delete-match
        (unless match
          (format *error-output* "Error: --delete-match requires --match~%")
          (clingon:exit 1))
        (setf code ""))
    (unless code
      (format *error-output* "Error: --replace is required~%")
      (clingon:exit 1))
    (unless (or (and line col) index name end contains-arg)
      (format *error-output* "Error: --end, --name, --index, --contains, or --line/--col required~%")
      (clingon:exit 1))
    ;; Empty --replace with --match deletes the matched subform.
    (unless (or code (and match (null code)))
      (format *error-output* "Error: --replace is required~%")
      (clingon:exit 1))
    (when (and match (not (or name index end contains-arg)))
      (format *error-output* "Error: --match requires --name, --index, --contains, or --end~%")
      (clingon:exit 1))
    (validate-new-code code no-validate-input)
    (handler-case
        (let* ((base-node
                (cond
                  (end
                   (let ((node (first (last (list-top-level (parse-for-edit text recovery))))))
                     (unless node
                       (error "No top-level forms found"))
                     node))
                  (name
                   (or (find-top-level-by-name text name :recovery recovery)
                       (error "No top-level form named '~a'" name)))
                  (contains-arg
                   (unique-containing-top-level text contains-arg recovery))
                  (index
                   (let ((node (nth index (list-top-level (parse-for-edit text recovery)))))
                     (unless node
                       (error "Index ~a out of range" index))
                     node))
                  (t
                   (let ((ast (parse-for-edit text recovery)))
                     (or (if nearest
                             (find-form-at ast text line col)
                             (find-form-starting-at ast text line col))
                         (if nearest
                             (error "No form found at line ~a, col ~a" line col)
                             (error "No form starts exactly at line ~a, col ~a -- pass --nearest for containment match" line col)))))))
               (resolved (multiple-value-list
                          (resolve-replace-target text base-node match :match-exact match-exact)))
               (target-node (first resolved))
               (fuzzy-p (second resolved))
               (result (replace-node-with-code text target-node code pretty)))
          ;; Replacement-shape guard: replacing ONE top-level form with
          ;; several silently multiplies structure (the stray in-package x4
          ;; failure). Multi-form replacement stays available behind the flag.
          (let ((repl-count (length (ignore-errors
                                     (list-top-level (parse-for-edit code recovery))))))
            (when (and repl-count
                       (> repl-count 1)
                       (not allow-multi-forms)
                       ;; only whole-top-level targets carry the risk
                       (= (count-if (lambda (n) (= (node-start n) (node-start base-node)))
                                    (list-top-level (parse-for-edit text recovery)))
                          1))
              (error "Refusing: replacement contains ~a top-level forms but replaces one. ~
                      Pass --allow-multi-forms if splitting is intended."
                     repl-count)))
          (validate-edited-source result recovery no-validate-result)
          (when target-node
            (notify-target
           (cond ((and match (stringp code) (string= code "")) "Deleting in")
                 ((and match fuzzy-p) "Replacing in form (fuzzy contains-match)")
                 (match "Replacing in")
                 (t "Replacing"))
           target-node text))
          (deliver-edit-result result original-text file preview write quiet))
      (error (c)
        (output-edit-result nil (format nil "~a" c)))))))

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
              (make-nearest-option)
              (clingon:make-option :string :long-name "contains"
                                   :description "Target the unique top-level form whose source contains this snippet"
                                   :key :contains)
              (clingon:make-option :string :long-name "match"
                                   :description "Replace smallest subform matching this snippet inside the target form" :key :match)
              (clingon:make-option :string :long-name "code-file"
                                   :description "Read code from file instead of inline argument (\"-\" on the code arg reads stdin)"
                                   :key :code-file)
              (clingon:make-option :flag :long-name "match-exact"
                                   :description "--match must match exactly; never fall back to contains-match"
                                   :key :match-exact)
              (clingon:make-option :flag :long-name "allow-multi-forms"
                                   :description "Permit replacing one top-level form with several"
                                   :key :allow-multi-forms)
              (clingon:make-option :flag :long-name "delete-match"
                                   :description "With --match: remove the matched subform instead of replacing"
                                   :key :delete-match)
              (clingon:make-option :string :long-name "replace" :short-name #\r
                                   :description "Replacement code (omit when using --delete-match)" :key :replace-code)
             (clingon:make-option :flag :long-name "pretty"
                                  :description "Preserve indentation of replaced form"
                                  :key :pretty)
             (make-write-option)
(clingon:make-option :string :long-name "backup-dir"
                                  :description "Also save timestamped pre-edit snapshots here"
                                  :key :backup-dir)
             (clingon:make-option :flag :long-name "no-backup"
                                  :description "Skip the rolling .bak backup on write"
                                  :key :no-backup)
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
(clingon:make-option :string :long-name "backup-dir"
                                   :description "Also save timestamped pre-edit snapshots here"
                                   :key :backup-dir)
              (clingon:make-option :flag :long-name "no-backup"
                                   :description "Skip the rolling .bak backup on write"
                                   :key :no-backup)
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
                                              (name-val (cdr (assoc :name e)))
                                              (index-val (cdr (assoc :index e)))
                                              (line-val (cdr (assoc :line e)))
                                              (col-val (cdr (assoc :col e))))
                                          (list :operation (if op-str
                                                               (intern (string-upcase op-str) :keyword)
                                                               :replace-index)
                                                :code code
                                                :name name-val
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
                       an :operation, :code where applicable, and a target. ~%
                       Name-based: replace-name, delete-name, insert-after-name (:name key). ~
                       Index-based: replace-index, delete-index, insert-after-index (:index). ~
                       Position-based: replace-position (:line/:col). ~
                       Name edits run first, index edits highest-to-lowest (no shifting)."
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
(clingon:make-option :string :long-name "backup-dir"
                                   :description "Also save timestamped pre-edit snapshots here"
                                   :key :backup-dir)
              (clingon:make-option :flag :long-name "no-backup"
                                   :description "Skip the rolling .bak backup on write"
                                   :key :no-backup)
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
;;; Source-of Command (exact source text extraction)
;;; ============================================================

(defun source-of/handler (cmd)
  (let* ((name (clingon:getopt cmd :name))
         (index (clingon:getopt cmd :index))
         (end (clingon:getopt cmd :end))
         (recovery (clingon:getopt cmd :recovery))
         (text (read-input cmd)))
    (unless (or name end index)
      (format *error-output* "Error: --name, --index, or --end required~%")
      (clingon:exit 1))
    (let ((source (source-of-top-level text
                                       :name name :index index :end end
                                       :recovery recovery)))
      (if source
          (format *standard-output* "~a" source)
          (progn
            (format *error-output* "Error: no matching top-level form~%")
            (clingon:exit 1))))))

(defun source-of/command ()
  (clingon:make-command
   :name "source-of"
   :usage "(-f FILE | --code CODE) (--name NAME | --index N | --end)"
   :description "Print the exact source text of a top-level form"
   :long-description "Extracts the verbatim source of one top-level form —
   safe read step for read-modify-write of large forms:
     source-of --name dispatch-token > /tmp/form.lisp
     # edit /tmp/form.lisp, then:
     replace-form --file X --name dispatch-token --replace \"$(cat /tmp/form.lisp)\")"
   :options (list
             (clingon:make-option :string :long-name "file" :short-name #\f
                                  :description "File to read" :key :file)
             (clingon:make-option :string :long-name "code"
                                  :description "Inline code" :key :code)
             (clingon:make-option :string :long-name "name" :short-name #\n
                                  :description "Top-level form name" :key :name)
             (clingon:make-option :integer :long-name "index"
                                  :description "Top-level form index" :key :index)
             (clingon:make-option :flag :long-name "end"
                                  :description "Last top-level form" :key :end)
             (make-recovery-option))
   :handler #'source-of/handler))

;;; ============================================================
;;; Find-forms Command (structural content search)
;;; ============================================================

(defun find-forms/handler (cmd)
  (let* ((contains (clingon:getopt cmd :contains))
         (with-source (clingon:getopt cmd :with-source))
         (recovery (clingon:getopt cmd :recovery))
         (text (read-input cmd)))
    (unless contains
      (format *error-output* "Error: --contains is required~%")
      (clingon:exit 1))
    (format *standard-output* "[")
    (let ((first t))
      (dolist (pair (find-forms-containing text contains :recovery recovery))
        (destructuring-bind (index . node) pair
          (multiple-value-bind (line col)
              (cl-toolkit-ast::offset-to-line-col text (node-start node))
            (unless first (format *standard-output* ","))
            (setf first nil)
            (format *standard-output*
                    "{\"index\":~a,\"name\":\"~a\",\"line\":~a,\"col\":~a~@[,\"source\":\"~a\"~]}"
                    index
                    (cl-toolkit-ast::escape-json-string (or (node-form-name node) "?"))
                    line col
                    (when with-source
                      (cl-toolkit-ast::escape-json-string
                       (node-source-text text node))))))))
    (format *standard-output* "]~%")))

(defun find-forms/command ()
  (clingon:make-command
   :name "find-forms"
   :usage "(-f FILE | --code CODE) --contains SNIPPET [--with-source]"
   :description "Find top-level forms whose source contains SNIPPET"
   :options (list
             (clingon:make-option :string :long-name "file" :short-name #\f
                                  :description "File to search" :key :file)
             (clingon:make-option :string :long-name "code"
                                  :description "Inline code" :key :code)
             (clingon:make-option :string :long-name "contains"
                                  :description "Snippet to search for" :required t
                                  :key :contains)
             (clingon:make-option :flag :long-name "with-source"
                                  :description "Include matching form source in output"
                                  :key :with-source)
             (make-recovery-option))
   :handler #'find-forms/handler))

;;; ============================================================
;;; Split-forms Command (whitespace-jam repair)
;;; ============================================================

(defun split-forms/handler (cmd)
  (with-edit-context (cmd)
    (handler-case
        (let ((result (split-jammed-top-level text :recovery recovery)))
          (deliver-edit-result result original-text file preview write quiet))
      (error (c)
        (output-edit-result nil (format nil "~a" c))))))

(defun split-forms/command ()
  (clingon:make-command
   :name "split-forms"
   :usage "(-f FILE | --code CODE)"
   :description "Insert newlines between top-level forms jammed on one line"
   :long-description "Minimal whitespace repair: adds a newline between any
   two adjacent top-level forms sharing a line. Unlike format, never
   reindents anything else — the diff touches only jammed boundaries."
   :options (list
             (clingon:make-option :string :long-name "file" :short-name #\f
                                  :description "File to edit" :key :file)
             (clingon:make-option :string :long-name "code"
                                  :description "Inline code" :key :code)
              (make-write-option)
(clingon:make-option :string :long-name "backup-dir"
                                   :description "Also save timestamped pre-edit snapshots here"
                                   :key :backup-dir)
              (clingon:make-option :flag :long-name "no-backup"
                                   :description "Skip the rolling .bak backup on write"
                                   :key :no-backup)
              (make-preview-option)
              (make-quiet-option)
              (make-recovery-option))
   :handler #'split-forms/handler))

;;; ============================================================
;;; ============================================================
;;; Check-anchor Command (occurrence verification)
;;; ============================================================

(defun check-anchor/handler (cmd)
  (let* ((text (read-input cmd))
         (anchor (clingon:getopt cmd :text)))
    (unless (and anchor (> (length anchor) 0))
      (format *error-output* "Error: --text is required~%")
      (clingon:exit 1))
    (multiple-value-bind (count first-offset)
        (count-text-occurrences text anchor)
      (multiple-value-bind (line col)
          (if (plusp count)
              (cl-toolkit-ast::offset-to-line-col text first-offset)
              (values -1 -1))
        (format *standard-output*
                "{\"count\":~a,\"first-offset\":~a,\"line\":~a,\"col\":~a}~%"
                count first-offset line col))
      ;; exit 1 when the anchor is not unique — the safe-edit precondition
      (unless (= count 1)
        (clingon:exit 1)))))

(defun check-anchor/command ()
  (clingon:make-command
   :name "check-anchor"
   :usage "(-f FILE | --code CODE) --text SNIPPET"
   :description "Verify a snippet occurs exactly once (safe-edit precondition)"
   :options (list
             (clingon:make-option :string :long-name "file" :short-name #\f
                                  :description "File to search" :key :file)
             (clingon:make-option :string :long-name "code"
                                  :description "Inline code" :key :code)
             (clingon:make-option :string :long-name "text"
                                  :description "Literal snippet to count" :required t
                                  :key :text))
   :handler #'check-anchor/handler))

;;; ============================================================
;;; Patch-span Command (verified byte-level substitution)
;;; ============================================================

(defun patch-span/handler (cmd)
  (with-edit-context (cmd :code-key :new-text)
    (let ((old-text (clingon:getopt cmd :old-text))
          (new-text code)
          (allow-shift (clingon:getopt cmd :allow-shift)))
      (unless (and line col old-text new-text)
        (format *error-output* "Error: --line, --col, --old, --new are required~%")
        (clingon:exit 1))
      (handler-case
          (let* ((offset (cl-toolkit-ast::offset-to-line-col-inverse text line col))
                 ;; byte-exact precondition: OLD must sit exactly at the position
                 (actual (subseq text offset
                                 (min (length text)
                                      (+ offset (length old-text))))))
            (unless (string= actual old-text)
              (format *error-output*
                      "Anchor mismatch at line ~a, col ~a~%  expected: ~s~%  found:    ~s~%"
                      line col old-text actual)
              (clingon:exit 1))
            (let ((delta (net-depth-delta old-text new-text)))
              (unless (zerop delta)
                (if allow-shift
                    (format *error-output* "Warning: net depth delta ~a (structure shifts)~%" delta)
                    (progn
                      (format *error-output*
                              "Refusing: net depth delta ~a -- closers would shift scope. ~
                               Pass --allow-shift if this wrap/restructure is intended.~%"
                              delta)
                      (clingon:exit 1))))
              (let ((result (concatenate 'string
                                         (subseq text 0 offset)
                                         new-text
                                         (subseq text (+ offset (length old-text))))))
                (validate-edited-source result recovery no-validate-result)
                (when (not quiet)
                  (format *error-output* "Patching [line ~a, col ~a] delta=~a~%"
                          line col delta))
                (deliver-edit-result result original-text file preview write quiet))))
        (error (c)
          (output-edit-result nil (format nil "~a" c)))))))

(defun patch-span/command ()
  (clingon:make-command
   :name "patch-span"
   :usage "(-f FILE | --code CODE) --line L --col C --old TXT --new TXT [--allow-shift]"
   :description "Byte-verified text substitution with net depth-delta guard"
   :long-description "Replaces an exact text span, but only after verifying: ~
                       (1) OLD appears byte-exactly at LINE/COL; ~
                       (2) the substitution does not change net paren depth ~
                       (reader-aware: strings, chars, comments respected). ~
                       Depth-shifting wraps/restructures need --allow-shift."
   :options (list
             (clingon:make-option :string :long-name "file" :short-name #\f
                                  :description "File to patch" :key :file)
             (clingon:make-option :string :long-name "code"
                                  :description "Inline code" :key :code)
             (clingon:make-option :integer :long-name "line" :short-name #\l
                                  :description "Line number (0-based)" :key :line)
             (clingon:make-option :integer :long-name "col" :short-name #\c
                                  :description "Column number (0-based)" :key :col)
             (clingon:make-option :string :long-name "old"
                                  :description "Exact existing text to replace" :key :old-text)
             (clingon:make-option :string :long-name "new"
                                  :description "Replacement text" :key :new-text)
              (clingon:make-option :string :long-name "code-file"
                                   :description "Read code from file instead of inline argument (\"-\" reads stdin)"
                                   :key :code-file)
             (clingon:make-option :flag :long-name "allow-shift"
                                  :description "Permit nonzero net depth delta"
                                  :key :allow-shift)
              (make-write-option)
              (make-preview-option)
              (clingon:make-option :string :long-name "backup-dir"
                                  :description "Also save timestamped pre-edit snapshots here"
                                   :key :backup-dir)
              (clingon:make-option :flag :long-name "no-backup"
                                   :description "Skip the rolling .bak backup on write"
                                   :key :no-backup)
              (make-quiet-option)
              (make-recovery-option)
              (clingon:make-option :flag :long-name "no-validate-result"
                                   :description "Skip result validation"
                                   :key :no-validate-result))
   :handler #'patch-span/handler))

;;; ============================================================
;;; Lint Command (duplicate form detection)
;;; ============================================================

(defun lint/handler (cmd)
  (let* ((recovery (clingon:getopt cmd :recovery))
         (text (read-input cmd))
         (groups (duplicate-top-level-forms text :recovery recovery)))
    (if groups
        (progn
          (dolist (g groups)
            (format *error-output* "Duplicate top-level forms at offsets ~{~a~^, ~}:~%" g)
            (dolist (off g)
              (multiple-value-bind (l c)
                  (cl-toolkit-ast::offset-to-line-col text off)
                (format *error-output* "  [line ~a, col ~a]~%" l c))))
          (clingon:exit 1))
        (format *standard-output* "No duplicate top-level forms.~%"))))

(defun lint/command ()
  (clingon:make-command
   :name "lint"
   :usage "(-f FILE | --code CODE)"
   :description "Flag duplicate identical top-level forms"
   :options (list
             (clingon:make-option :string :long-name "file" :short-name #\f
                                  :description "File to lint" :key :file)
             (clingon:make-option :string :long-name "code"
                                  :description "Inline code" :key :code)
             (make-recovery-option))
   :handler #'lint/handler))

;;; ============================================================
;;; Diff-forms Command (structural comparison)
;;; ============================================================

(defun collapse-to-line (string &optional (max-chars 60))
  "First MAX-CHARS of STRING with newlines/tabs collapsed to spaces."
  (let ((flat (with-output-to-string (out)
                (loop for ch across string
                      do (write-char (if (member ch '(#\Newline #\Tab #\Return)) #\Space ch) out)))))
    (let ((trimmed (string-left-trim " " flat)))
      (if (> (length trimmed) max-chars)
          (concatenate 'string (subseq trimmed 0 max-chars) "...")
          trimmed))))

(defun diff-forms/handler (cmd)
  (let* ((name (clingon:getopt cmd :name))
         (against-file (clingon:getopt cmd :against-file))
         (against-name (clingon:getopt cmd :against-name))
         (recovery (clingon:getopt cmd :recovery))
         (text-a (read-input cmd))
         (file-b (resolve-file-path against-file))
         (text-b (if file-b
                     (read-file-to-string file-b)
                     (read-input cmd))))
    (unless name
      (format *error-output* "Error: --name is required~%")
      (clingon:exit 1))
    (let ((node-a (or (find-top-level-by-name text-a name :recovery recovery)
                      (error "No top-level form named '~a' in first source" name)))
          (node-b (or (find-top-level-by-name
                       text-b (or against-name name) :recovery recovery)
                      (error "No top-level form named '~a' in second source" (or against-name name)))))
      ;; classify direct children by exact source equality
      (let ((kids-a (mapcar (lambda (n) (node-source-text text-a n))
                            (node-children node-a)))
            (kids-b (mapcar (lambda (n) (node-source-text text-b n))
                            (node-children node-b)))
            (added nil) (removed nil) (kept-a (make-hash-table :test #'equal)))
      (dolist (k kids-a) (incf (gethash k kept-a 0)))
      (dolist (k kids-b)
        (if (plusp (gethash k kept-a 0))
            (decf (gethash k kept-a 0))
            (push k added)))
      (maphash (lambda (k n)
                 (dotimes (_ n) (push k removed)))
               kept-a)
      (format *standard-output* "~{+ ~a~%~}" (mapcar #'collapse-to-line (nreverse added)))
      (format *standard-output* "~{- ~a~%~}" (mapcar #'collapse-to-line (nreverse removed)))
      (when (and (null added) (null removed))
        (format *standard-output* "Structurally identical (direct children).~%"))
      (when (or added removed)
        (clingon:exit 1))))))

(defun diff-forms/command ()
  (clingon:make-command
   :name "diff-forms"
   :usage "-f FILE --name X [--against-file G] [--against-name Y]"
   :description "Tree-level add/remove summary of a form's direct children across two versions"
   :options (list
             (clingon:make-option :string :long-name "file" :short-name #\f
                                  :description "First (current) file" :key :file)
             (clingon:make-option :string :long-name "code"
                                  :description "Inline code (first source)" :key :code)
             (clingon:make-option :string :long-name "name" :short-name #\n
                                  :description "Form to compare" :key :name)
             (clingon:make-option :string :long-name "against-file"
                                  :description "Second file (default: same as --code/-f)" :key :against-file)
             (clingon:make-option :string :long-name "against-name"
                                  :description "Form name in second source (default: same)" :key :against-name)
             (make-recovery-option))
   :handler #'diff-forms/handler))

;;; Help / Version Subcommands
;;; ============================================================

(defun version/handler (cmd)
  (declare (ignore cmd))
  (format *standard-output* "cl-toolkit 0.3.4~%"))

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
   :version "0.3.4"
   :description "Lisp code parser for structural analysis and editing. All positions are 0-based (grep -n counts from 1)."
   :long-description "A CLI tool for parsing, querying, and editing Lisp source code ~
                      using structural AST operations. Supports standard and ~
                      error-recovery parsing modes. ~
                      Line/col arguments and all output are 0-based; ~
                      editor grep -n line numbers are 1-based."
   :authors '("cl-agent-validate")
   :license "MIT"
   :handler (lambda (cmd) (clingon:print-usage-and-exit cmd t))
   :sub-commands (list
                    (parse/command)
                    (find/command)
                    (find-forms/command)
                    (check-anchor/command)
                    (patch-span/command)
                    (lint/command)
                    (diff-forms/command)
                    (extract/command)
                    (validate/command)
                    (top-level/command)
                    (source-of/command)
                    (balance/command)
                   (format/command)
                   (delete-form/command)
                   (insert-form/command)
                   (append-form/command)
                    (replace-form/command)
                    (batch-replace/command)
                    (insert-at/command)
                    (split-forms/command)
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
