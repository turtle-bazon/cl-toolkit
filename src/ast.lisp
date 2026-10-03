(in-package #:cl-toolkit-ast)

;;; AST Node representation
;;; Nodes are plists with :type, :start, :end, :line, :col, :source, :children

(defun make-node (type &key start end line col source children value name package kind)
  "Create an AST node of TYPE with position info and optional data."
  (let ((node (list :type type)))
    (when start (setf (getf node :start) start))
    (when end (setf (getf node :end) end))
    (when line (setf (getf node :line) line))
    (when col (setf (getf node :col) col))
    (when source (setf (getf node :source) source))
    (when children (setf (getf node :children) children))
    (when value (setf (getf node :value) value))
    (when name (setf (getf node :name) name))
    (when package (setf (getf node :package) package))
    (when kind (setf (getf node :kind) kind))
    node))

(defun node-type (node) (getf node :type))
(defun node-start (node) (getf node :start))
(defun node-end (node) (getf node :end))
(defun node-line (node) (getf node :line))
(defun node-col (node) (getf node :col))
(defun node-source (node) (getf node :source))
(defun node-children (node) (getf node :children))
(defun node-value (node) (getf node :value))
(defun node-name (node) (getf node :name))
(defun node-package (node) (getf node :package))

(defun nodep (thing) (and (listp thing) (getf thing :type)))

(defun node-list-p (node) (eq (node-type node) :list))

(defun node-atom-p (node)
  (member (node-type node) '(:symbol :number :string :char)))

(defun node-error-p (node) (eq (node-type node) :error))

(defun node-form-count (node)
  "Count the number of top-level forms in a :list node (source file)."
  (if (node-list-p node)
      (length (node-children node))
      0))

(defun node-form-name (node)
  "Extract a human-readable name from a top-level form node.
   For defun/defvar/defmacro etc., returns the defined name (second child).
   For atoms, returns the symbol name or value."
  (cond
    ((not (nodep node)) nil)
     ((node-list-p node)
      (let ((children (node-children node)))
        (when (and children (nodep (first children)))
          (let ((form-type (node-name (first children))))
            (cond
              ;; For defun/defvar/defmacro/defgeneric/defclass etc,
              ;; return the second child (the name being defined)
              ((and form-type
                    (> (length children) 1)
                    (nodep (second children))
                    (member form-type '("defun" "defvar" "defparameter" "defmacro"
                                        "defgeneric" "defclass" "defstruct"
                                        "deftype" "defmethod"
                                        "define-compiler-macro" "defsetf"
                                        "define-setf-expander" "defpackage"
                                        ;; fiveam-style: (test NAME body...)
                                        "test")
                            :test #'string-equal))
               (node-name (second children)))
              ;; For other forms, return the first symbol
              ((and form-type (eq (node-type (first children)) :symbol))
               (node-name (first children)))
              ;; A wrapper list ((defun a ...)) names what it wraps, so
              ;; --name selection reaches through one level of wrapping
              ((node-list-p (first children))
               (node-form-name (first children)))
              (t nil))))))
    ((eq (node-type node) :symbol) (node-name node))
    ((eq (node-type node) :number) (format nil "~a" (node-value node)))
    ((eq (node-type node) :string) (format nil "~s" (node-value node)))
    (t nil)))

;;; Position helpers

(defun line-break-at (text i limit)
  "Length in characters of the line break starting at I, or NIL.
   CR, LF and CRLF each count as ONE break so CRLF files and
   classic-Mac CR-only files map to the same line/column as LF files."
  (let ((ch (char text i)))
    (cond
      ((char= ch #\Newline) 1)
      ((char= ch #\Return)
       (if (and (< (1+ i) limit) (char= (char text (1+ i)) #\Newline))
           2
           1))
      (t nil))))

(defun offset-to-line-col (text offset)
  "Convert a 0-indexed byte offset to (values line col) 0-indexed."
  (let ((line 0) (col 0) (i 0))
    (let ((limit (min offset (length text))))
      (loop while (< i limit)
            do (let ((n (line-break-at text i limit)))
                 (if n
                     (progn (incf line) (setf col 0) (incf i n))
                     (progn (incf col) (incf i))))))
    (values line col)))

(defun offset-to-line-col-inverse (text line col)
  "Convert LINE and COL (0-indexed) to a 0-indexed byte offset.
   Signals an error when the position is out of range; the EOF position
   (one past the last character) is valid for end-of-line appends."
  (when (or (< line 0) (< col 0))
    (error "Invalid position: line ~a, col ~a (must be >= 0)" line col))
  (let ((offset 0) (current-line 0) (current-col 0)
        (len (length text)))
    (loop while (< offset len)
          do (when (and (= current-line line)
                        (= current-col col))
               (return-from offset-to-line-col-inverse offset))
             (let ((n (line-break-at text offset len)))
               (if n
                   (progn (incf current-line) (setf current-col 0)
                          (incf offset n))
                   (progn (incf current-col) (incf offset)))))
    ;; Loop ended at EOF: valid only if target is exactly EOF.
    (if (and (= current-line line) (= current-col col))
        offset
        (error "Position out of range: line ~a, col ~a (file has ~a lines)"
               line col (1+ current-line)))))

;;; Span invariants
;;;;
;;; Every node carries the half-open byte range [start, end) of the
;;; source it came from, and the editing commands slice with those
;;; numbers. A node that claims a range outside its parent, or siblings
;;; that run backwards, produces silently wrong edits, so the invariants
;;; are worth stating once and checking mechanically.
;;;;
;;;   1. both bounds are integers and start <= end
;;;   2. a child lies inside its parent: parent.start <= child.start and
;;;      child.end <= parent.end
;;;   3. siblings appear in source order: a child's start is not before
;;;      its predecessor's start
;;;   4. a child that consumed something has start < end (a zero-width
;;;      child is only legitimate for a skipped feature branch)
;;;   5. bounds stay inside TEXT when TEXT is supplied

(defun span-violation (node reason)
  (list :type (node-type node)
        :start (node-start node)
        :end (node-end node)
        :reason reason))

(defun check-node-spans (node &optional text)
  "Return a list of span problems found in NODE's tree, deepest first.
   Each problem is a plist with :type, :start, :end and :reason. An empty
   list means every node satisfies the span invariants. TEXT, when given,
   additionally bounds-checks against the source length."
  (let ((problems nil)
        (limit (and text (length text))))
    (labels ((walk (node parent)
               (let ((start (node-start node))
                     (end (node-end node)))
                 (cond ((not (and (integerp start) (integerp end)))
                        (push (span-violation node "bounds are not integers")
                              problems))
                       ((> start end)
                        (push (span-violation node "start is after end") problems))
                       ((and parent
                             (or (< start (node-start parent))
                                 (> end (node-end parent))))
                        (push (span-violation node "escapes its parent") problems))
                       ((and limit (or (> end limit) (< start 0)))
                        (push (span-violation node "outside the source")
                              problems)))
                 (let ((kids (node-children node))
                       (previous nil))
                   (dolist (kid kids)
                     ;; Overlap is fine -- a reader-macro marker can cover
                     ;; the same character as its parent -- but siblings
                     ;; must still appear in source order.
                     (when (and previous (> (node-start previous) (node-start kid)))
                       (push (span-violation kid
                                             "sibling starts before its predecessor")
                             problems))
                     (when (and (= (node-start kid) (node-end kid))
                                (not (eq (node-type kid) :skip)))
                       (push (span-violation kid "zero-width child consumed nothing")
                             problems))
                     (walk kid node)
                     (setf previous kid))))))
      (walk node nil))
    (nreverse problems)))

(defun leaf-span-problem (node text)
  "A problem plist for a childless NODE whose range does not read back as
   the token it parsed, or NIL."
  (let* ((start (node-start node))
         (end (node-end node)))
    (when (and (integerp start) (integerp end)
               (<= start end (length text))
               (= start end))
      (return-from leaf-span-problem
        (span-violation node "leaf span is empty")))
    (when (or (not (integerp start)) (not (integerp end))
              (> start end) (< start 0) (> end (length text)))
      (return-from leaf-span-problem nil))
    (let ((slice (subseq text start end)))
      (case (node-type node)
        (:string
         (unless (and (>= (length slice) 2)
                      (char= (char slice 0) (code-char 34))
                      (char= (char slice (1- (length slice))) (code-char 34)))
           (span-violation node "string span is not wrapped in quotes")))
        (:character
         (unless (and (>= (length slice) 2) (char= (char slice 0) (code-char 92)))
           (span-violation node "character span has no backslash prefix")))
        (t nil)))))

(defun check-source-spans (node text)
  "CHECK-NODE-SPANS plus a leaf check: every childless node's range must
   read back as the token it parsed. Returns a list of problems."
  (let ((problems (check-node-spans node text)))
    (labels ((walk (n)
               (if (node-children n)
                   (dolist (k (node-children n)) (walk k))
                   (let ((problem (leaf-span-problem n text)))
                     (when problem (push problem problems))))))
      (walk node))
    (nreverse problems)))

;;; JSON serialization using cl-json

(defun escape-json-string (str)
  "Escape a string for JSON output."
  (with-output-to-string (out)
    (loop for ch across str
          do (case ch
               (#\" (write-string "\\\"" out))
               (#\\ (write-string "\\\\" out))
               (#\Newline (write-string "\\n" out))
               (#\Tab (write-string "\\t" out))
               (#\Return (write-string "\\r" out))
               (otherwise (write-char ch out))))))

(defun node-to-alist (node)
  "Convert AST node to an alist suitable for cl-json encoding."
  (cond
    ((null node) nil)
    ((not (nodep node)) node)
    (    t
     (let ((result nil))
       (push (cons :type (symbol-name (node-type node))) result)
       (when (node-start node) (push (cons :start (node-start node)) result))
       (when (node-end node) (push (cons :end (node-end node)) result))
       (when (node-line node) (push (cons :line (node-line node)) result))
       (when (node-col node) (push (cons :col (node-col node)) result))
       (when (node-source node) (push (cons :source (node-source node)) result))
       (when (node-name node) (push (cons :name (node-name node)) result))
       (when (node-package node) (push (cons :package (node-package node)) result))
       (when (node-value node) (push (cons :value (node-value node)) result))
       (when (getf node :kind) (push (cons :kind (symbol-name (getf node :kind))) result))
       (when (node-children node)
         (push (cons :children (mapcar #'node-to-alist (node-children node))) result))
       (nreverse result)))))

(defun node-to-json-string (node)
  "Convert NODE to a JSON string."
  (cl-json:encode-json-to-string (node-to-alist node)))

(defun node-to-json (node &optional (stream *standard-output*))
  "Write NODE as JSON to STREAM."
  (let ((json-str (node-to-json-string node)))
    (write-string json-str stream)))
