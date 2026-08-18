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
   For lists (defun, defvar, etc.), returns the name of the first child symbol.
   For atoms, returns the symbol name or value."
  (cond
    ((not (nodep node)) nil)
    ((node-list-p node)
     (let ((children (node-children node)))
       (when (and children (nodep (first children)))
         (cond
           ((eq (node-type (first children)) :symbol)
            (node-name (first children)))
           ((node-list-p (first children))
            (node-form-name (first children)))
           (t nil)))))
    ((eq (node-type node) :symbol) (node-name node))
    ((eq (node-type node) :number) (format nil "~a" (node-value node)))
    ((eq (node-type node) :string) (format nil "~s" (node-value node)))
    (t nil)))

;;; Position helpers

(defun offset-to-line-col (text offset)
  "Convert a 0-indexed byte offset to (values line col) 0-indexed."
  (let ((line 0) (col 0))
    (loop for i from 0 below (min offset (length text))
          do (if (char= (char text i) #\Newline)
                 (progn (incf line) (setf col 0))
                 (incf col)))
    (values line col)))

(defun offset-to-line-col-inverse (text line col)
  "Convert LINE and COL (0-indexed) to a 0-indexed byte offset."
  (let ((offset 0) (current-line 0) (current-col 0))
    (loop while (< offset (length text))
          do (when (and (= current-line line)
                        (= current-col col))
               (return offset))
              (if (char= (char text offset) #\Newline)
                  (progn (incf current-line) (setf current-col 0))
                  (incf current-col))
              (incf offset))
    offset))

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
