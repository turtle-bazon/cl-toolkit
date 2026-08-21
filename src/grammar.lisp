(in-package #:cl-toolkit-grammar)

;;; ============================================================
;;; Lisp source grammar using esrap
;;; ============================================================
;;; Parses Lisp source code into AST nodes with source positions.
;;; Handles: lists, vectors, atoms, strings, char literals,
;;;          numbers, symbols, comments, reader macros.

;;; --- Utility functions ---

(defun not-doublequote (char)
  (not (eql char #\")))

(defun not-paren-char (char)
  (not (member char '(#\( #\) #\[ #\] #\{ #\}))))

;;; --- Character classes ---

(defrule digit
    (character-ranges (#\0 #\9)))

(defrule alpha
    (character-ranges (#\a #\z) (#\A #\Z)))

(defrule alphanumeric
    (or digit alpha))

(defrule symbol-char
    (or alphanumeric #\- #\* #\+ #\! #\? #\_ #\= #\< #\> #\& #\/ #\~ #\@ #\$ #\% #\^ #\. #\: #\# #\| #\[ #\] #\` #\,))

;;; --- Comment rules ---

(defrule line-comment-char
    (and (! #\Newline) character)
  (:lambda (pair) (second pair)))

(defrule line-comment
    (and #\; (* line-comment-char))
  (:destructure (semi chars &bounds start end)
    (declare (ignore semi chars))
    (make-node :comment :kind :line :start start :end end)))

(defrule block-comment-body
    (* (or nested-block-comment (and (! (and #\| #\#)) character)))
  (:lambda (chars)
    (apply #'concatenate 'string
           (mapcar (lambda (pair)
                     (if (consp pair)
                         (if (consp (second pair))
                             (second pair)  ; nested block comment
                             (string (second pair)))
                         (string pair)))
                   chars))))

(defrule nested-block-comment
    (and "#|" block-comment-body "|#")
  (:lambda (result)
    (destructuring-bind (open body close) result
      (declare (ignore open close))
      body)))

(defrule block-comment
    (and "#|" block-comment-body "|#")
  (:destructure (open body close &bounds start end)
    (declare (ignore open close))
    (make-node :comment :kind :block :value body :start start :end end)))

(defrule comment
    (or line-comment block-comment))

;;; --- Whitespace including comments ---

(defrule ws-unit
    (or whitespace comment))

(defrule whitespace
    (+ (or #\Space #\Tab #\Newline #\Page))
  (:constant nil))

(defrule ws
    (* ws-unit)
  (:constant nil))

(defrule ws+
    (+ ws-unit)
  (:constant nil))

;;; --- Atom rules ---

;;; String
(defrule string-escape
    (and #\\ character)
  (:lambda (pair)
    (string (second pair))))

(defrule string-char
    (or string-escape (not-doublequote character))
  (:lambda (ch)
    (if (stringp ch) ch (string ch))))

(defrule string-body
    (* string-char)
  (:lambda (chars)
    (apply #'concatenate 'string chars)))

(defrule string-literal
    (and #\" string-body #\")
  (:destructure (open body close &bounds start end)
    (declare (ignore open close))
    (make-node :string :value body :start start :end end)))

;;; Number (integer or float).
;;; Boundary check: a number must not be followed by a symbol character,
;;; so tokens like 1+ / 1- / 123abc fall through to the symbol rule
;;; (matching the CL reader, where 1+ and 1- are symbols).
(defrule integer-part
    (+ digit)
  (:lambda (chars)
    (parse-integer (esrap:text chars))))

(defrule not-symbol-tail-char
    (! symbol-tail-char))

(defrule float-exponent
    (and (or #\e #\E) (? (or #\+ #\-)) (+ digit))
  (:lambda (exp)
    (destructuring-bind (e sign digits) exp
      (declare (ignore e))
      (let ((sign-str (if sign (string sign) ""))
            (digits-str (esrap:text digits)))
        (parse-integer (concatenate 'string sign-str digits-str))))))

(defrule float-body
    (and integer-part #\. (+ digit) (? float-exponent) not-symbol-tail-char)
  (:lambda (result)
    (destructuring-bind (int dot digits exp _) result
      (declare (ignore dot _))
      (let* ((frac (map 'string #'identity digits))
             (frac-val (if (> (length frac) 0)
                           (/ (parse-integer frac)
                              (expt 10 (length frac)))
                           0))
             (base (+ int frac-val))
             (exponent (if exp exp 0)))
        (float (* base (expt 10 exponent)) 1.0d0)))))

(defrule int-with-exponent
    (and integer-part float-exponent not-symbol-tail-char)
  (:lambda (result)
    (destructuring-bind (int exp _) result
      (float (* int (expt 10 exp)) 1.0d0))))

(defrule plain-integer
    (and integer-part not-symbol-tail-char)
  (:lambda (result)
    (first result)))

(defrule number
    (and (? sign-char) (or float-body int-with-exponent plain-integer))
  (:lambda (result &bounds start end)
    (destructuring-bind (sign val) result
      (make-node :number
                 :value (if (and sign (string= sign "-")) (- val) val)
                 :start start :end end))))

;;; Character literal
(defrule char-name
    (+ alpha)
  (:lambda (chars)
    (string-upcase (esrap:text chars))))

(defrule char-literal
    (and "#\\" (or char-name character))
  (:destructure (prefix char &bounds start end)
    (declare (ignore prefix))
    (let ((val (if (stringp char) char (string char))))
      (make-node :char :value val :start start :end end))))

;;; Dispatch after a leading '#'. The '#' itself is already consumed here,
;;; so only sub-rules NOT starting with '#' belong in this choice.
;;; #' / #. / #\ are matched by their own top-level rules in `form'.
(defrule sharp-dispatch
    (and "#" vector-form)
  (:destructure (sharp vec &bounds start end)
    (declare (ignore sharp))
    vec))

;;; Symbol. May start with a digit only if the whole token is not a
;;; valid number — the number rule is ordered before symbol in `form`,
;;; and its boundary checks make tokens like 1+/123abc fall through.
(defrule sign-char
    (or #\+ #\-))

(defrule symbol-head-char
    (or alpha digit #\- #\* #\+ #\! #\? #\_ #\= #\< #\> #\& #\/ #\~ #\@ #\$ #\% #\^ #\: #\# #\| #\` #\,))

(defrule symbol-tail-char
    (or symbol-head-char #\. #\[ #\]))

(defrule symbol-body
    (+ symbol-tail-char)
  (:lambda (chars)
    (esrap:text chars)))

(defrule symbol
    (and symbol-head-char (* symbol-tail-char))
  (:lambda (chars &bounds start end)
    (let ((full (esrap:text chars)))
      (let ((colon (position #\: full)))
        (if colon
            (make-node :symbol
                       :name (subseq full (1+ colon))
                       :package (subseq full 0 colon)
                       :start start :end end)
            (make-node :symbol :name full :start start :end end))))))

;;; --- Compound rules ---

;;; List (parenthesized form)
(defrule list-form
    (and #\( ws (* (and form ws)) ws #\))
  (:destructure (open ws1 forms-ws ws2 close &bounds start end)
    (declare (ignore open close ws1 ws2))
    (make-node :list
               :children (mapcar #'first forms-ws)
               :start start :end end)))

;;; Vector
(defrule vector-form
    (and "#(" ws form (* (and ws form)) ws #\))
  (:destructure (open ws1 first rest ws2 close &bounds start end)
    (declare (ignore open close ws1 ws2))
    (make-node :vector
               :children (cons first (mapcar #'second rest))
               :start start :end end)))

;;; Quote/sharp-reader macros
;;; Marker symbols carry the reader-macro's own bounds so every node in
;;; the tree has usable :start/:end (needed by source extraction).
(defrule quote-form
    (and #\' ws form)
  (:destructure (quote ws form &bounds start end)
    (declare (ignore quote ws))
    (make-node :list
               :children (list (make-node :symbol :name "QUOTE"
                                          :start start :end (+ start 1))
                               form)
               :start start :end end)))

(defrule sharp-quote
    (and "#'" ws form)
  (:destructure (sharp ws form &bounds start end)
    (declare (ignore sharp ws))
    (make-node :list
               :children (list (make-node :symbol :name "FUNCTION"
                                          :start start :end (+ start 2))
                               form)
               :start start :end end)))

(defrule sharp-dot
    (and "#." ws form)
  (:destructure (sharp ws form &bounds start end)
    (declare (ignore sharp ws))
    (make-node :list
               :children (list (make-node :symbol :name "EVAL"
                                          :start start :end (+ start 2))
                               form)
               :start start :end end)))

;;; Top-level form
(defrule form
    (or comment sharp-dispatch list-form vector-form quote-form sharp-quote sharp-dot char-literal string-literal number symbol)
  (:lambda (result)
    result))

;;; Source file = sequence of top-level forms with whitespace
(defrule source-file
    (and ws (* (and form ws)))
  (:lambda (result &bounds start end)
    (destructuring-bind (ws1 forms-ws) result
      (declare (ignore ws1))
      (let ((forms (mapcar #'first forms-ws)))
        (make-node :list
                   :children forms
                   :source "source-file"
                   :start start :end end)))))

;;; ============================================================
;;; Public API
;;; ============================================================

(defun first-line (string)
  "Return STRING up to its first newline."
  (let ((nl (position #\Newline string)))
    (if nl (subseq string 0 nl) string)))

(defun extract-error-location (report)
  "Return the '(Line L, Column C, Position N)' fragment of an esrap
   error REPORT, or NIL if absent."
  (let ((idx (search "(Line " report)))
    (when idx
      (let ((close (position #\) report :start idx)))
        (when close
          (subseq report idx (1+ close)))))))

(defun compact-parse-error (condition)
  "One-line summary of an esrap parse error.
   Esrap's full report enumerates every grammar alternative across many
   lines; keep only the location so CLI output stays TUI-friendly."
  (let* ((report (princ-to-string condition))
         (loc (extract-error-location report)))
    (if loc
        (format nil "Syntax error at ~a" (subseq loc 1 (1- (length loc))))
        (first-line report))))

(defun parse-lisp-source (text &optional (start 0) end)
  "Parse TEXT as Lisp source code. Returns AST root node.
   START and END are optional bounds into TEXT."
  (let ((text-end (or end (length text)))
        (*standard-output* (make-broadcast-stream))
        (*error-output* (make-broadcast-stream)))
    (handler-case
        (let ((ast (esrap:parse 'source-file text
                                :start start
                                :end text-end)))
          ;; Check if parse consumed all input
          (let ((consumed-end (or (getf ast :end) start)))
            (if (< consumed-end text-end)
                ;; Unconsumed input = incomplete or invalid form
                (let ((remaining (subseq text consumed-end text-end))
                      (remaining-start consumed-end))
                  ;; Check if remaining is just whitespace/comments
                  (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) remaining)))
                    (if (> (length trimmed) 0)
                        ;; Real unconsumed content = error
                        (make-node :error
                                   :value (format nil "Incomplete or invalid form: ~s" trimmed)
                                   :start remaining-start
                                   :end text-end)
                        ;; Just whitespace/comments = ok, but note trailing content
                        ast)))
                ast)))
      (esrap:esrap-parse-error (c)
        (make-node :error
                   :value (compact-parse-error c)
                   :start 0
                   :end text-end)))))

;;; ============================================================
;;; Error Recovery Parser
;;; ============================================================
;;; Parses Lisp source with error recovery. When parsing fails,
;;; creates ERROR nodes for malformed regions and continues.

(defun find-next-form-boundary (text pos end)
  "Find the position after the next form boundary starting from POS."
  (when (>= pos end) (return-from find-next-form-boundary end))
  (let ((depth 0) (in-string nil) (in-comment nil) (block-depth 0))
    (loop for i from pos below end
          for ch = (char text i)
          do (cond
               (in-comment
                (when (char= ch #\Newline) (setf in-comment nil)))
               (in-string
                (when (char= ch #\") (setf in-string nil))
                (when (and (char= ch #\\) (< (1+ i) end)) (incf i)))
               ((plusp block-depth)
                (when (and (char= ch #\#) (< (1+ i) end) (char= (char text (1+ i)) #\|))
                  (incf block-depth) (incf i))
                (when (and (char= ch #\|) (< (1+ i) end) (char= (char text (1+ i)) #\#))
                  (decf block-depth) (incf i)))
               (t
                (case ch
                  (#\; (setf in-comment t))
                  (#\" (setf in-string t))
                  (#\( (incf depth))
                  (#\) (if (zerop depth) (return (1+ i)) (decf depth)))
                  (#\[ (incf depth))
                  (#\] (if (zerop depth) (return (1+ i)) (decf depth)))
                  (#\{ (incf depth))
                  (#\} (if (zerop depth) (return (1+ i)) (decf depth)))
               (#\# (when (and (< (1+ i) end) (char= (char text (1+ i)) #\|))
                          (incf block-depth) (incf i))
                     (when (and (< (1+ i) end) (char= (char text (1+ i)) #\\))
                           (incf i))))))
          finally (return end))))

(defun skip-whitespace (text pos end)
  "Skip whitespace characters starting at POS."
  (loop while (and (< pos end)
                   (member (char text pos) '(#\Space #\Tab #\Newline #\Page #\;)))
        do (incf pos))
  pos)

(defun offset-node (node offset)
  "Add OFFSET to :start and :end of NODE and all descendants."
  (when node
    (when (getf node :start) (incf (getf node :start) offset))
    (when (getf node :end) (incf (getf node :end) offset))
    (dolist (child (getf node :children))
      (offset-node child offset)))
  node)

(defun try-parse-form-at (text pos end)
  "Try to parse a single form at POS.
   Returns (values node new-pos) or (values nil skip-pos) on failure."
  (let ((remaining (subseq text pos end))
        (*standard-output* (make-broadcast-stream))
        (*error-output* (make-broadcast-stream)))
    (handler-case
        (let ((node (esrap:parse 'form remaining)))
          (let ((form-end (+ pos (or (getf node :end) (length remaining)))))
            (values (offset-node node pos) (skip-whitespace text form-end end))))
      (esrap:esrap-parse-error (c)
        (let ((result (ignore-errors (esrap:esrap-parse-error-result c))))
          (if result
              (let ((parse-end (ignore-errors (result-position result)))
                    (node (ignore-errors (successful-parse-production result))))
                (if (and parse-end node)
                    (values (offset-node node pos) (skip-whitespace text (+ pos parse-end) end))
                    (values nil (find-next-form-boundary text pos end))))
              (values nil (find-next-form-boundary text pos end))))))))

(defun parse-with-recovery (text &optional (start 0) end)
  "Parse TEXT with error recovery. Returns AST with ERROR nodes."
  (let ((end (or end (length text)))
        (pos start)
        (forms nil))
    (loop while (< pos end)
          do (multiple-value-bind (node new-pos)
                 (try-parse-form-at text pos end)
               (cond
                 (node
                  (push node forms)
                  (setf pos new-pos))
                 ((> new-pos pos)
                  (push (make-node :error
                                   :value (subseq text pos new-pos)
                                   :start pos :end new-pos)
                        forms)
                  (setf pos new-pos))
                 (t
                  (push (make-node :error
                                   :value (string (char text pos))
                                   :start pos :end (1+ pos))
                        forms)
                  (setf pos (1+ pos))))))
    (make-node :list
               :children (nreverse forms)
               :source "source-file"
               :start start :end end)))
