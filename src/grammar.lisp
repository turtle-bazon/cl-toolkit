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

;;; Unicode-aware: (alpha-char-p character) matches one character
;;; satisfying the predicate (parser.common-rules uses the same idiom).
;;; ASCII-only ranges broke real code (:λlist in trivia => hard ERROR).
(defrule alpha
    (alpha-char-p character))

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
      (declare (ignore _))
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
    ;; Two or more letters: single alphabetic chars (e.g. #\a, #\λ) fall
    ;; through to the `character' branch preserving case; upcasing only
    ;; applies to multi-char names (Space, Newline, ...).
    (and alpha (+ alpha))
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
    (declare (ignore sharp start end))
    vec))

;;; Symbol. May start with a digit only if the whole token is not a
;;; valid number — the number rule is ordered before symbol in `form`,
;;; and its boundary checks make tokens like 1+/123abc fall through.
;;; Head includes dot so lone "." (dotted-pair syntax) parses as a
;;; symbol instead of failing the whole enclosing list.
(defrule sign-char
    (or #\+ #\-))

(defrule symbol-head-char
    (or alpha digit #\. #\- #\* #\+ #\! #\? #\_ #\= #\< #\> #\& #\/ #\~ #\@ #\$ #\% #\^ #\: #\# #\| #\` #\,))

;;; Symbol constituent escapes: \X is literal X, |...| quotes an
;;; arbitrary span (spaces and parens included). Found in the wild as
;;; '#(\a |b| |cD|)' (iterate) — previously a hard parse ERROR since "\"
;;; matched nothing at all.
(defrule symbol-escape
    (and #\\ character)
  (:lambda (pair)
    (esrap:text pair)))

(defrule bar-inner-char
    (or symbol-escape (and (! #\|) character))
  (:lambda (x)
    (if (stringp x) x (string (second x)))))

(defrule bar-segment
    (and #\| (* bar-inner-char) #\|)
  (:lambda (parts)
    (esrap:text parts)))

(defrule symbol-tail-char
    (or symbol-escape bar-segment symbol-head-char #\. #\[ #\]))

(defrule symbol-head
    (or symbol-escape bar-segment symbol-head-char))

(defrule symbol-body
    (+ symbol-tail-char)
  (:lambda (chars)
    (esrap:text chars)))

(defrule symbol
    (and symbol-head (* symbol-tail-char))
  (:lambda (chars &bounds start end)
    (let ((full (esrap:text chars)))
      ;; Split on the LAST colon so "foo::bar" yields package "foo",
      ;; name "bar" (not ":bar"). Leading ":" means keyword package.
      (let ((colon (position #\: full :from-end t)))
        (if colon
            (let ((raw-pkg (subseq full 0 colon))
                  (raw-name (subseq full (1+ colon))))
              (make-node :symbol
                         :name (string-left-trim ":" raw-name)
                         :package (let ((trimmed (string-trim ":" raw-pkg)))
                                    (if (> (length trimmed) 0)
                                        trimmed
                                        "KEYWORD"))
                         :start start :end end))
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

;;; Backquote / comma reader macros. Previously "`" and "," were symbol
;;; chars, so "`(a ,b)" parsed as TWO forms (stray "`" symbol + list).
;;; Ubiquitous in real code (26+ files in a 6-lib sample).
(defrule backquote-form
    (and #\` ws form)
  (:destructure (bq ws form &bounds start end)
    (declare (ignore bq ws))
    (make-node :list
               :children (list (make-node :symbol :name "BACKQUOTE"
                                          :start start :end (+ start 1))
                               form)
               :start start :end end)))

(defrule comma-form
    (and #\, (? #\@) ws form)
  (:destructure (comma at ws form &bounds start end)
    (declare (ignore comma ws))
    (make-node :list
               :children (list (make-node :symbol
                                          :name (if at "UNQUOTE-SPLICING" "UNQUOTE")
                                          :start start :end (+ start (if at 2 1)))
                               form)
               :start start :end end)))

;;; Feature conditionals #+ / #-. Previously parsed as a stray symbol
;;; ("#+sbcl") plus the guarded form — two top-level forms instead of
;;; one. Wrapped so the file's top-level shape stays accurate.
(defrule feature-form
    (and (or "#+" "#-") ws form ws form)
  (:destructure (marker ws1 feat ws2 target &bounds start end)
    (declare (ignore ws1 ws2))
    (make-node :list
               :children (list (make-node :symbol :name marker
                                          :start start :end (+ start 2))
                               feat target)
               :start start :end end)))

;;; Structure / complex / pathname / array / bit-vector literals.
;;; Previously split into a stray symbol ("#S") plus payload — again two
;;; forms instead of one. Case-insensitive dispatch per the CL reader.
(defrule struct-form
    (and (or "#S" "#s") ws form)
  (:destructure (marker ws payload &bounds start end)
    (declare (ignore marker ws))
    (make-node :list
               :children (list (make-node :symbol :name "STRUCT"
                                          :start start :end (+ start 2))
                               payload)
               :start start :end end)))

(defrule complex-form
    (and (or "#C" "#c") ws form)
  (:destructure (marker ws payload &bounds start end)
    (declare (ignore marker ws))
    (make-node :list
               :children (list (make-node :symbol :name "COMPLEX"
                                          :start start :end (+ start 2))
                               payload)
               :start start :end end)))

(defrule pathname-form
    (and (or "#P" "#p") ws form)
  (:destructure (marker ws payload &bounds start end)
    (declare (ignore marker ws))
    (make-node :list
               :children (list (make-node :symbol :name "PATHNAME"
                                          :start start :end (+ start 2))
                               payload)
               :start start :end end)))

(defrule array-form
    (and "#" (* digit) (or "A" "a") ws form)
  (:destructure (hash rank letter ws payload &bounds start end)
    (declare (ignore hash ws))
    (make-node :list
               :children (list (make-node :symbol :name "ARRAY"
                                          :start start
                                          :end (+ start 2 (length rank)))
                               payload)
               :start start :end end)))

(defrule bit-char
    (or #\0 #\1))

(defrule bitvector-form
    (and "#*" (* bit-char))
  (:destructure (marker bits &bounds start end)
    (declare (ignore marker))
    (make-node :list
               :children (list (make-node :symbol :name "BIT-VECTOR"
                                          :start start :end (+ start 2))
                               (make-node :symbol
                                          :name (esrap:text bits)
                                          :start (+ start 2) :end end))
               :start start :end end)))

;;; Top-level form
;;; NOTE: sharp-dispatch ("#" + vector-form) required "##(" and never
;;; matched — vector-form already covers "#(...)". Removed from the
;;; choice to avoid dead-branch confusion.
(defrule form
    (or comment list-form vector-form quote-form sharp-quote sharp-dot
        backquote-form comma-form feature-form array-form struct-form
        complex-form pathname-form bitvector-form
        char-literal string-literal number symbol)
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
  "Skip whitespace and line comments starting at POS.
   Bare ';' is not whitespace — the comment body must be skipped too,
   otherwise recovery treats it as broken forms."
  (loop while (< pos end)
        do (let ((ch (char text pos)))
             (cond
               ((member ch '(#\Space #\Tab #\Newline #\Page #\Return))
                (incf pos))
               ((char= ch #\;)
                ;; skip to (and past) the newline
                (loop while (and (< pos end)
                                (char/= (char text pos) #\Newline))
                      do (incf pos))
                (when (< pos end) (incf pos)))
               (t (return pos))))
        finally (return pos))
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
   Leading whitespace/comments are skipped first (form itself does not
   allow them). Returns (values node new-pos) or (values nil skip-pos)."
  (setf pos (skip-whitespace text pos end))
  (when (>= pos end)
    (return-from try-parse-form-at (values nil end)))
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
                    (values nil (skip-whitespace text (find-next-form-boundary text pos end) end))))
              (values nil (skip-whitespace text (find-next-form-boundary text pos end) end))))))))

(defun parse-with-recovery (text &optional (start 0) end)
  "Parse TEXT with error recovery. Returns AST with ERROR nodes."
  (let ((end (or end (length text)))
        (pos (skip-whitespace text start (or end (length text))))
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
                  (setf pos (skip-whitespace text (1+ pos) end))))))
    (make-node :list
               :children (nreverse forms)
               :source "source-file"
               :start start :end end)))
