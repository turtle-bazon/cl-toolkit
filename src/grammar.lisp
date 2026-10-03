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
    (or alphanumeric #\- #\* #\+ #\! #\? #\_ #\= #\< #\> #\& #\/ #\~ #\@ #\$ #\% #\^ #\. #\: #\# #\| #\[ #\] #\{ #\} #\` #\,))

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

;;; CLHS 2.1.1 whitespace is Space, Tab, Newline, Return and Page —
;;; #\Return matters in practice: without it every CRLF file (Windows
;;; checkouts, old CVS archives) failed to parse with a bogus syntax
;;; error. Note #\Vt (code 11) is NOT whitespace: the reader returns it
;;; as an ordinary character, i.e. "#\Vt".
(defrule whitespace
    (+ (or #\Space #\Tab #\Newline #\Return #\Page))
  (:constant nil))

(defrule ws
    (* ws-unit)
  (:constant nil))

(defrule ws+
    (+ ws-unit)
  (:constant nil))

;;; --- Atom rules ---

;;; String
;;; esrap builds one production per repetition element and walks that
;;; list recursively, so a body rule written as (* char) died on the
;;; control stack for any long literal — real files embedding a big
;;; JSON string (a single "~100KB string literal) killed the whole
;;; process, uncatchably. Both bodies are therefore matched in ONE
;;; production by an iterative scanner.

(defun scan-string-body (text position end)
  "esrap terminal: the body of a string literal, up to the closing
   quote. Returns (values body-string position-after-body nil) on
   success, or (values nil position reason) when unterminated."
  (let ((i position)
        (buf (make-array 0 :element-type 'character
                         :adjustable t :fill-pointer 0)))
    (loop while (< i end) do
      (let ((ch (char text i)))
        (cond
          ((char= ch #\")
           (return))
          ((char= ch #\\)
           (if (< (1+ i) end)
               (progn
                 (vector-push-extend (char text (1+ i)) buf)
                 (incf i 2))
               (progn
                 (vector-push-extend ch buf)
                 (incf i))))
          (t
           (vector-push-extend ch buf)
           (incf i)))))
    ;; stop AT the closing quote: string-literal has its own rule for it
    (if (and (< i end) (char= (char text i) #\"))
        (if (= i position)
            ;; An EMPTY body consumes nothing, and esrap treats a
            ;; zero-width non-positive match as failure. Third value T
            ;; means "success even without progress" in its terminal
            ;; protocol; the closing quote rule still moves us on.
            (values "" position t)
            (values (coerce buf 'string) i nil))
        (values nil position "Unterminated string literal"))))

(defrule string-body
  (function scan-string-body))

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

;;; Exponent marker with its value: e/E/f/F/s/S mean the default
;;; (single) float format, d/D/l/L mean double. The marker letter is
;;; kept (not just the value) so the result coerces to the same float
;;; format the reader produces: 1e0 is single, 1d0 is double.
(defrule float-exponent
    (and (or #\e #\E #\d #\D #\f #\F #\s #\S #\l #\L)
         (? (or #\+ #\-)) (+ digit))
  (:lambda (exp)
    (destructuring-bind (marker sign digits) exp
      (let ((sign-str (if sign (string sign) ""))
            (digits-str (esrap:text digits)))
        (cons marker
              (parse-integer (concatenate 'string sign-str digits-str)))))))

(defun float-with-marker (rational-value marker)
  "Coerce exact RATIONAL-VALUE to the float format MARKER selects:
   d/D/l/L give double, everything else (e/f/s, or absent) gives
   single — matching *read-default-float-format*. MARKER is the
   exponent letter as a one-character string."
  (let ((letter (if (characterp marker) marker (char marker 0))))
    (if (member letter '(#\d #\D #\l #\L))
        (coerce rational-value 'double-float)
        (coerce rational-value 'single-float))))

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
             (exponent (if exp (cdr exp) 0)))
        (float-with-marker (* base (expt 10 exponent))
                           (if exp (car exp) #\e))))))

(defrule int-with-exponent
    (and integer-part float-exponent not-symbol-tail-char)
  (:lambda (result)
    (destructuring-bind (int exp _) result
      (declare (ignore _))
      (float-with-marker (* int (expt 10 (cdr exp))) (car exp)))))

;;; 5.e3 is a float (the reader accepts an empty fraction with an
;;; exponent); 5. alone is an integer (see trailing-dot-integer).
(defrule dot-exponent-float
    (and integer-part #\. float-exponent not-symbol-tail-char)
  (:lambda (result)
    (destructuring-bind (int dot exp _) result
      (declare (ignore dot _))
      (float-with-marker (* int (expt 10 (cdr exp))) (car exp)))))

;;; .5 reads as 0.5, with the same marker rule for .5e2 and .5d0.
(defrule leading-dot-float
    (and #\. (+ digit) (? float-exponent) not-symbol-tail-char)
  (:lambda (result)
    (destructuring-bind (dot digits exp _) result
      (declare (ignore dot _))
      (let* ((frac-text (map 'string #'identity digits))
             (frac (/ (parse-integer frac-text)
                      (expt 10 (length frac-text))))
             (exponent (if exp (cdr exp) 0)))
        (float-with-marker (* frac (expt 10 exponent))
                           (if exp (car exp) #\e))))))

;;; 5. reads as the integer 5. The digit guard keeps 5.5 and 5.e3 for
;;; the float rules above.
(defrule trailing-dot-integer
    (and integer-part #\. (! digit) not-symbol-tail-char)
  (:lambda (result)
    (destructuring-bind (int dot guard _) result
      (declare (ignore dot guard _))
      int)))

(defun scan-ratio (text position end)
  "esrap terminal: an integer ratio NUM/DEN with nonzero DEN.
   Returns (values ratio new-pos nil), or failure when the shape is
   wrong or the denominator is zero (the reader signals there too)."
  (let ((i position))
    (let ((nstart i))
      (loop while (and (< i end) (digit-char-p (char text i))) do (incf i))
      (when (and (> i nstart)
                 (< i end) (char= (char text i) #\/))
        (let ((num (parse-integer (subseq text nstart i)))
              (dstart (1+ i)))
          (setf i dstart)
          (loop while (and (< i end) (digit-char-p (char text i))) do (incf i))
          (when (> i dstart)
            (let ((den (parse-integer (subseq text dstart i))))
              (unless (zerop den)
                (return-from scan-ratio (values (/ num den) i nil)))))))))
    (values nil position nil))

(defrule ratio-number
    (and (function scan-ratio) not-symbol-tail-char)
  (:lambda (result)
    (destructuring-bind (val guard) result
      (declare (ignore guard))
      val)))

(defun scan-radix-integer (text position end)
  "esrap terminal: #b/#o/#x/#Nr integer with optional sign.
   Returns (values int new-pos nil), or failure. Validity (digit
   values below the base, base 2-36, at least one digit) is checked
   here so #xFFg fails instead of silently stopping at FF."
  (let ((i position))
    (when (and (< i end) (char= (char text i) #\#))
      (incf i)
      (let ((base nil))
        (cond ((and (< i end) (member (char text i) '(#\b #\B)))
               (setf base 2) (incf i))
              ((and (< i end) (member (char text i) '(#\o #\O)))
               (setf base 8) (incf i))
              ((and (< i end) (member (char text i) '(#\x #\X)))
               (setf base 16) (incf i))
              (t
               (let ((j i))
                 (loop while (and (< j end) (digit-char-p (char text j)))
                       do (incf j))
                 (when (and (> j i) (< j end)
                            (member (char text j) '(#\r #\R)))
                   (let ((b (parse-integer (subseq text i j))))
                     (when (<= 2 b 36)
                       (setf base b)
                       (setf i (1+ j))))))))
        (when base
          (let ((neg nil))
            (when (and (< i end) (member (char text i) '(#\+ #\-)))
              (setf neg (char= (char text i) #\-))
              (incf i))
            (let ((dstart i))
              (loop while (and (< i end)
                               (let ((v (digit-char-p (char text i) base)))
                                 (and v (< v base))))
                    do (incf i))
              (when (> i dstart)
                (let ((val (parse-integer (subseq text dstart i)
                                          :radix base)))
                  (return-from scan-radix-integer
                    (values (if neg (- val) val) i nil)))))))))
    (values nil position nil)))

(defrule radix-integer
    (and (function scan-radix-integer) not-symbol-tail-char)
  (:lambda (result)
    (destructuring-bind (val guard) result
      (declare (ignore guard))
      val)))

;;; A symbol may not start with a radix prefix: #b/#o/#x/#Nr always
;;; means integer-or-bust, so #xFFg and #10r must fail outright instead
;;; of degrading into a "#" symbol plus trailing forms.
(defrule radix-prefix
    (and "#" (or (or "b" "B" "o" "O" "x" "X")
                 (and (+ digit) (or "r" "R")))))

(defrule plain-integer
    (and integer-part not-symbol-tail-char)
  (:lambda (result)
    (first result)))

(defrule number
    (and (? sign-char) (or radix-integer
                           float-body
                           dot-exponent-float
                           int-with-exponent
                           leading-dot-float
                           trailing-dot-integer
                           ratio-number
                           plain-integer))
  (:lambda (result &bounds start end)
    (destructuring-bind (sign val) result
      (make-node :number
                 :value (if (and sign (string= sign "-")) (- val) val)
                 :start start :end end))))

;;; Character literal names. Rather than spelling out every mnemonic as
;;; grammar (the hand-written version needed 20 lines for the ASCII
;;; controls alone and still missed SBCL's 2-3 letter abbreviations), the
;;; accepted names live in a table and one esrap terminal looks them up.
;;; The set is what SBCL actually accepts for #\Nul / #\Soh / #\Ack /
;;; #\Dle / #\Nak / #\Vt ... plus the traditional short forms
;;; (Bel Esc Del Alt Sp Cr Lf Bs Ht) and the Maclisp-era extras
;;; (Delete, Altmode, Linefeed, Lock). Hyphenated names are NOT
;;; accepted — SBCL rejects those outright.
(defparameter *char-name-table*
  (let ((table (make-hash-table :test #'equal)))
    (dolist (name '("NUL" "NULL" "SOH" "STX" "ETX" "EOT" "ENQ" "ACK"
                    "BEL" "BELL" "BACKSPACE" "BS" "TAB" "HT" "NEWLINE"
                    "NL" "LINEFEED" "LF" "VT" "PAGE" "RETURN" "CR" "SO"
                    "SI" "DLE" "DC1" "DC2" "DC3" "DC4" "NAK" "SYN" "ETB"
                    "CAN" "EM" "SUB" "ESC" "ESCAPE" "FS" "GS" "RS" "US"
                    "SPACE" "SP" "RUBOUT" "DELETE" "DEL" "ALT" "ALTMODE"
                    "LOCK"))
      (setf (gethash name table) name))
    table))

(defun scan-char-name (text position end)
  "esrap terminal: one of SBCL's character-name mnemonics, matched
   case-insensitively. Returns (values upcased-name position-after nil),
   or (values nil position nil) when the run of letters is not a name."
  (let ((i position))
    ;; letters, then letters/digits: SBCL's control mnemonics include
    ;; "Dc4", so a name is not purely alphabetic
    (when (and (< i end) (alpha-char-p (char text i)))
      (loop while (and (< i end)
                       (or (alpha-char-p (char text i))
                           (digit-char-p (char text i))))
            do (incf i)))
    (if (= i position)
        (values nil position nil)
        (let ((name (string-upcase (subseq text position i))))
          (if (gethash name *char-name-table*)
              (values name i nil)
              (values nil position nil))))))

(defrule char-known-name
  (function scan-char-name))

;;; What can EXTEND a character name past its first char (SBCL-probed
;;; over every printable ASCII follower). Only whitespace, the
;;; delimiters " ' ( ) , ; ` and EOF end the name; everything else
;;; continues it, so "#\AB", "#\A{" and "#\ |" must NOT read as
;;; singles. A trailing backslash extends too (hence not "\X" but "\").
(defrule char-name-continue
    (or alphanumeric
        #\\
        #\! #\# #\$ #\% #\& #\* #\+ #\- #\. #\/ #\: #\< #\= #\>
        #\? #\@ #\[ #\] #\^ #\_ #\{ #\} #\| #\~))

;;; Whitespace that may follow "#\" directly (the Space/Tab/... char).
;;; The char AFTER it must be innocent (whitespace, EOF, or an opening
;;; delimiter/quote/comment starter) — otherwise SBCL reads on into an
;;; unknown name (" #)", " a") and errors.
(defrule char-ws-follow-ok
    (or #\Space #\Tab #\Newline #\Page #\Return
        (! character)
        #\( #\) #\" #\' #\` #\, #\;))

(defrule char-ws-name
    (and (or #\Space #\Tab #\Newline #\Page #\Return)
         (& char-ws-follow-ok))
  (:lambda (parts)
    (first parts)))

;;; Any other single character is the literal itself: printable
;;; punctuation, backslash, control codes and non-alphanumeric Unicode
;;; alike. The lookahead is what forbids "#\AB" — the "A" is consumed,
;;; then "B" would extend the name.
(defrule char-graphic
    (and character (! char-name-continue))
  (:lambda (parts)
    (first parts)))

;;; A known name only counts when nothing extends it, so "#\Space!"
;;; and "#\Nulx" fail instead of degrading to a char plus a symbol.
(defrule char-known-stopped
    (and char-known-name (! char-name-continue))
  (:lambda (parts)
    (first parts)))

;;; Unicode character names as SBCL spells them, e.g.
;;; #\LATIN_SMALL_LETTER_E_WITH_ACUTE — underscore-separated words.
;;; SBCL validates these against its Unicode table and rejects made-up
;;; ones (#\Foo_Bar); this rule cannot, and is deliberately more
;;; permissive: reading a valid literal wrongly is the worse failure for
;;; an editor than accepting a name nobody writes. Hyphens are NOT part
;;; of the pattern — SBCL rejects those outright.
(defrule name-letter-or-digit
    (or alpha digit))

(defrule name-word
    (+ name-letter-or-digit))

(defrule name-word-pair
    (and #\_ name-word))

(defrule char-unicode-name
    ;; two or more words joined by single underscores. Written as
    ;; word (word)* rather than one greedy run: a repetition never
    ;; gives characters back, so "(+ (or alpha digit #\_))" would
    ;; swallow the underscores and leave nothing for the rest.
    (and name-word (+ name-word-pair))
  (:lambda (parts)
    (string-upcase (esrap:text parts))))

(defrule char-unicode-stopped
    (and char-unicode-name (! char-name-continue))
  (:lambda (parts)
    (first parts)))

;;; SBCL also reads a character by code point: #\U+DF, #\u+DF, #\uDF.
;;; Not standard CL, but it appears in real code (SBCL's own encoding
;;; tests, anything generated from Unicode data).
(defrule hex-digit
    (or digit (character-ranges (#\a #\f) (#\A #\F))))

(defrule char-code-point
    (and (or #\u #\U) (? #\+) (+ hex-digit))
  (:lambda (parts)
    (destructuring-bind (prefix plus digits) parts
      (declare (ignore prefix plus))
      ;; digits arrive as one production per hex digit
      (code-char (parse-integer (esrap:text digits) :radix 16)))))

(defrule char-code-point-stopped
    (and char-code-point (! char-name-continue))
  (:lambda (parts)
    (first parts)))

(defrule char-literal
    (and "#\\" (or char-known-stopped char-unicode-stopped
                   char-code-point-stopped char-ws-name char-graphic))
  (:lambda (parts &bounds start end)
    ;; Known names arrive upcased from their rule; singles keep exact
    ;; case (#\a stays "a").
    (destructuring-bind (prefix value) parts
      (declare (ignore prefix))
      (make-node :char :value (if (stringp value) value (string value))
                 :start start :end end))))

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

;;; The reader treats ANY non-ASCII character as a symbol constituent,
;;; not just the alphanumeric ones: "§derpy", "«x«" and "→y" are single
;;; symbols in SBCL. ASCII is spelled out below because that is what the
;;; standard defines; everything above code point 127 comes from here.
(defun scan-unicode-constituent (text position end)
  "esrap terminal: one non-ASCII character, a symbol constituent."
  (if (and (< position end)
           (> (char-code (char text position)) 127))
      (values (string (char text position)) (1+ position) nil)
      (values nil position nil)))

(defrule unicode-constituent
  (function scan-unicode-constituent))

(defrule symbol-head-char
    (or alpha digit unicode-constituent
        #\. #\- #\* #\+ #\! #\? #\_ #\= #\< #\> #\& #\/ #\~ #\@ #\$ #\% #\^ #\: #\# #\| #\` #\,
        ;; a name may START with a bracket: "[1]" is |[1]|, not a list
        #\[ #\] #\{ #\}))

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

(defun scan-bar-body (text position end)
  "esrap terminal: the inside of a |...| symbol, stopping at the first
   unescaped bar so the whole span costs one production."
  (let ((i position))
    (loop while (< i end) do
      (let ((ch (char text i)))
        (cond
          ((char= ch #\|) (return))
          ((char= ch #\\) (incf i 2))
          (t (incf i)))))
    ;; stop AT the closing bar; bar-segment has its own rule for it
    (if (and (< i end) (char= (char text i) #\|))
        (values (subseq text position i) i nil)
        (values nil position "Unterminated |...| symbol"))))

(defrule bar-segment
    (and #\| (function scan-bar-body) #\|)
  (:lambda (parts)
    ;; esrap:text keeps the surrounding bars, which are part of the name
    (esrap:text parts)))

(defrule symbol-tail-char
    ;; [ ] { } are constituent characters, not delimiters (CLHS 2.1.3),
    ;; so they continue a name just like #\. does
    (or symbol-escape bar-segment symbol-head-char #\. #\[ #\] #\{ #\}))

(defrule symbol-head
    (or symbol-escape bar-segment symbol-head-char))

(defrule symbol-body
    (+ symbol-tail-char)
  (:lambda (chars)
    (esrap:text chars)))

(defrule reader-conditional-prefix
    ;; "#+" / "#-" introduce a feature conditional, never a symbol.
    ;; Without this guard "#+sbcl" parsed as one symbol and the following
    ;; form became a second top-level form.
    (or "#+" "#-"))

(defun unescape-symbol-text (text)
  "Drop the backslash of every \\x escape, which is what the reader
   does: the name of some\\!thing is \"SOME!THING\". Escapes never occur
   inside a |...| bar segment, whose contents are already literal."
  (let ((out (make-string-output-stream))
        (i 0)
        (n (length text))
        (bar nil))
    (loop while (< i n) do
      (let ((c (char text i)))
        (cond (bar
               (write-char c out)
               (when (char= c #\|) (setf bar nil)))
              ((char= c #\|)
               (write-char c out)
               (setf bar t))
              ((and (char= c #\\) (< (1+ i) n))
               (write-char (char text (1+ i)) out)
               (incf i))
              (t (write-char c out))))
      (incf i))
    (get-output-stream-string out)))

(defun unescaped-colon-position (text)
  "Index of the last colon that really separates package from name, or
   NIL. A \\: is part of the name, and so is a colon inside |...|."
  (let ((i 0)
        (n (length text))
        (bar nil)
        (found nil))
    (loop while (< i n) do
      (let ((c (char text i)))
        (cond ((and (char= c #\\) (< (1+ i) n)) (incf i))
              ((char= c #\|) (setf bar (not bar)))
              ((and (not bar) (char= c #\:)) (setf found i))))
      (incf i))
    found))

(defrule symbol
    ;; A "#\" prefix always means char-literal-or-bust: without this
    ;; guard an invalid "#\AB" would degrade into a "#" symbol plus
    ;; trailing forms instead of failing the enclosing form.
    ;; Likewise "#(" always means vector-or-bust: without this guard a
    ;; broken "#(a . b)" degrades into a "#" symbol plus a list instead
    ;; of failing, disagreeing with the reader. And a radix prefix
    ;; always means integer-or-bust, so #xFFg cannot degrade either.
    (and (! "#\\")
         (! "#(")
         (! radix-prefix)
         (! reader-conditional-prefix)
         symbol-head
         (* symbol-tail-char))
  (:lambda (chars &bounds start end)
    (let ((full (esrap:text chars)))
      ;; Split on the LAST unescaped colon so "foo::bar" yields package
      ;; "foo", name "bar" (not ":bar"), while "foo\\:bar" stays a
      ;; single symbol named "foo:bar". Leading ":" means keyword.
      (let ((colon (unescaped-colon-position full)))
        (if colon
            (let ((raw-pkg (subseq full 0 colon))
                  (raw-name (subseq full (1+ colon))))
              (make-node :symbol
                         :name (string-left-trim ":"
                                                 (unescape-symbol-text raw-name))
                         :package (let ((trimmed (string-trim ":"
                                                           (unescape-symbol-text
                                                            raw-pkg))))
                                    (if (> (length trimmed) 0)
                                        trimmed
                                        "KEYWORD"))
                         :start start :end end))
            (make-node :symbol
                       :name (unescape-symbol-text full)
                       :start start :end end))))))

;;; --- Compound rules ---

;;; List (parenthesized form).
;;;
;;; Only "(" and ")" delimit a list. CLHS 2.1.3 also calls [ ] { } open and
;;; close brackets, but it makes them constituent characters, and the
;;; reader honours that: "[1]" reads as the SYMBOL |[1]|, "{a}b" as
;;; |{A}B|, and "(let ([x 1]) ...)" does NOT bind x. Treating them as
;;; delimiters here would silently disagree with every reader, so they
;;; go to `symbol-head-char'/`symbol-tail-char' instead.
;;;
;;; Dotted pairs need real structure, not just "dot as another symbol":
;;; the reader treats (a . (b)) as the two-list (a b), so a flat
;;; three-child (a DOT (b)) tree silently disagrees about shape. Worse,
;;; (a . b c) and (a .) must FAIL (the reader rejects them) but a bare
;;; repetition accepts them. Hence two rules: a proper list whose
;;; elements can never be a lone dot, and a dotted list with exactly
;;; one dot before exactly one tail form.

;;; A "." that stands alone: dot followed by a non-constituent, so
;;; ".5", ".." and ".b" (number/symbol continuations) do not match.
(defrule dot-form
    (and #\. (! symbol-tail-char))
  (:destructure (dot guard &bounds start end)
    (declare (ignore dot guard))
    (make-node :symbol :name "." :start start :end end)))

(defrule proper-list-form
    (and #\( ws (* (and (! dot-form) form ws)) ws #\))
  (:destructure (open ws1 forms-ws ws2 close &bounds start end)
    (declare (ignore open close ws1 ws2))
    (make-node :list
               :children (mapcar #'second forms-ws)
               :start start :end end)))

(defun nil-symbol-p (node)
  "True for the plain symbol NIL, which the reader treats as the empty
   list -- but not for #:nil or a NIL shadowed inside a bar segment."
  (and (eq (cl-toolkit-ast:node-type node) :symbol)
       (null (cl-toolkit-ast::node-package node))
       (string-equal (cl-toolkit-ast::node-name node) "nil")))

(defun dotted-kids-p (kids)
  "True when KIDS is a non-empty list whose last two elements are a
   lone dot and NIL, i.e. the reader's proper-list-in-disguise
   spelling (b . nil)."
  (and (>= (length kids) 2)
       (let ((penultimate (nth (- (length kids) 2) kids))
             (last (car (last kids))))
         (and (eq (cl-toolkit-ast:node-type penultimate) :symbol)
              (string= (cl-toolkit-ast::node-name penultimate) ".")
              (nil-symbol-p last)))))

(defrule dotted-list-form
    ;; At least one element must precede the dot: (. b) is not a list.
    ;; The tail itself must not be a lone dot: (a . .) is rejected by
    ;; the reader, while (a . .5) and (a . .b) stay legal
    (and #\( ws (+ (and (! dot-form) form ws)) ws dot-form ws
         (! dot-form) form ws #\))
  (:destructure (open ws1 prefixes ws2 dot ws3 tail-guard tail ws4 close
                 &bounds start end)
    (declare (ignore open close ws1 ws2 ws3 ws4 tail-guard))
    (let ((prefix-kids (mapcar #'second prefixes)))
      (make-node :list
                 :children (cond ((eq (cl-toolkit-ast:node-type tail) :list)
                                   ;; (a . (b c)) is the three-list (a b c):
                                   ;; splice the tail's children in place,
                                   ;; and drop a trailing ". nil" because
                                   ;; (a . (b . nil)) is just (a b)
                                   (let ((kids (cl-toolkit-ast:node-children tail)))
                                     (append prefix-kids
                                             (if (dotted-kids-p kids)
                                                 (butlast kids 2)
                                                 kids))))
                                  ;; NIL is the empty list, so (a . nil)
                                  ;; is the one-list (a) -- common in
                                  ;; iteration macros like (for (x . nil) in y)
                                  ((nil-symbol-p tail) prefix-kids)
                                  ;; (a . b) keeps the dot explicit, as before
                                  (t (append prefix-kids (list dot tail))))
                 :start start :end end))))

(defrule list-form
    (or dotted-list-form proper-list-form))

;;; Vector. Elements can never be a lone dot: the reader rejects
;;; #(a . b), so the repetition excludes it the same way proper lists do.
(defrule vector-form
    (and "#(" ws (! dot-form) form (* (and ws (! dot-form) form)) ws #\))
  (:destructure (open ws1 first-guard first rest ws2 close
                 &bounds start end)
    (declare (ignore open close ws1 ws2 first-guard))
    (make-node :vector
               :children (cons first (mapcar #'third rest))
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
(defrule feature-target
  ;; try the real grammar first so live branches keep a proper AST
  (or form skip-form))

(defrule feature-form
    (and (or "#+" "#-") ws form ws feature-target)
  (:destructure (marker ws1 feat ws2 target &bounds start end)
    (declare (ignore ws1 ws2))
    (make-node :list
               :children (list (make-node :symbol :name marker
                                          :start start
                                          :end (+ start 2))
                               feat
                               ;; `form' yields a node, `skip-form' raw text
                               (if (stringp target)
                                   (make-node :symbol
                                              :name (if (plusp (length target))
                                                        target "<skipped>")
                                              :start (cl-toolkit-ast:node-end feat)
                                              :end end)
                                   target))
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
    (declare (ignore hash letter ws))
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

;;; Deep nesting and long files used to be fatal. esrap's PEG engine
;;; recurses per bracket level and accumulates state per repetition, so
;;; ~1500 nested parens overflowed the control stack and ~20000
;;; top-level forms exhausted the heap. Neither is catchable: SBCL dies
;;; on the guard page. So the depth is measured up front by an iterative
;;; scan (no recursion), pathological input is refused with an ordinary
;;; :ERROR node, and the forms themselves are parsed one at a time so
;;; per-repetition state never accumulates across a whole file.

(defparameter *max-parse-depth*
  800
  "Maximum bracket nesting `parse-lisp-source' will attempt.
   esrap descends recursively per bracket level, so deep nesting can
   run off the end of the control stack — and a stack overflow is not
   catchable, it just kills the process (measured cliff: fine at 1000
   levels, guard-page fault at 1200). 800 keeps a comfortable margin
   while accepting anything realistic; deeper input gets a clear
   :ERROR node instead of a segfault.")

(defun whitespace-char-p (ch)
  "True for the whitespace characters of CLHS 2.1.1."
  (or (char= ch #\Space) (char= ch #\Tab) (char= ch #\Newline)
      (char= ch #\Return) (char= ch #\Page)))

(defun scan-source (text start end)
  "Single iterative pass over TEXT between START and END.
   Returns (values max-depth cut-points).
   MAX-DEPTH is the deepest bracket nesting. CUT-POINTS are ascending
   offsets at which the text may be cut without splitting a form: any
   whitespace that sits outside every bracket, string, comment and
   |...| symbol. The pass is iterative, so it costs no stack.
   Strings, line comments, nested block comments, |...| symbols and
   backslash escapes are all treated as opaque."
  (let ((i start)
        (depth 0)
        (max-depth 0)
        (block-stack nil)      ; block-comment depth outside each bracket
        (block-depth 0)
        (mode :normal)
        (cuts '()))
    (labels ((at-clean-state-p ()
               (and (zerop depth)
                    (zerop block-depth)
                    (eq mode :normal))))
      (loop while (< i end) do
        (let ((ch (char text i))
              (nxt (and (< (1+ i) end) (char text (1+ i)))))
          (if (and (at-clean-state-p) (whitespace-char-p ch))
              (push i cuts))
          (cond
            ((eq mode :line-comment)
             (when (char= ch #\Newline) (setf mode :normal)))
            ((eq mode :string)
             (cond ((char= ch #\\) (when (< (1+ i) end) (incf i)))
                   ((char= ch #\") (setf mode :normal))))
            ((eq mode :bar)
             (cond ((char= ch #\\) (when (< (1+ i) end) (incf i)))
                   ((char= ch #\|) (setf mode :normal))))
            ((plusp block-depth)
             ;; Inside #| ... |# EVERYTHING is literal text: quotes,
             ;; semicolons, bars and brackets are not structure. Only a
             ;; nested block comment matters.
             (cond ((and nxt (char= ch #\#) (char= nxt #\|))
                    (incf block-depth)
                    (incf i))
                   ((and nxt (char= ch #\|) (char= nxt #\#))
                    (decf block-depth)
                    (incf i))))
            ((char= ch #\;) (setf mode :line-comment))
            ((char= ch #\") (setf mode :string))
            ;; a backslash escapes the next character: \( and \" are
            ;; literals, not structure
            ((char= ch #\\) (when (< (1+ i) end) (incf i)))
            ((and nxt (char= ch #\#) (char= nxt #\|))
             (incf block-depth)
             (incf i))
            ;; "|#" must be tested before a bare "|", or the bar looks
            ;; like the start of a |...| symbol
            ;; a bare "|" outside a block comment starts a |...| symbol;
            ;; the "|#" terminator was already handled in the block-depth
            ;; case above, so it must not be tested again here
            ((char= ch #\|) (setf mode :bar))
            ;; only "(" nests: [ ] { } are constituent characters
            ((char= ch #\()
             (push block-depth block-stack)
             (incf depth)
             (when (> depth max-depth) (setf max-depth depth)))
            ((char= ch #\))
             (when (plusp depth)
               (decf depth)
               (pop block-stack)))))
        (incf i))
      (values max-depth (nreverse cuts)))))

(defun skip-ws-and-comments (text pos end)
  "Skip whitespace, line comments and block comments from POS.
   Iterative, so a comment-only megabyte costs no stack."
  (let ((i pos))
    (loop
      (when (>= i end) (return i))
      (let ((ch (char text i)))
        (cond
          ((whitespace-char-p ch) (incf i))
          ((char= ch #\;)
           (loop while (and (< i end) (not (char= (char text i) #\Newline)))
                 do (incf i)))
          ((and (char= ch #\#) (< (1+ i) end) (char= (char text i) #\|))
           (let ((level 1))
             (incf i 2)
             (loop while (and (< i end) (plusp level)) do
               (cond ((and (char= (char text i) #\#)
                           (< (1+ i) end) (char= (char text (1+ i)) #\|))
                      (incf level) (incf i 2))
                     ((and (char= (char text i) #\|)
                           (< (1+ i) end) (char= (char text (1+ i)) #\#))
                      (decf level) (incf i 2))
                     (t (incf i))))))
          ;; a lone "#" or any other non-form character: stop and let the
          ;; caller report it
          (t (return i)))))))

(defun skip-one-form (text position end)
  "Iteratively find the end of the form starting at POSITION.
   Returns the position just past it, or NIL when nothing is consumed.
   Deliberately permissive: this only has to find a form's extent, not
   validate it."
  (let ((i (skip-ws-and-comments text position end)))
    (when (>= i end) (return-from skip-one-form nil))
    (let ((ch (char text i)))
      (cond
        ;; a quote-like macro applies to the form that follows
        ((or (char= ch #\') (char= ch #\`) (char= ch #\,))
         (skip-one-form text (1+ i) end))
        ((or (char= ch #\( ) (char= ch #\[) (char= ch #\{))
         (let ((start i) (depth 0))
           (loop while (< i end) do
             (let ((c (char text i)))
               (cond
                 ((char= c #\;)
                  (loop while (and (< i end)
                                   (not (char= (char text i) #\Newline)))
                        do (incf i)))
                 ((char= c #\")
                  (loop while (< i end) do
                    (incf i)
                    (when (char= (char text (1- i)) #\")
                      (return))))
                 ((and (char= c #\#) (< (1+ i) end) (char= (char text (1+ i)) #\|))
                  ;; nested block comment: run to its "|#"
                  (loop while (< i end) do
                    (incf i)
                    (when (and (< i end) (char= (char text i) #\|)
                               (> i start) (char= (char text (1- i)) #\#))
                      (return))))
                 ((char= c #\\) (incf i))
                 ((or (char= c #\( ) (char= c #\[) (char= c #\{)) (incf depth))
                 ((char= c #\))
                  (decf depth)
                  (when (zerop depth) (return)))))
             (incf i))
           (when (zerop depth) (max (1+ start) i))))
        (t
         ;; an atom: run to the next delimiter
         (loop while (and (< i end)
                          (not (whitespace-char-p (char text i)))
                          ;; [ ] { } are constituents, so an atom such as
                          ;; [foo] runs to the next real delimiter
                          (not (member (char text i) '(#\( #\) #\" #\;))))
               do (incf i))
         (when (> i position) i))))))

(defun skip-form-text (text position end)
  "esrap terminal: the raw text of one form, matched without validating
   it. A feature conditional's branch is never even read by the reader
   when the feature is absent, so a file may legitimately contain
   \"#-other-lisp #\\Name-Only-That-Lisp-Knows\" and still load fine."
  (let ((stop (skip-one-form text position end)))
    (if stop
        (values (subseq text position stop) stop nil)
        (values nil position nil))))

(defrule skip-form
  (function skip-form-text))

(defun too-deep-error-node (start end depth)
  (make-node :error
             :value (format nil
                            "Nesting too deep: ~d levels exceeds the ~d level parse limit"
                            depth *max-parse-depth*)
             :start start
             :end end))

(defun parse-chunk (text lo hi)
  "Parse TEXT[LO,HI) as one strict `source-file' match.
   Returns (values nodes error-node).  Error-node is NIL on success and
   then NODES holds the forms; esrap only succeeds when the rule
   consumes the WHOLE range, so success proves [LO,HI) holds nothing
   but complete forms."
  (handler-case
      (let ((ast (esrap:parse 'source-file text :start lo :end hi)))
        (values (cl-toolkit-ast:node-children ast) nil))
    (esrap:esrap-parse-error (c)
      (values nil
              (make-node :error
                         :value (compact-parse-error c)
                         :start lo
                         :end hi)))))

(defun parse-forms-from (text lo text-end cuts)
  "Collect the forms of TEXT from LO to TEXT-END.
   CUTS are ascending candidate end offsets from `scan-source'.  Each
   candidate is accepted only when esrap parses the whole range as
   complete forms; when one does not, POS stays put and the next
   candidate extends the range, because whitespace alone does not
   prove a form boundary (the space in \"#+sbcl (a)\" precedes the
   macro's argument).  Whatever is left after the last candidate is
   parsed as one final range.
   Returns (values forms error-node)."
  (let ((forms '())
        (error-node nil)
        (pos lo))
    (dolist (cand cuts)
      (unless (or error-node (<= cand pos))
        (multiple-value-bind (nodes err) (parse-chunk text pos cand)
          (when (null err)
            (dolist (node nodes) (push node forms))
            (setf pos cand)))))
    (unless (or error-node (>= pos text-end))
      (multiple-value-bind (nodes err) (parse-chunk text pos text-end)
        (if (null err)
            (progn
              (dolist (node nodes) (push node forms))
              (setf pos text-end))
            (setf error-node err))))
    (values (nreverse forms) error-node)))

(defun parse-lisp-source (text &optional (start 0) end)
  "Parse TEXT as Lisp source code. Returns AST root node.
   START and END are optional bounds into TEXT.
   A single `source-file' match over a whole file accumulates esrap
   state per top-level form, which exhausted the heap on large inputs
   and overflowed the control stack on deep ones (neither is catchable).
   So the text is scanned once, iteratively: too-deep nesting is
   refused up front, and the rest is handed to esrap in chunks whose
   ends esrap itself proves to be form boundaries. A syntax error
   anywhere still yields an :ERROR root, as before; use
   `parse-with-recovery' to keep the good forms."
  (let ((text-end (or end (length text)))
        (*standard-output* (make-broadcast-stream))
        (*error-output* (make-broadcast-stream)))
    (multiple-value-bind (depth cuts) (scan-source text start text-end)
      (if (> depth *max-parse-depth*)
          (too-deep-error-node start text-end depth)
          (multiple-value-bind (forms error-node)
              (parse-forms-from text start text-end cuts)
            (or error-node
                (make-node :list
                           :children forms
                           :source "source-file"
                           :start start
                           :end text-end)))))))

;;; ============================================================
;;; Error Recovery Parser
;;; ============================================================
;;; Parses Lisp source with error recovery. When parsing fails,
;;; creates ERROR nodes for malformed regions and continues.

(defun find-next-form-boundary (text pos end)
  "Find the position after the next form boundary starting from POS.
   Bar-quoted |...| spans and \\ escapes are opaque (a ) inside |a)|
   must not end the scan early)."
  (when (>= pos end) (return-from find-next-form-boundary end))
  (let ((depth 0) (in-string nil) (in-comment nil) (block-depth 0) (in-bar nil))
    (loop for i from pos below end
          for ch = (char text i)
          do (cond
               (in-comment
                (when (char= ch #\Newline) (setf in-comment nil)))
               (in-string
                (when (char= ch #\") (setf in-string nil))
                (when (and (char= ch #\\) (< (1+ i) end)) (incf i)))
               (in-bar
                (when (char= ch #\|) (setf in-bar nil))
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
                  (#\| (setf in-bar t))
                  (#\\ (when (< (1+ i) end) (incf i)))
                  ;; only parens delimit; brackets are constituents
                  (#\( (incf depth))
                  (#\) (if (zerop depth) (return (1+ i)) (decf depth)))
               (#\# (when (and (< (1+ i) end) (char= (char text (1+ i)) #\|))
                          (incf block-depth) (incf i))
                     ;; skip #\NAME / #\X char literals whole so a #\| never
                     ;; looks like a bar-symbol opener below
                     (when (and (< (1+ i) end) (char= (char text (1+ i)) #\\))
                       (let ((ci (+ i 2)))
                         (if (and (< ci end) (alphanumericp (char text ci)))
                             (loop while (and (< ci end)
                                              (alphanumericp (char text ci)))
                                   do (incf ci))
                             (when (< ci end) (incf ci)))
                         (setf i (1- ci))))))))
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
