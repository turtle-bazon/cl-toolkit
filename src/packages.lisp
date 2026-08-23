(defpackage #:cl-toolkit-ast
  (:use #:cl)
  (:export
   #:make-node
   #:node-type
   #:node-start
   #:node-end
   #:node-line
   #:node-col
   #:node-source
   #:node-children
   #:node-value
   #:node-name
   #:node-package
   #:nodep
   #:node-list-p
   #:node-atom-p
   #:node-error-p
   #:node-form-count
   #:node-form-name
   #:offset-to-line-col
   #:offset-to-line-col-inverse
   #:node-to-json
   #:node-to-json-string
   #:escape-json-string))

(defpackage #:cl-toolkit-grammar
  (:use #:cl #:esrap)
  (:import-from #:parser.common-rules
                #:whitespace
                #:whitespace*
                #:whitespace+
                #:lisp-style-comment)
  (:import-from #:cl-toolkit-ast
                #:make-node)
  (:import-from #:esrap
                #:esrap-parse-error-result
                #:result-position
                #:successful-parse-production)
  ;; NOTE: analyze-balance / format-source are implemented in CL-TOOLKIT
  ;; (parser.lisp); exporting them here too created symbol clashes for
  ;; packages :using both. Do not re-add them to this export list.
  (:export
   #:parse-lisp-source
   #:parse-with-recovery))

(defpackage #:cl-toolkit
  (:use #:cl #:clingon)
  (:import-from #:cl-toolkit-ast
                #:make-node
                #:node-type
                #:node-start
                #:node-end
                #:node-line
                #:node-col
                #:node-source
                #:node-children
                #:node-value
                #:node-name
                #:nodep
                #:node-list-p
                #:node-error-p
                #:node-to-json
                #:node-to-json-string
                #:node-form-name
                #:node-form-count)
  (:export
   #:parse-file
   #:parse-string
   #:find-form-at
   #:extract-range
   #:validate
   #:list-top-level
   #:analyze-balance
   #:format-source
   #:node-to-json
   #:node-to-json-string
   #:delete-form-at
   #:delete-top-level-at
   #:insert-form-at
   #:insert-form-end
    #:replace-form-at
    #:replace-form-pretty
    #:find-top-level-by-name
    #:delete-node-from-text
    #:move-form
   #:source-of-top-level
   #:find-subform-matching
   #:find-forms-containing
   #:count-text-occurrences
   #:find-subform-matching-exact
   #:node-at-path
   #:split-string-on-char
   #:net-depth-delta
   #:duplicate-top-level-forms
   #:node-source-text
   #:split-jammed-top-level
   #:splice-replacement
   #:top-level-node-at
   #:apply-batch-edits
   #:apply-single-edit
   #:apply-edit
   #:cl-toolkit-main
   #:main))
