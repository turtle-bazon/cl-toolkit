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
  (:export
   #:parse-lisp-source
   #:parse-with-recovery
   #:analyze-balance
   #:format-source))

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
                #:node-to-json-string)
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
   #:move-form
   #:apply-edit
   #:cl-toolkit-main
   #:main))
