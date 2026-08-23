(defsystem #:cl-toolkit
  :description "Lisp code toolkit for structural analysis and editing"
  :author "cl-agent-validate"
  :license "GPL-3.0"
  :version "0.4.1"
  :depends-on (#:alexandria
               #:esrap
               #:parser.common-rules
               #:cl-json
               #:clingon)
  :serial t
  :components ((:module "src"
                :serial t
                :components ((:file "packages")
                             (:file "ast")
                             (:file "grammar")
                             (:file "parser")
                             (:file "cli")))))

(defsystem #:cl-toolkit/bin
  :description "cl-toolkit binary build"
  :depends-on (#:cl-toolkit)
  :build-operation program-op
  :build-pathname "cl-toolkit"
  :entry-point "cl-toolkit:cl-toolkit-main")

(defsystem #:cl-toolkit/tests
  :description "cl-toolkit tests"
  :depends-on (#:cl-toolkit #:fiveam)
  :serial t
  :components ((:module "test"
                :serial t
                :components ((:file "tests")))))
