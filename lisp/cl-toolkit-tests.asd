(defsystem #:cl-toolkit/tests
  :description "Tests for cl-toolkit"
  :depends-on (#:cl-toolkit #:fiveam)
  :serial t
  :components ((:file "tests")))

(defmethod asdf:perform ((o asdf:test-op) (c (eql (asdf:find-system :cl-toolkit))))
  (asdf:operate 'asdf:test-op :cl-toolkit/tests))
