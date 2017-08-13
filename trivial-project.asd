;;
;;
(asdf:defsystem #:trivial-project
  :description "A simple project skeleton generator with key-value substitution"
  :author "StackSmith <fpgasm@apple2.x10.mx>"
  :license "BSD Simplified (2-clause)"
  :serial t
  :depends-on (#:cl-ppcre #:alexandria)
  :components ((:file "package")
	       (:file "actions")
	       (:file "trivial-project")))

