;;
;;
(asdf:defsystem #:trivial-project
  :description "A simple project skeleton generator with key-value substitution"
  :author "StackSmith <fpgasm@apple2.x10.mx>"
  :license "BSD Simplified (2-clause)"
  :serial t
  :version "1.0.0"
  :depends-on (#:cl-ppcre #:alexandria)
  :components ((:file "package")
	       (:file "params")
	       (:file "trivial-project")))

