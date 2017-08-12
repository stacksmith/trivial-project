;;
;;
(asdf:defsystem #:trivial-project
  :description "An extremely simple project skeleton generator"
  :author "StackSmith <fpgasm@apple2.x10.mx>"
  :license "BSD Simplified (2-clause)"
  :serial t
  :depends-on (#:cl-ppcre #:alexandria)
  :components ((:file "package")
	       (:file "trivial-project")))

