;;;; cmake-parser.asd

(asdf:defsystem #:cmake-parser
  :description "A cmake script parser."
  :author "zbq"
  :license  "MIT"
  :version "0.1.0"
  :serial t
  :depends-on (#:esrap)
  :components ((:file "package")
               (:file "cmake-parser")
			   (:file "example")))
