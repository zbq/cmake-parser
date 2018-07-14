;;;; package.lisp

(defpackage #:cmake-parser
  (:use #:cl)
  (:export #:parse-file
		   #:parse-string
		   #:expand-argument
		   #:grammar
		   #:test-all))

