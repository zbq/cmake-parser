;;;; example.lisp

(in-package #:cmake-parser)

(defun test-crlf ()
  (let* ((str (concatenate 'string "add(1 2)" '(#\return #\linefeed) "sub(2 3) #hello"))
		 (prod (parse-string str)))
	(assert (= (length prod) 2))
	(assert (= (length (first prod)) 3)) ; add 1 2
	(assert (string= (first (first prod)) "add"))
	(assert (string= (second (first prod)) "1"))
	(assert (string= (third (first prod)) "2"))

	(assert (= (length (second prod)) 3)) ; sub 2 3
	(assert (string= (first (second prod)) "sub"))
	(assert (string= (second (second prod)) "2"))
	(assert (string= (third (second prod)) "3"))))

(defun test-cmd-invocation ()
  (let* ((str "
# hello
#add(test arg1 arg2)
add1 ( test arg1 arg2) #hello again
add2 ( arg1 arg2 arg3;arg4)
add3( arg1 arg2#hello
arg3 # hello
#hello again
arg4)
")
		 (prod (parse-string str))
		 tmp)
	(assert (= (length prod) 3))
	(setf tmp (nth 0 prod))
	(assert (= (length tmp) 4))
	(assert (string= (nth 0 tmp) "add1"))
	(assert (string= (nth 3 tmp) "arg2"))
	(setf tmp (nth 1 prod))
	(assert (= (length tmp) 5))
	(assert (string= (nth 0 tmp) "add2"))
	(assert (string= (nth 3 tmp) "arg3"))
	(assert (string= (nth 4 tmp) "arg4"))
	(setf tmp (nth 2 prod))
	(assert (= (length tmp) 5))
	(assert (string= (nth 2 tmp) "arg2"))
	(assert (string= (nth 4 tmp) "arg4"))))

(defun test-quoted-arg ()
  (let* ((str "ADD(\"arg1 arg\" \"arg2\\\"arg\\;arg\")")
		 (prod (parse-string str))
		 tmp)
	(assert (= (length prod) 1))
	(setf tmp (nth 0 prod))
	(assert (= (length tmp) 3))
	(assert (string= (nth 1 tmp) "arg1 arg"))
	(assert (string= (nth 2 tmp) "arg2\"arg;arg"))))

; \r \t \n \;
(defun test-quoted-encoded ()
  (let* ((str "
ADD(\"\\t\\r\\n\\;\")")
		 (prod (parse-string str))
		 tmp)
	(assert (= (length prod) 1))
	(setf tmp (nth 0 prod))
	(assert (= (length tmp) 2))
	(assert (string= (nth 1 tmp) (concatenate 'string '(#\tab #\return #\linefeed #\;))))
	))

(defun test-unquoted-encoded ()
  (let* ((str "add(arg1\\ arg arg2\\;arg)")
		 (prod (parse-string str))
		 tmp)
	(assert (= (length prod) 1))
	(setf tmp (nth 0 prod))
	(assert (= (length tmp) 3))
	(assert (string= (nth 1 tmp) "arg1 arg"))
	(assert (string= (nth 2 tmp) "arg2;arg"))))

(defun test-quote-continuation ()
  (let* ((str "add(\"hello\\
world\\

!\")")
		 (prod (parse-string str))
		 tmp)
	(setf tmp (nth 0 prod))
	(assert (string= (nth 1 tmp) "helloworld
!"))))

(defun test-expand-arg ()
  (let ((binding (make-hash-table :test 'equal)))
	(assert (string= (expand-argument "abc" binding) "abc"))
	(assert (string= (expand-argument "a${b}c" binding) "ac"))
	(setf (gethash "b" binding) "B")
	(assert (string= (expand-argument "a${b}c" binding) "aBc"))
	(setf (gethash "xBx" binding) 1024)
	(assert (string= (expand-argument "a${x${b}x}c" binding) "a1024c"))
	(assert (string= (expand-argument "a${b${xBx}c" binding) "a${b1024c"))
	(assert (string= (expand-argument "a{}b${b}c{}d" binding) "a{}bBc{}d"))))

(defun test-common-usage ()
  (let ((binding (make-hash-table :test 'equal))
		(prod (parse-string "SET(TARGET_NAME \"HelloWorld\")
ADD_EXECUTABLE(${TARGET_NAME} hello.cpp world.cpp)")))
	(loop for call in prod
	   do (cond
			((string-equal "SET" (first call))
			 (setf (gethash (second call) binding) (third call)))
			((string-equal "ADD_EXECUTABLE" (first call))
			 (format t "bin: ~A, src: ~{~A ~}~%"
					 (expand-argument (second call) binding)
					 (cddr call)))
			(t (format t "Not Supported Command Invocation"))))))

(defun test-all ()
  (test-crlf)
  (test-cmd-invocation)
  (test-quoted-arg)
  (test-quoted-encoded)
  (test-unquoted-encoded)
  (test-expand-arg)
  (test-quote-continuation)
  (test-common-usage))
