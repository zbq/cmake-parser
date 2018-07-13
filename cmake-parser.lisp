;;;; cmake-parser.lisp

(in-package #:cmake-parser)

(defvar *bracket-argument-parsing* nil)
(defvar *bracket-open-=-len* 0)
(defvar *bracket-close-]-got* nil)
(defvar *bracket-close-=-len* 0)

(esrap:defrule file-elements (esrap:* file-element))

(esrap:defrule file-element (or (and command-invocation (and (esrap:* space)
															 line-ending))
								(and (esrap:* (or bracket-comment space))
									 line-ending))
  (:destructure (cmd ending)
				(declare (ignore ending))
				cmd))

(esrap:defrule line-ending (and (esrap:? line-comment) newline)
  (:constant nil))

(esrap:defrule space (esrap:+ (or #\space #\tab)))

(esrap:defrule newline (or #\return
						   #\linefeed
						   (and #\return #\linefeed)))

(esrap:defrule command-invocation (and (esrap:* space)
									   identifier
									   (esrap:* space)
									   #\( arguments #\))
  (:destructure (s1 id s2 left args right)
				(declare (ignore s1 s2 left right))
				(cons id args)))

(defun identifier-first-char? (char)
  (or (alpha-char-p char) (char= char #\_)))

(defun identifier-rest-char? (char)
  (or (alphanumericp char) (char= char #\_)))

(esrap:defrule identifier (and (identifier-first-char? character)
							   (esrap:* (identifier-rest-char? character)))
  (:text t))

(esrap:defrule arguments (and (esrap:? argument)
							  (esrap:* sep-arguments))
  (:destructure (arg seps)
				(let ((seps (remove nil seps)))
				  (if arg (cons arg seps) seps))))

(esrap:defrule sep-arguments (or sep-arguments1 sep-arguments2))

(esrap:defrule sep-arguments1 (and (esrap:+ separation)
								   (esrap:? argument))
  (:destructure (sep arg)
				(declare (ignore sep))
				arg))

(esrap:defrule sep-arguments2 (and (esrap:* separation)
								   #\( arguments #\))
  (:destructure (sep left args right)
				(declare (ignore sep left right))
				args))

(esrap:defrule separation (or space line-ending))

(esrap:defrule argument (or bracket-argument quoted-argument unquoted-argument))

(esrap:defrule bracket-argument (and bracket-open bracket-close)
  (:text t))

(defun parsing-bracket-argument (str)
  (setf *bracket-argument-parsing* t)
  (setf *bracket-open-=-len* (length (cadr str)))
  (setf *bracket-close-]-got* nil)
  (setf *bracket-close-=-len* 0)
  t)

(esrap:defrule bracket-open (parsing-bracket-argument (and #\[ (esrap:* #\=) #\[))
  (:constant nil))

(defun bracket-argument-char? (char)
  (cond
	((not *bracket-argument-parsing*) nil)
	((char= char #\]) (if *bracket-close-]-got*
						  (if (= *bracket-open-=-len* *bracket-close-=-len*)
							  (setf *bracket-argument-parsing* nil)
							  (setf *bracket-close-=-len* 0))
						  (progn
							(setf *bracket-close-]-got* t)
							(setf *bracket-close-=-len* 0)))
	 t)
	((char= char #\=) (when *bracket-close-]-got*
						(incf *bracket-close-=-len*))
	 t)
	(t (setf *bracket-close-]-got* nil)
	   t)))

(esrap:defrule bracket-argument-char (bracket-argument-char? character))

(defun bracket-close-found? (str)
  (declare (ignore str))
  (not *bracket-argument-parsing*))

(esrap:defrule bracket-close (bracket-close-found? (esrap:+ bracket-argument-char))
  (:lambda (chs)
	(subseq chs 0 (- (length chs) 2 *bracket-open-=-len*)))) ; trim ]=*]

(esrap:defrule quoted-argument (and #\" (esrap:* quoted-element) #\")
  (:text t))

(esrap:defrule quoted-element (or (not (or #\\ #\"))
								  escape-sequence
								  quoted-continuation))

(esrap:defrule quoted-continuation (and #\\ newline))

(esrap:defrule unquoted-argument (esrap:+ unquoted-element)
  (:text t))

(esrap:defrule unquoted-element (or (not (or space #\( #\) #\# #\" #\\))
									escape-sequence))

(esrap:defrule escape-sequence (or escape-identity
								   escape-encoded
								   escape-semicolon))

(defun escape-identity-char? (char)
  (not (or (alphanumericp char) (char= char #\;))))

(esrap:defrule escape-identity (and #\\ (escape-identity-char? character))
  (:lambda (prod)
	(second prod)))

; \t \t \n
(esrap:defrule escape-encoded (and #\\ (or #\t #\r #\n)))

; \;
(esrap:defrule escape-semicolon (and #\\ #\;))

; #[=*[xxxxxxxx]=*]
(esrap:defrule bracket-comment (and #\# bracket-argument))

; #xxxxxxx
(esrap:defrule line-comment (and #\# (esrap:! (and #\[ (esrap:* #\=) #\[))
								 (esrap:* (not newline))))

(defun expand-argument (arg bindings)
  (let ((pos2 (search "}" arg)))
	(if (null pos2)
		arg
		(let ((pos1 (search "${" arg :from-end t :end2 pos2)))
		  (if (null pos1)
			  (concatenate 'string (subseq arg 0 (+ 1 pos2))
						   (expand-argument (subseq arg (+ 1 pos2)) bindings))
			  (expand-argument (concatenate 'string
											(subseq arg 0 pos1)
											(gethash (subseq arg (+ 2 pos1) pos2)
													 bindings "")
											(subseq arg (+ 1 pos2))) bindings))))))

(defun parse-string (str)
  (let ((*bracket-argument-parsing* nil)
		(*bracket-open-=-len* 0)
		(*bracket-close-]-got* nil)
		(*bracket-close-=-len* 0))
	(let* ((len (length str))
		   (last (elt str (- len 1)))
		   (s (if (or (char= last #\return)
					  (char= last #\linefeed))
				  str
				  (concatenate 'string str (string #\newline))))) ; add \n to simplify parsing rule definition
	  (remove nil (esrap:parse 'file-elements s)))))

(defun parse-file (pathname)
  (let ((lines (with-open-file (in pathname)
				 (loop for line = (read-line in nil)
					while line collect line))))
	(remove nil (esrap:parse 'file-elements
							 (format nil "~{~A~%~}" lines)))))

