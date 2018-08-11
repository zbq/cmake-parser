;;;; cmake-parser.lisp

(in-package #:cmake-parser)

(defvar *bracket-argument-parsing* nil "whether we are parsing bracket argument")
(defvar *bracket-open-=-len* 0 "length of '=' in '[=...['")
(defvar *bracket-close-]-got* nil "whether we got first ']' of ']=...]'")
(defvar *bracket-close-=-len* 0 "length of '=' in ']=...]'")

; file         ::=  file_element*
(esrap:defrule file (esrap:* file-element)
  (:lambda (elements)
	(remove nil elements)))

; file_element ::=  command_invocation line_ending |
;                   (bracket_comment|space)* line_ending
; FIX: add (* space) before line-ending
(esrap:defrule file-element (or (and command-invocation (esrap:* space) line-ending)
								(and (esrap:* (or bracket-comment space)) line-ending))
  (:lambda (prod)
	(first prod)))

; line_ending  ::=  line_comment? newline
(esrap:defrule line-ending (and (esrap:? line-comment) newline)
  (:lambda (prod)
	(first prod)))

; space        ::=  <match '[ \t]+'>
(esrap:defrule space (esrap:+ (or #\space #\tab))
  (:constant " "))

; newline      ::=  <match '\n'>
(esrap:defrule newline (and #\linefeed)
  (:constant "
"))

; command_invocation  ::=  space* identifier space* '(' arguments ')'
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

; identifier          ::=  <match '[A-Za-z_][A-Za-z0-9_]*'>
(esrap:defrule identifier (and (identifier-first-char? character)
							   (esrap:* (identifier-rest-char? character)))
  (:text t))

; arguments           ::=  argument? separated_arguments*
(esrap:defrule arguments (and (esrap:? argument)
							  (esrap:* sep-arguments))
  (:lambda (prod)
	(remove nil (alexandria:flatten prod))))

; separated_arguments ::=  separation+ argument? |
;                          separation* '(' arguments ')'
(esrap:defrule sep-arguments (or (and (esrap:+ separation)
									  (esrap:? argument))
								 (and (esrap:* separation)
									  #\( arguments #\)))
  (:lambda (prod)
	(if (= (length prod) 2)
		(second prod)
		`("(" ,@(third prod) ")"))))

; separation          ::=  space | line_ending
(esrap:defrule separation (or space #\; line-ending))

; argument ::=  bracket_argument | quoted_argument | unquoted_argument
(esrap:defrule argument (or bracket-argument quoted-argument unquoted-argument))

#|Brackets do not nest. 
Bracket argument content consists of all text between the opening and closing brackets, 
except that one newline immediately following the opening bracket, if any, is ignored. 
No evaluation of the enclosed content, such as Escape Sequences or Variable References, is performed. 
A bracket argument is always given to the command invocation as exactly one argument.
|#
; bracket_argument ::=  bracket_open bracket_content bracket_close
(esrap:defrule bracket-argument (and bracket-open (esrap:? newline) bracket-close)
  (:lambda (prod)
	(third prod)))

(defun parsing-bracket-argument (str) ; [=...[
  (setf *bracket-argument-parsing* t)
  (setf *bracket-open-=-len* (length (cadr str))) ; length of '='
  (setf *bracket-close-]-got* nil)
  (setf *bracket-close-=-len* 0)
  t)

; bracket_open     ::=  '[' '='* '['
(esrap:defrule bracket-open (parsing-bracket-argument (and #\[ (esrap:* #\=) #\[))
  (:constant nil))

(defun bracket-argument-char? (char)
  (cond
	((not *bracket-argument-parsing*) nil)
	((char= char #\]) (if *bracket-close-]-got*
						  (if (= *bracket-open-=-len* *bracket-close-=-len*)
							  (setf *bracket-argument-parsing* nil) ; yes, bracket closed
							  (setf *bracket-close-=-len* 0) ; discard ']==' if length of '=' does not match
							  )
						  (progn
							(setf *bracket-close-]-got* t) ; first ] found
							(setf *bracket-close-=-len* 0)))
	 t)
	((char= char #\=) (when *bracket-close-]-got*
						(incf *bracket-close-=-len*))
	 t)
	(t (setf *bracket-close-]-got* nil) ; ']==abc]==]', any char that is not ']' or '=' destroy match
	   t)))

(esrap:defrule bracket-argument-char (bracket-argument-char? character))

(defun bracket-close-found? (str)
  (declare (ignore str))
  (not *bracket-argument-parsing*))

(esrap:defrule bracket-close (bracket-close-found? (esrap:+ bracket-argument-char))
  (:lambda (prod)
	(esrap:text (subseq prod 0 (- (length prod) 2 *bracket-open-=-len*))))) ; trim ]=...]

#|
Quoted argument content consists of all text between opening and closing quotes. 
Both Escape Sequences and Variable References are evaluated. A quoted argument 
is always given to the command invocation as exactly one argument.
|#
; quoted_argument     ::=  '"' quoted_element* '"'
(esrap:defrule quoted-argument (and #\" (esrap:* quoted-element) #\")
  (:lambda (prod)
	(esrap:text (second prod))))

; FIX: quoted_continuation should be placed before escape-sequence, otherwise it
;      will be captured by escape-sequence
; quoted_element      ::=  <any character except '\' or '"'> |
;                          escape_sequence |
;                          quoted_continuation
(esrap:defrule quoted-element (or (not (or #\\ #\"))
								  quoted-continuation
								  escape-sequence))

; The final \ on any line ending in an odd number of backslashes is treated as a 
; line continuation and ignored along with the immediately following newline character. 
; quoted_continuation ::=  '\' newline
(esrap:defrule quoted-continuation (and #\\ #\linefeed)
  (:constant nil))

; do not support legacy
; unquoted_argument ::=  unquoted_element+ | unquoted_legacy
; handle something like xx="yy zz"
(esrap:defrule unquoted-argument (esrap:+ (and unquoted-element (esrap:? (and #\" (esrap:* quoted-element) #\"))))
  (:text t))

; unquoted_element  ::=  <any character except whitespace or one of '()#"\'> |
;                        escape_sequence
(esrap:defrule unquoted-element (or (not (or #\space #\tab #\return #\linefeed #\( #\) #\# #\" #\\ #\;))
									escape-sequence))

; escape_sequence  ::=  escape_identity | escape_encoded | escape_semicolon
(esrap:defrule escape-sequence (or escape-identity
								   escape-encoded
								   escape-semicolon))

(defun escape-identity-char? (char)
  (not (or (alphanumericp char) (char= char #\;))))

; escape_identity  ::=  '\' <match '[^A-Za-z0-9;]'>
(esrap:defrule escape-identity (and #\\ (escape-identity-char? character))
  (:lambda (prod)
	(second prod)))

; escape_encoded   ::=  '\t' | '\r' | '\n'
(esrap:defrule escape-encoded (and #\\ (or #\t #\r #\n))
  (:lambda (prod)
	(cond
	  ((string= (second prod) "t") #\tab)
	  ((string= (second prod) "r") #\return)
	  (t #\linefeed))))

#|
A \; outside of any Variable References encodes itself but may be used in an 
Unquoted Argument to encode the ; without dividing the argument value on it. 
A \; inside Variable References encodes the literal ; character.
|#
; escape_semicolon ::=  '\;'
(esrap:defrule escape-semicolon (and #\\ #\;)
  (:constant ";"))

; bracket_comment ::=  '#' bracket_argument
; #[=*[xxxxxxxx]=*]
(esrap:defrule bracket-comment (and #\# bracket-argument)
  (:constant nil))

; line_comment ::=  '#' <any text not starting in a bracket_argument
;                        and not containing a newline>
; #xxxxxxx
(esrap:defrule line-comment (and #\# (esrap:! (and #\[ (esrap:* #\=) #\[))
								 (esrap:* (not newline)))
  (:constant nil))

(defun expand-argument (arg binding)
  "Expand argument if it contains variable reference.
You should provide a variable binding hash-table.
Ex. arg: 'Hello ${someone}', binding: 'someone'->'Bob',
it will expand to 'Hello Bob'."
  (let ((pos2 (search "}" arg)))
	(if (null pos2)
		arg
		(let ((pos1 (search "${" arg :from-end t :end2 pos2)))
		  (if (null pos1)
			  (concatenate 'string (subseq arg 0 (+ 1 pos2))
						   (expand-argument (subseq arg (+ 1 pos2)) binding))
			  (expand-argument (concatenate 'string
											(subseq arg 0 pos1)
											(format nil "~A"
													(gethash (subseq arg (+ 2 pos1) pos2)
															 binding ""))
											(subseq arg (+ 1 pos2))) binding))))))

(defun grammar ()
  "Return cmake grammar."
  (let ((str (make-array '(0) :element-type 'character
						 :fill-pointer 0 :adjustable t)))
	(with-output-to-string (s str)
	  (esrap:describe-grammar 'file s))
	str))

(defun slurp (pathname)
  (with-open-stream (in (open pathname :element-type 'character))
	(let* ((buffer (make-array (file-length in)
							   :element-type 'character
							   :fill-pointer t))
		   (pos (read-sequence buffer in)))
	  (setf (fill-pointer buffer) pos)
	  buffer)))

; \r\n -> \n, add trailing newline to simplify grammar rule definition
(defun convert-newline (str)
  (let ((buffer (make-array (+ 1 (length str))
							:element-type 'character
							:fill-pointer 0
							:adjustable t)))
	(with-output-to-string (s buffer)
	  (do ((pos 0)
		   (bak 0 pos))
		  ((null pos))
		(setf pos (search '(#\return #\linefeed) str :start2 pos))
		(format s "~A~A" (subseq str bak pos) #\linefeed)
		(if pos (setf pos (+ 2 pos))))
	  buffer)))

(defun parse-string (str)
  "Parse cmake script string, return a list of command invocation."
  (let ((*bracket-argument-parsing* nil)
		(*bracket-open-=-len* 0)
		(*bracket-close-]-got* nil)
		(*bracket-close-=-len* 0))
	(remove nil (esrap:parse 'file (convert-newline str)))))

(defun parse-file (pathname)
  "Parse cmake script file, return a list of command invocation."
  (parse-string (slurp pathname)))

