;;; Enable use of ERRSET function:
(nodebug)
(trace should-be-true)
(defparameter test-number 0)
(defparameter bugs-found 0)

(defvar *abort-on-error* nil)	; Abort tests on error, otherwise continue
(defvar *verbose-tests* t)	; Print message on successful tests too.

(setq rtc *readtable-case* pc *print-case*)
(setq *readtable-case* :invert
      *print-case* :upcase)

;;; Define a central error-report fn:
(defun r-fail (i x why)
  (setq bugs-found (1+ bugs-found))
  (format t "***** Failed test number ~s: ~s~%  because: ~a~%" i x why)
  (when *abort-on-error* (top-level)))

(defun passed-test (i x) 
       (when *verbose-tests* 
	     (format t "Passed test ~s: ~s~%" i x)))

;;; Squeeze out some redundancy:
(defmacro should-fail (x)
  (setq test-number (1+ test-number))
  `(if (errset ,x nil) 
       (r-fail test-number ',x "execution didn't fail")
       (passed-test test-number ',x)))
(defmacro should-work (x)
  (setq test-number (1+ test-number))
  `(if (not (errset ,x nil)) 
       (r-fail test-number ',x "execution failed")
       (passed-test test-number ',x)))
(defmacro should-be-true (x &aux (y (gensym)))
  (setq test-number (1+ test-number))
  `(let ((,y (errset ,x nil))) 
       (cond ((null ,y) (r-fail test-number ',x "execution failed"))
	     ((eq (car ,y) 'nil) (r-fail test-number ',x "returned NIL"))
	     (t (passed-test test-number ',x)))))
(defmacro should-be-false (x &aux (y (gensym)))
  (setq test-number (1+ test-number))
  `(let ((,y (errset ,x nil))) 
       (cond ((null ,y) (r-fail test-number ',x "execution failed"))
	     ((not (eq (car ,y) 'nil)) 
	      (r-fail test-number ',x (format nil "returned ~s" (car ,y))))
	     (t (passed-test test-number ',x)))))


; TESTS GO HERE

(defun read-from-string (arg)
       (read (make-string-input-stream arg)))

; It will be easier to define a function that does the read/print comparisons

(defun rp (function instring string 
		    &aux
			result
		    (stream (make-string-output-stream)))
       (funcall function (read-from-string instring) stream)
	   (setq result (get-output-stream-string stream))
       (if (string= string result)
		   t
		   (format t "Expected ~s~%Observed ~s~~%" string result)))


; This function reads instring, which must be a symbol name.
; The printname of the symbol must be the same as intstring.
; The value after printing with prin1 must be the same as string.
; The value after printing with princ must be the same as string2.

(defun prin1-to-string (arg)
       (let ((stream (make-string-output-stream)))
	    (prin1 arg stream)
	    (get-output-stream-string stream)))

(defun princ-to-string (arg)
       (let ((stream (make-string-output-stream)))
	    (princ arg stream)
	    (get-output-stream-string stream)))

(defun rcp (instring intstring string string2
		    &aux 
		    tmp)
       (setq tmp (read-from-string instring))
       (and (string= string (prin1-to-string tmp))
	    (string= string2 (princ-to-string tmp))
	    (string= intstring (symbol-name tmp))))

(let ((*print-case* :upcase)
      (*readtable-case* :upcase)
      (*print-length* nil)
      (*print-depth* nil))
     (should-be-true (rp #'prin1 "x" "X"))	;; symbols -- uppercase
     (should-be-true (rp #'prin1 "X" "X"))
     (should-be-true (rp #'prin1 "|x|" "|x|"))
     (should-be-true (rp #'prin1 "1x" "1X"))
     (should-be-true (rp #'prin1 "|12|" "\\12"))
     (should-be-true (rp #'prin1 "|-1|" "\\-1"))
     (should-be-true (rp #'prin1 "12\\3" "\\123"))
     (should-be-true (rp #'princ "|aBc|" "aBc"))
     (should-be-true (rp #'princ "\\12" "12"))
     
     (setq *print-case* :downcase)
     (should-be-true (rp #'prin1 "x" "x"))	;; symbols -- lowercase
     (should-be-true (rp #'prin1 "X" "x"))
     (should-be-true (rp #'prin1 "|x|" "|x|"))
     (should-be-true (rp #'prin1 "1x" "1x"))
     
     (should-be-true (rp #'princ "x" "x"))	;; princ of symbols
     (should-be-true (rp #'princ "|aBc|" "abc"))

     (should-be-true (rp #'prin1 "#:x" "#:x"))	;; uninterned symbols
     (should-be-true (rp #'prin1 "#:|abc|" "#:|abc|"))
     (should-be-true (rp #'princ "#:x" "x"))

     (should-be-true (rp #'prin1 "\"abCDe \"" "\"abCDe \"")) ; strings
     (should-be-true (rp #'prin1 "\"\\n\"" "\"\n\""))
     (should-be-true (rp #'prin1 
			 "\"\\\\\\012\\011\\015\\014\\000A\""
			 "\"\\\\\n\\t\\r\\f\\000A\""))
     (should-be-true (rp #'prin1 "\"
\"" "\"\n\""))
     (should-be-true (rp #'princ "\"abCDe \"" "abCDe "))
     (should-be-true (rp #'princ "\"\\n\"" "\n"))
     (should-be-true (rp #'princ 
			 "\"\\\\\\012\\011\\015\\014\\000A\""
			 "\\\n\t\r\f\000A"))

     (should-be-true (rp #'prin1 "#\\x" "#\\x"))	; characters
     (should-be-true (rp #'prin1 "(#\\\\#\\|)" "(#\\\\ #\\|)"))
     (should-be-true (rp #'prin1 "(#\\newline)" "(#\\Newline)"))
     (should-be-true (rp #'prin1 "(#\\space)" "(#\\Space)"))
     (should-be-true (rp #'prin1 "(#\\rubout)" "(#\\Rubout)"))
     (should-be-true (rp #'prin1 "(#\\C-@)" "(#\\C-@)"))
; Without extended characters
;     (should-be-true (rp #'prin1 "(#\\M-a)" "(#\\M-a)"))
;     (should-be-true (rp #'prin1 "(#\\M-0)" "(#\\M-0)"))
;     (should-be-true (rp #'prin1 "(#\\M-space)" "(#\\M-Space)"))
;     (should-be-true (rp #'prin1 "(#\\M-C-@)" "(#\\M-C-@)"))
;     (should-be-true (rp #'prin1 "#\\M-newline" "#\\M-Newline"))
; With extended characters
     (should-be-true (rp #'prin1 "(#\\M-a)" "(#\\á)"))
     (should-be-true (rp #'prin1 "(#\\M-0)" "(#\\°)"))
     (should-be-true (rp #'prin1 "(#\\M-space)" "(#\\ )"))
     (should-be-true (rp #'prin1 "(#\\M-C-@)" "(#\\€)"))
     (should-be-true (rp #'prin1 "#\\M-newline" "#\\Š"))

     (should-be-true (rp #'princ "#\\M-C-@" "\200"))
     (should-be-true (rp #'prin1 "-100" "-100"))		; other stuff
     (should-be-true (rp #'prin1 "+199.123E+1" "1991.23"))
     (should-be-true (rp #'prin1 "(a b . c)" "(a b . c)"))
     (should-be-true (rp #'prin1 "#C(1 2)" "#C(1 2)"))
     (should-be-true (rp #'prin1 "#(1 2 x)" "#(1 2 x)"))
     (should-be-true (rp #'prin1 "()" "nil"))
     (should-be-true (rp #'princ "()" "nil"))

; Let's test FORMAT

     (should-be-true (string= (format nil "A string~~~\n  y~:\n x~@\n  y")
			      "A string~y x\ny"))
     (should-be-true (string= (format nil "~s" '|Hello|) "|Hello|"))
     (should-be-true (string= (format nil "~a" '|Hello|) "hello"))
     (should-be-true (string= (format nil "~10@:s~10s" nil nil)
				      "        ()nil       "))
     (should-be-true (string= (format nil "~,,5,'.@s" 1) ".....1"))
     (should-be-true (string= (format nil "~10,4,8s" 'x) "x            "))
     (should-be-true (string= (format nil "~1&ab~tc~,0tx~10td~0,4te~13,4tf") 
			      "ab cx     d e    f"))
     (should-be-true (string= (format nil "~d ~5,'xd ~@d ~5@d~d" -5 5 -5 5 '|x|)
			      "-5 xxxx5 -5    +5x"))
     (should-be-true (string= (format nil "~10,5,'*@f~7,5,'*@e" 10 '|x|)
			      "*+10.00000x      "))
     )

;; Check all print and readcase modes

(let ((*print-case* :upcase)
      (*readtable-case* :upcase)
      (*print-length* nil)
      (*print-depth* nil))
;			  in    pname prin1 princ
     (should-be-true (rcp "abc" "ABC" "ABC" "ABC"))
     (should-be-true (rcp "ABC" "ABC" "ABC" "ABC"))
     (should-be-true (rcp "AbC" "ABC" "ABC" "ABC"))
     (should-be-true (rcp "|abc|" "abc" "|abc|" "abc"))
     (should-be-true (rcp "|ABC|" "ABC" "ABC" "ABC"))
     (should-be-true (rcp "|AbC|" "AbC" "|AbC|" "AbC"))
)
(let ((*print-case* :downcase)
      (*readtable-case* :upcase)
      (*print-length* nil)
      (*print-depth* nil))
;			  in    pname prin1 princ
     (should-be-true (rcp "abc" "ABC" "abc" "abc"))
     (should-be-true (rcp "ABC" "ABC" "abc" "abc"))
     (should-be-true (rcp "AbC" "ABC" "abc" "abc"))
     (should-be-true (rcp "|abc|" "abc" "|abc|" "abc"))
     (should-be-true (rcp "|ABC|" "ABC" "abc" "abc"))
     (should-be-true (rcp "|AbC|" "AbC" "|AbC|" "abc"))
)
(let ((*print-case* :capitalize)
      (*readtable-case* :upcase)
      (*print-length* nil)
      (*print-depth* nil))
;			  in    pname prin1 princ
     (should-be-true (rcp "abc" "ABC" "Abc" "Abc"))
     (should-be-true (rcp "ABC" "ABC" "Abc" "Abc"))
     (should-be-true (rcp "AbC" "ABC" "Abc" "Abc"))
     (should-be-true (rcp "|abc|" "abc" "|abc|" "abc"))
     (should-be-true (rcp "|ABC|" "ABC" "Abc" "Abc"))
     (should-be-true (rcp "|AbC|" "AbC" "|AbC|" "Abc"))
)


(let ((*print-case* :downcase)
      (*readtable-case* :downcase)
      (*print-length* nil)
      (*print-depth* nil))
;			  in    pname prin1 princ
     (should-be-true (rcp "abc" "abc" "abc" "abc"))
     (should-be-true (rcp "ABC" "abc" "abc" "abc"))
     (should-be-true (rcp "AbC" "abc" "abc" "abc"))
     (should-be-true (rcp "|abc|" "abc" "abc" "abc"))
     (should-be-true (rcp "|ABC|" "ABC" "|ABC|" "ABC"))
     (should-be-true (rcp "|AbC|" "AbC" "|AbC|" "AbC"))
)
(let ((*print-case* :upcase)
      (*readtable-case* :downcase)
      (*print-length* nil)
      (*print-depth* nil))
;			  in    pname prin1 princ
     (should-be-true (rcp "abc" "abc" "ABC" "ABC"))
     (should-be-true (rcp "ABC" "abc" "ABC" "ABC"))
     (should-be-true (rcp "AbC" "abc" "ABC" "ABC"))
     (should-be-true (rcp "|abc|" "abc" "ABC" "ABC"))
     (should-be-true (rcp "|ABC|" "ABC" "|ABC|" "ABC"))
     (should-be-true (rcp "|AbC|" "AbC" "|AbC|" "ABC"))
)
(let ((*print-case* :capitalize)
      (*readtable-case* :downcase)
      (*print-length* nil)
      (*print-depth* nil))
;			  in    pname prin1 princ
     (should-be-true (rcp "abc" "abc" "Abc" "Abc"))
     (should-be-true (rcp "ABC" "abc" "Abc" "Abc"))
     (should-be-true (rcp "AbC" "abc" "Abc" "Abc"))
     (should-be-true (rcp "|abc|" "abc" "Abc" "Abc"))
     (should-be-true (rcp "|ABC|" "ABC" "|ABC|" "ABC"))
     (should-be-true (rcp "|AbC|" "AbC" "|AbC|" "AbC"))
)


(let ((*print-case* :upcase)
      (*readtable-case* :preserve)
      (*print-length* nil)
      (*print-depth* nil))
;			  in    pname prin1 princ
     (should-be-true (rcp "abc" "abc" "abc" "abc"))
     (should-be-true (rcp "ABC" "ABC" "ABC" "ABC"))
     (should-be-true (rcp "AbC" "AbC" "AbC" "AbC"))
     (should-be-true (rcp "|abc|" "abc" "abc" "abc"))
     (should-be-true (rcp "|ABC|" "ABC" "ABC" "ABC"))
     (should-be-true (rcp "|AbC|" "AbC" "AbC" "AbC"))
)
(let ((*print-case* :downcase)
      (*readtable-case* :preserve)
      (*print-length* nil)
      (*print-depth* nil))
;			  in    pname prin1 princ
     (should-be-true (rcp "abc" "abc" "abc" "abc"))
     (should-be-true (rcp "ABC" "ABC" "ABC" "ABC"))
     (should-be-true (rcp "AbC" "AbC" "AbC" "AbC"))
     (should-be-true (rcp "|abc|" "abc" "abc" "abc"))
     (should-be-true (rcp "|ABC|" "ABC" "ABC" "ABC"))
     (should-be-true (rcp "|AbC|" "AbC" "AbC" "AbC"))
)
(let ((*print-case* :capitalize)
      (*readtable-case* :preserve)
      (*print-length* nil)
      (*print-depth* nil))
;			  in    pname prin1 princ
     (should-be-true (rcp "abc" "abc" "abc" "abc"))
     (should-be-true (rcp "ABC" "ABC" "ABC" "ABC"))
     (should-be-true (rcp "AbC" "AbC" "AbC" "AbC"))
     (should-be-true (rcp "|abc|" "abc" "abc" "abc"))
     (should-be-true (rcp "|ABC|" "ABC" "ABC" "ABC"))
     (should-be-true (rcp "|AbC|" "AbC" "AbC" "AbC"))
)

(let ((*print-case* :upcase)
      (*readtable-case* :invert)
      (*print-length* nil)
      (*print-depth* nil))
;			  in    pname prin1 princ
     (should-be-true (rcp "abc" "ABC" "abc" "abc"))
     (should-be-true (rcp "ABC" "abc" "ABC" "ABC"))
     (should-be-true (rcp "AbC" "AbC" "AbC" "AbC"))
     (should-be-true (rcp "|abc|" "abc" "ABC" "ABC"))
     (should-be-true (rcp "|ABC|" "ABC" "abc" "abc"))
     (should-be-true (rcp "|AbC|" "AbC" "AbC" "AbC"))
)
(let ((*print-case* :downcase)
      (*readtable-case* :invert)
      (*print-length* nil)
      (*print-depth* nil))
;			  in    pname prin1 princ
     (should-be-true (rcp "abc" "ABC" "abc" "abc"))
     (should-be-true (rcp "ABC" "abc" "ABC" "ABC"))
     (should-be-true (rcp "AbC" "AbC" "AbC" "AbC"))
     (should-be-true (rcp "|abc|" "abc" "ABC" "ABC"))
     (should-be-true (rcp "|ABC|" "ABC" "abc" "abc"))
     (should-be-true (rcp "|AbC|" "AbC" "AbC" "AbC"))
)
(let ((*print-case* :capitalize)
      (*readtable-case* :invert)
      (*print-length* nil)
      (*print-depth* nil))
;			  in    pname prin1 princ
     (should-be-true (rcp "abc" "ABC" "abc" "abc"))
     (should-be-true (rcp "ABC" "abc" "ABC" "ABC"))
     (should-be-true (rcp "AbC" "AbC" "AbC" "AbC"))
     (should-be-true (rcp "|abc|" "abc" "ABC" "ABC"))
     (should-be-true (rcp "|ABC|" "ABC" "abc" "abc"))
     (should-be-true (rcp "|AbC|" "AbC" "AbC" "AbC"))
)

; Redirect standard output and do testing of defaults

; These tests will be a major disaster if SPECIAL variable option not used.

(defun foo () *print-length*)
(unless (eq 'x (let ((*print-length* 'x)) (foo)))
	(error "Cannot continue tests when SPECIALS option not compiled!!"))

(let ((*print-case* :upcase)
      (*readtable-case* :upcase)
      (*print-length* nil)
      (*print-depth* nil))
     (should-work (let ((*standard-output* (open "test.tmp" 
						 :direction :output)))
		       (princ 'x)
		       (prin1 'x)
		       (fresh-line)	; does new line
		       (print 'x)
		       (terpri)	; does new line
		       (fresh-line)	; does nothing
		       (write-char #\x)
			   (write-char #\A)
;;		       (write-byte 65)	; 'A'
		       (format t "end~%")
		       (close *standard-output*)))

     (should-work (let ((*standard-output* (open "test.tmp"
						 :direction :output
						 :if-exists :append)))
		       (princ 'x nil)
		       (prin1 'x nil)
		       (fresh-line nil)	; does new line
		       (print 'x nil)
		       (terpri nil)	; does new line
		       (fresh-line nil)	; does nothing
		       (write-char #\x nil)
			   (write-char #\A nil)
;;		       (write-byte 65 nil)	; 'A'
		       (format t "end~%")
		       (close *standard-output*)))
     
;; NOTE ON REBINDING OF *terminal-io* -- Don't try this at home!

     (should-work (let ((*terminal-io* (open "test.tmp"
						 :direction :output
						 :if-exists :append)))
		       (princ 'x t)
		       (prin1 'x t)
		       (fresh-line t)	; does new line
		       (print 'x t)
		       (terpri t)	; does new line
		       (fresh-line t)	; does nothing
		       (write-char #\x t)
			   (write-char #\A t)
;;		       (write-byte 65 t)	; 'A'
		       (format t "end~%")
		       (close *terminal-io*)))

     )
     
; Read the test.tmp file to validate operation
(let ((file (open "test.tmp")))
     (should-be-true (string= (read-line file) "XX"))
     (should-be-true (string= (read-line file) "X"))
     (should-be-true (string= (read-line file) ""))
     (should-be-true (string= (read-line file) "xAend"))
     (should-be-true (string= (read-line file) "XX"))
     (should-be-true (string= (read-line file) "X"))
     (should-be-true (string= (read-line file) ""))
     (should-be-true (string= (read-line file) "xAend"))
     (should-be-true (string= (read-line file) "XX"))
     (should-be-true (string= (read-line file) "X"))
     (should-be-true (string= (read-line file) ""))
     (should-be-true (string= (read-line file) "xA"))
     (close file))


; Check reads
(let ((file (open "test.tmp" :direction :output :if-exists :supersede)))
     (format file "ABTest~% (a b c)~%")
     (close file))

(should-be-true (let ((*standard-input* (open "test.tmp")))
		     (prog1
		      (equal (list (read-char)
				   (read-char)
				   (read-line)
				   (read)
				   (read *standard-input* nil))
			     '(#\A #\B "Test" (a b c) nil))
		      (close *standard-input*))))

(should-be-true (let ((*standard-input* (open "test.tmp")))
		     (prog1
		      (equal (list (read-char nil)
				   (read-char nil)
				   (read-line nil)
				   (read nil)
				   (read nil nil 'x))
			     '(#\A #\B "Test" (a b c) x))
		      (close *standard-input*))))

(should-be-true (let ((*terminal-io* (open "test.tmp")))
		     (prog1
		      (equal (list (read-char t)
				   (read-char t)
				   (read-line t)
				   (read t)
				   (read t nil 'x))
			     '(#\A #\B "Test" (a b c) x))
		      (close *terminal-io*))))


(system "rm test.tmp")

(setq *readtable-case* rtc *print-case* pc)
(makunbound 'rtc)
(makunbound 'pc)

(terpri)

(princ test-number)
(princ " tests performed.")
(terpri)

(princ bugs-found)
(princ " bugs found.")
(terpri)

