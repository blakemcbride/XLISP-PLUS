;;; Enable use of ERRSET function:
(nodebug)
(defparameter test-number 0)
(defparameter bugs-found 0)

(defvar *abort-on-error* nil)	; Abort tests on error, otherwise continue
(defvar *verbose-tests* nil)	; Print message on successful tests too.

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

(should-be-true (equal '(-6 0.5)  (multiple-value-call #'list (floor -5.5))))
(should-be-true (equal '(4 0.5)	  (multiple-value-call #'list (floor 4.5))))
(should-be-true (equal '(2 1/2)	  (multiple-value-call #'list (floor 5/2))))
(should-be-true (equal '(-3 1/2)  (multiple-value-call #'list (floor -5/2))))
(should-be-true (equal '(2 1)	  (multiple-value-call #'list (floor 5 2))))
(should-be-true (equal '(-3 1)	  (multiple-value-call #'list (floor -5 2))))
(should-be-true (equal '(-3 -1)	  (multiple-value-call #'list (floor 5 -2))))
(should-be-true (equal '(2 -1)	  (multiple-value-call #'list (floor -5 -2))))
(should-be-true (equal '(2 1.0)	  (multiple-value-call #'list (floor 5.0 2.0))))
(should-be-true (equal '(-3 1.0)	  (multiple-value-call #'list (floor -5.0 2.0))))
(should-be-true (equal '(-3 -1.0)	  (multiple-value-call #'list (floor 5.0 -2.0))))
(should-be-true (equal '(2 -1.0)    (multiple-value-call #'list (floor -5.0 -2.0))))

(should-be-true (equal '(-5 -0.5) (multiple-value-call #'list (ceiling -5.5))))
(should-be-true (equal '(5 -0.5)  (multiple-value-call #'list (ceiling 4.5))))
(should-be-true (equal '(3 -1/2)  (multiple-value-call #'list (ceiling 5/2))))
(should-be-true (equal '(-2 -1/2) (multiple-value-call #'list (ceiling -5/2))))
(should-be-true (equal '(3 -1)	  (multiple-value-call #'list (ceiling 5 2))))
(should-be-true (equal '(-2 -1)	  (multiple-value-call #'list (ceiling -5 2))))
(should-be-true (equal '(-2 1)	  (multiple-value-call #'list (ceiling 5 -2))))
(should-be-true (equal '(3 1)	  (multiple-value-call #'list (ceiling -5 -2))))
(should-be-true (equal '(3 -1.0)	  (multiple-value-call #'list (ceiling 5.0 2.0))))
(should-be-true (equal '(-2 -1.0)	  (multiple-value-call #'list (ceiling -5.0 2.0))))
(should-be-true (equal '(-2 1.0)	  (multiple-value-call #'list (ceiling 5.0 -2.0))))
(should-be-true (equal '(3 1.0)     (multiple-value-call #'list (ceiling -5.0 -2.0))))

(should-be-true (equal '(-6 0.5)  (multiple-value-call #'list (round -5.5))))
(should-be-true (equal '(4 0.5)   (multiple-value-call #'list (round 4.5))))
(should-be-true (equal '(2 1/2)   (multiple-value-call #'list (round 5/2))))
(should-be-true (equal '(-2 -1/2) (multiple-value-call #'list (round -5/2))))
(should-be-true (equal '(2 1)	  (multiple-value-call #'list (round 5 2))))
(should-be-true (equal '(-2 -1)	  (multiple-value-call #'list (round -5 2))))
(should-be-true (equal '(-2 1)	  (multiple-value-call #'list (round 5 -2))))
(should-be-true (equal '(2 -1)	  (multiple-value-call #'list (round -5 -2))))
(should-be-true (equal '(2 1.0)	  (multiple-value-call #'list (round 5.0 2.0))))
(should-be-true (equal '(-2 -1.0) (multiple-value-call #'list (round -5.0 2.0))))
(should-be-true (equal '(-2 1.0)  (multiple-value-call #'list (round 5.0 -2.0))))
(should-be-true (equal '(2 -1.0)  (multiple-value-call #'list (round -5.0 -2.0))))

(should-be-true (equal '(6 -0.5)  (multiple-value-call #'list (round 5.5))))
(should-be-true (equal '(-4 -0.5) (multiple-value-call #'list (round -4.5))))
(should-be-true (equal '(4 -1/2)  (multiple-value-call #'list (round 7/2))))
(should-be-true (equal '(-4 1/2)  (multiple-value-call #'list (round -7/2))))
(should-be-true (equal '(4 -1)	  (multiple-value-call #'list (round 7 2))))
(should-be-true (equal '(-4 1)	  (multiple-value-call #'list (round -7 2))))
(should-be-true (equal '(-4 -1)	  (multiple-value-call #'list (round 7 -2))))
(should-be-true (equal '(4 1)	  (multiple-value-call #'list (round -7 -2))))
(should-be-true (equal '(4 -1.0)	  (multiple-value-call #'list (round 7.0 2.0))))
(should-be-true (equal '(-4 1.0)	  (multiple-value-call #'list (round -7.0 2.0))))
(should-be-true (equal '(-4 -1.0)	  (multiple-value-call #'list (round 7.0 -2.0))))
(should-be-true (equal '(4 1.0)     (multiple-value-call #'list (round -7.0 -2.0))))



(terpri)

(princ test-number)
(princ " tests performed.")
(terpri)

(princ bugs-found)
(princ " bugs found.")
(terpri)
