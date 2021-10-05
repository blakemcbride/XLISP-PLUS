;;; Enable use of ERRSET function:
(nodebug)
(defparameter test-number 0)
(defparameter bugs-found 0)

(defvar *abort-on-error* t)	; Abort tests on error, otherwise continue
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

;; Check lambda bindings -- SPECIALS and NILSYMBOL must be declared
;;                          to pass all tests

(defvar *a* 10)
(defconstant acon 1)
(setq b 20)	; Not special

(defun getspec () (list *a* b))

(should-fail (setq acon t))	; can't assign to constant
(should-fail (defvar acon nil))
(should-be-true (progn (defconstant acon 2) (eq acon 2))) ; gives warning
(should-be-true (progn (defvar *a* 20)
		    (eq *a* 10)))	; can't redefine special variables

(should-fail ((lambda (x)) ))	; arg missing
(should-fail ((lambda ()) 'x))	; extra arg
(should-work ((lambda (&rest x)) :key 'foo))	; key arg is just "rest"
(should-fail ((lambda (&key ((x y))))))		; keyword is not keyword (no :)
(should-fail ((lambda (&key :foo))))		; key arg is constant!
(should-fail ((lambda (&rest x &key y)) :key 'foo))	; extra key
(should-be-true (eql 10 ((lambda (&key x) x) :x 10 :x 20))) ; key used twice
(should-work ((lambda (&rest x &key y &allow-other-keys)) :key 'foo))
(should-fail ((lambda (acon)) 'x))	; bind to constant
(should-fail ((lambda (&optional acon)) ))	; bind nil to constant
(should-fail ((lambda (&optional acon)) 'x))	; bind to constant
(should-fail ((lambda (&optional (x nil acon))) ))	; bind to constant
(should-fail ((lambda (&optional (x nil acon))) 'x))	; bind to constant
(should-fail ((lambda (&rest acon)) 'x))	; bind to constant
(should-fail ((lambda (&key ((:con acon)))) ))  ; bind nil to constant
(should-fail ((lambda (&key ((:con acon)))) :con 'x)) ; bind to constant
(should-fail ((lambda (&key (key nil acon))) ))	; bind nil to a constant
(should-fail ((lambda (&key (key nil acon))) :key 'x))	; bind nil to a constant

; Test required arguments
(should-be-true (equal ((lambda (*a* b) (append (getspec) (list *a* b)))
			100 200)
		       '(100 20 100 200)))

; Test optional arguments
(should-be-true (equal ((lambda (&optional *a* b) 
				(append (getspec) (list *a* b)))
			100 200)
		       '(100 20 100 200)))
(should-be-true (equal ((lambda (&optional *a* b) 
				(append (getspec) (list *a* b)))
			)
		       '(nil 20 nil nil)))
(should-be-true (equal ((lambda (&optional (*a* 100) (b 200)) 
				(append (getspec) (list *a* b)))
			)
		       '(100 20 100 200)))
(should-be-true (equal ((lambda (&optional (x nil *a*))
				(list (first (getspec)) *a*))
			)
		       '(nil nil)))
(should-be-true (equal ((lambda (&optional (x nil *a*))
				(list (first (getspec)) *a*))
			10)
		       '(t t)))
(should-be-true (equal ((lambda (&optional (x nil b))
				(list (second (getspec)) b))
			)
		       '(20 nil)))
(should-be-true (equal ((lambda (&optional (x nil b))
				(list (second (getspec)) b))
			10)
		       '(20 t)))
; Test &rest argument

(should-be-true (equal ((lambda (&rest *a*) (list (first (getspec)) *a*))
			'a 'b 'c)
		       '((a b c) (a b c))))

(should-be-true (equal ((lambda (&rest b) (list (second (getspec)) b))
			'a 'b 'c)
		       '(20 (a b c))))

(should-be-true (equal ((lambda (a &optional b &rest c) (list a b c))
			'x 'y 'z 'q)
		       '(x y  (z q))))

; Test keyword arguments
(should-be-true (equal ((lambda (&key *a* b)
				(append (getspec) (list *a* b)))
			:*a* 'x :b 'y)
		       '(x 20 x y)))

(should-be-true (equal ((lambda (&key (x *a*) (y b))
				(list x y))
			:x 'x1 :y 'y1)
		       '(x1 y1)))

(should-be-true (equal ((lambda (&key (x *a*) (y b))
				(list x y))
			)
		       '(10 20)))

(should-be-true (equal ((lambda (&key (x *a*) (y b))
				(list x y))
			:y 'y1)
		       '(10 y1)))

(should-be-true (equal ((lambda (&key (x nil *a*) (y nil b))
				(append (getspec) (list *a* b)))
			:x 10 :y 20)
		       '(t 20 t t)))

(should-be-true (equal ((lambda (&key (x nil *a*) (y nil b))
				(append (getspec) (list *a* b)))
			)
		       '(nil 20 nil nil)))

(should-be-true (equal ((lambda (x1 &rest y1 &key (x y1) (y 20))
				(list x1 y1 x y))
			100 :y 200)
		       '(100 (:y 200) (:y 200) 200)))



; Test let
(should-fail (let (acon)))	; bind to constant
(should-fail (let ((acon 10))))	; bind to constant
(should-fail (let (7)))		; bind to non-symbol
(should-be-true (let ((*a* 15) (b 30)) 
		     (equal (append (getspec) (list *a* b))
			    '(15 20 15 30))))
(should-be-true (let (x) (null x)))
(should-be-true (let ((*a* (1+ b)) (b (1+ *a*)) (c (1+ b)))
		     (equal (list *a* b c) '(21 11 21))))


; Test let*
(should-fail (let* (acon)))		; bind to constant
(should-fail (let* ((acon 10))))	; bind to constant
(should-fail (let* (7)))		; bind to non-symbol
(should-be-true (let* ((*a* 15) (b 30)) 
		     (equal (append (getspec) (list *a* b))
			    '(15 20 15 30))))
(should-be-true (let* (x) (null x)))
(should-be-true (let* ((*a* (1+ b)) (b (1+ *a*)) (c (1+ b)))
		     (equal (list *a* b c) '(21 22 23))))

; Test progv
(should-work (progv '(x) '()))		; unmatched lengths
(should-work (progv '(x) '(10 20)))
(should-be-false (progv '(x y) '(15)	; x is bound to 15, y is unbound
			(or (not (eql x 15)) (boundp 'y)))) 
(should-fail (progv '(acon) '(10)))	; bind to constant
(should-fail (progv '(7) '(10)))	; bind to non-symbol
(should-be-true (progv '(*a* b) '(15 30)
		       (equal (append (getspec) (list *a* b))
			      '(15 30 15 30))))
(should-be-true (progv '(*a* b c) (list (1+ b) (1+ *a*) (1+ b))
		       (equal (list *a* b c) '(21 11 21))))

; Test tagbody
(should-fail (tagbody (go nowhere)))	; tag doesn't exist
(should-work (tagbody x (go there) there))	; tag exists
(should-be-true (let ((x 10)) (tagbody (tagbody (go foo)) (setq x 20) foo)
		     (eq x 10)))

; Test block, return, return-from
(should-fail (return-from nowhere 10))	; return must exist
(should-be-true (eql (block foo 10) 10))

; Doesn't work with lexically scoped common lisp.

(defun retfoo () (return-from foo 10))
; (should-be-true (eql (block foo (retfoo) 20) 10))
(should-fail (eql (block foo (retfoo) 20) 10))

; Doesn't work with lexical scope
(defun gothere () (go there))
; (should-be-true (eql (block foo (tagbody (gothere) 
;					 (return-from foo 10)
;					 there
;					 (return-from foo 20)))
;		     20))
(should-fail (eql (block foo (tagbody (gothere) 
				      (return-from foo 10)
				      there
				      (return-from foo 20)))
		  20))
(should-be-true (eql (block nil (return-from nil 20) 30)
		     20))
(should-be-true (eql (block nil (return 20) 30)
		     20))

; Test prog
(should-fail (prog (acon)))	; bind to constant
(should-fail (prog ((acon 10))))	; bind to constant
(should-fail (prog (7)))		; bind to non-symbol
(should-be-true (prog ((*a* 15) (b 30)) 
		     (return (equal (append (getspec) (list *a* b))
				    '(15 20 15 30)))))
(should-be-true (prog (x) (return (null x))))
(should-be-true (prog ((*a* (1+ b)) (b (1+ *a*)) (c (1+ b)))
		     (return (equal (list *a* b c) '(21 11 21)))))


; Test prog*
(should-fail (prog* (acon)))		; bind to constant
(should-fail (prog* ((acon 10))))	; bind to constant
(should-fail (prog* (7)))		; bind to non-symbol
(should-be-true (prog* ((*a* 15) (b 30)) 
		       (return (equal (append (getspec) (list *a* b))
				      '(15 20 15 30)))))
(should-be-true (prog* (x) (return (null x))))
(should-be-true (prog* ((*a* (1+ b)) (b (1+ *a*)) (c (1+ b)))
		       (return (equal (list *a* b c) '(21 22 23)))))


; Test Catch and Throw
(defun test1 () (throw 'bar))
(defun test2 () (throw 'bar 10))
(defun test3 () (throw 'foo))
(defun test4 () (unwind-protect (test2) (setq *a* 100)))

(should-be-true (eql (catch 'bar 22) 22))
(should-be-true (null (catch 'bar (test1) 10)))
(should-be-true (eql (catch 'bar (test2) 20) 10))
(should-fail (catch 'bar (test3)))
(should-be-true (let ((*a* 10)) (and (eql (catch 'bar (test4) 20) 10)
				     (eql *a* 100))))


; Test do 
(should-fail (do (acon) (t nil)))	; bind to a constant
(should-fail (do ((acon 10)) (t nil)))	; bind to a constant
(should-fail (do (7) (t nil)))		; bind to non-symbol
(should-be-true (do ((*a* 15) (b 30))
		    (t (equal (append (getspec) (list *a* b))
			      '(15 20 15 30)))))	; check binding
(should-be-true (do ((*a* (1+ b)) (b (1+ *a*)) (c (1+ b)))
		    (t (equal (list *a* b c) '(21 11 21)))))
(should-be-false (do () (t)))
(should-be-true (do ((*a* *a* (1+ b)) 
		     (b b (1+ *a*)) 
		     (c b (1+ b)) 
		     (d nil t))
		    (d (equal (list *a* b c) '(21 11 21)))))

(should-be-true (do ((a 0 (1+ a)) (b nil))
		    (b (eq a 1))
		    (setq b t)))

; Test do*

(should-fail (do* (acon) (t nil)))	; bind to a constant
(should-fail (do* ((acon 10)) (t nil)))	; bind to a constant
(should-fail (do* (7) (t nil)))		; bind to non-symbol
(should-be-true (do* ((*a* 15) (b 30))
		    (t (equal (append (getspec) (list *a* b))
			      '(15 20 15 30)))))	; check binding
(should-be-true (do* ((*a* (1+ b)) (b (1+ *a*)) (c (1+ b)))
		    (t (equal (list *a* b c) '(21 22 23)))))
(should-be-false (do* () (t)))
(should-be-true (do* ((*a* *a* (1+ b)) (b b (1+ *a*)) (c b (1+ b)) (d nil t))
		    (d (equal (list *a* b c) '(21 22 23)))))

(should-be-true (do* ((a 0 (1+ a)) (b nil))
		    (b (eq a 1))
		    (setq b t)))

; Test dotimes

(should-fail (dotimes (acon 10)))
(should-fail (dotimes (7 10)))
(should-be-false (dotimes (b 0)(return t)))
(should-be-true (dotimes (b 1)(return t)))
(should-be-true (dotimes (b 0 t)))
(should-be-true (let (n)
		     (equal
		      (dotimes (b 5 n) (setq n (cons b n)))
		      '(4 3 2 1 0))))


; test dolist


(terpri)

(princ test-number)
(princ " tests performed.")
(terpri)

(princ bugs-found)
(princ " bugs found.")
(terpri)

