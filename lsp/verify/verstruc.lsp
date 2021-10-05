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

;; THIS IS A PRETTY BLAND TEST!!


(should-be-true (eq (defstruct str1 a b (c 10) (d 20)) 'str1))
(should-be-true (eq (defstruct (str2 (:include str1 (b 30) (d 15))) (e 5) f)
		    'str2))
(should-work (setq x1 (make-str1) x2 (make-str2)))	; Default structs
(should-be-true (and (str1-p x1) (str2-p x2)
		     (not (str2-p x1))
		     (not (str1-p x2))))
(should-be-true (equal (list (str1-a x1) (str1-b x1) (str1-c x1) (str1-d x1))
		       '(nil nil 10 20)))
(should-be-true (equal (list (str2-a x2) (str2-b x2) (str2-c x2)
			     (str2-d x2) (str2-e x2) (str2-f x2))
		       '(nil 30 10 15 5 nil)))
(should-be-true (let ((x3 (copy-str2 x2))) 
		     (equal (list (str2-a x3) (str2-b x3) (str2-c x3)
				  (str2-d x3) (str2-e x3) (str2-f x3))
			    '(nil 30 10 15 5 nil))))
(should-be-true (let ((x2 (make-str2 :a 5 :e 20 :b 7)))
		     (equal (list (str2-a x2) (str2-b x2) (str2-c x2)
				  (str2-d x2) (str2-e x2) (str2-f x2))
			    '(5 7 10 15 20 nil))))
(should-be-true (let ((x2 #S(str2 a 5 e 20 b 7)))
		     (equal (list (str2-a x2) (str2-b x2) (str2-c x2)
				  (str2-d x2) (str2-e x2) (str2-f x2))
			    '(5 7 10 15 20 nil))))

(should-work (defstruct (str3) a b))
(should-work (defstruct (str4 (:conc-name xx) (:include str3 a)) c ))
(should-work (setq x1 #s(str4 :b 15)))
(should-be-true (eql (xxb x1) 15))
(should-fail (make-str3 :c 30))		; invalid key
(should-be-true (eql (setf (xxa x1) 10) 10))
(should-be-true (eql (xxa x1) 10))
(should-be-true (eql (setf (str3-b x1) 10) 10))
(should-be-true (eql (str3-b x1) 10))
(should-fail (str3-b #(1 2 3)))
(should-be-false (str3-p #(1 2 3)))

(terpri)

(princ test-number)
(princ " tests performed.")
(terpri)

(princ bugs-found)
(princ " bugs found.")
(terpri)
