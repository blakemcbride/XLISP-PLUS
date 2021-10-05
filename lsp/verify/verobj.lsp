;; Assumes class variables are not inherited. 

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

#-:classes (progn
(setq a 310)	; some global bindings -- in case we don't see the instance/class variables
(setq b 311)
(setq c 312)
(setq d 313)
(setq e 314)
(setq f 315)

; If these fail, we will be very dead, so just execute them

(setq cl1 (send class :new '(a b) '(c d)))
(setq cl2 (send class :new '(e) '(f) cl1))
(setq cl3 (send class :new '() '() cl2))

(send cl1 :answer :setup '(x y) '((setq a x b y)))
(send cl2 :answer :setup '(x y z) '((setq a x b y e z)))
(send cl1 :answer :setupg '(x y) '((setq c x d y)))
(send cl2 :answer :setupg '(x y z) '((setq c x d y f z)))
(send cl1 :answer :getall '() '((list a b c d)))
(send cl2 :answer :getall '() '((list a b c d e f)))
(send cl3 :answer :getall '() '((list a b c d e f)))
(send cl2 :answer :getsup '() '((send-super :getall)))

(setq o1 (send cl1 :new))
(setq o2 (send cl2 :new))
(setq o3 (send cl3 :new))
            )

#+:classes (progn    ;; We can use defclass defmethod and definst
(setq a 310)	; some global bindings -- in case we don't see the instance/class variables
(setq b 311)
(setq c 312)
(setq d 313)
(setq e 314)
(setq f 315)

; If these fail, we will be very dead, so just execute them

(defclass cl1 (a b) (c d))
(defclass cl2 (e) (f) cl1)
(defclass cl3 () () cl2)

(defmethod cl1 :setup (x y) (setq a x b y))
(defmethod cl2 :setup (x y z) (setq a x b y e z))
(defmethod cl1 :setupg (x y) (setq c x d y))
(defmethod cl2 :setupg (x y z) (setq c x d y f z))
(defmethod cl1 :getall () (list a b c d))
(defmethod cl2 :getall () (list a b c d e f))
(defmethod cl3 :getall () (list a b c d e f))
(defmethod cl2 :getsup () (send-super :getall))

(definst cl1 o1)
(definst cl2 o2)
(definst cl3 o3)
            )



(should-work (send o1 :setup 10 20))
(should-work (send o2 :setup 11 21 31))
(should-work (send o3 :setup 12 22 32))
(should-work (send o1 :setupg 100 101))

(should-be-true (equal (send o1 :getall) '(10 20 100 101)))
(should-be-true (equal (send o2 :getall) '(11 21 312 313 31 nil)))
(should-work  (send o2 :setupg 200 201 202))
(should-be-true (equal (send o1 :getall) '(10 20 100 101)))
(should-be-true (equal (send o2 :getall) '(11 21 200 201 31 202)))
(should-be-true (equal (send o3 :getall) '(12 22 200 201 32 315)))
(should-be-true (equal (send o3 :getsup) '(12 22 100 101)))
(should-be-true (equal (send o2 :getsup) '(11 21 100 101)))



(terpri)

(princ test-number)
(princ " tests performed.")
(terpri)

(princ bugs-found)
(princ " bugs found.")
(terpri)
