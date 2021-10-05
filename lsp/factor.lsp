; Prime number and factoring functions.
; Author - Tom Almy
; Date - May 2011

#+:packages
(unless (find-package "FACTOR")
        (make-package "FACTOR" :use '("XLISP")))
(in-package "FACTOR")
(export '(*primes* factor primep nextprime))

(defvar *primes* '(2 3 5 7 11 13 17 19 23)
"*primes* - Expanding list of prime numbers used by FACTOR, PRIMEP, and NEXTPRIME"
        )

(defun factor (val &optional (primes *primes*))
"(factor <val> [<primes>])                                    FIND PRIME FACTORS
         <val>         value to factor. Must be positive integer
         <primes>      list of primes which may be expanded. If not supplied
                       then *primes* is used.
         returns       list of prime factors"
        
	 (case val
	       (1 (list 1)) ; special cases
	       (2 (list 2))
	       (t 
		  (let ((final (ceiling (sqrt val))))
		       (loop
			(when (> (car primes) final) (return (list val)))
			(when (zerop (rem val (car primes)))
			      (return (cons (car primes) (factor (/ val (car primes)) primes))))
			(when (null (cdr primes)) ;; we must add a prime
			      (addprime))
			(setf primes (cdr primes)))))))
  

(defun primep (val &optional (primes *primes*))
"(primep <val> [<primes>])                                    IS VALUE PRIME?
       val        value, which must be positive integer.
       primes     list of primes which may be expanded. If not supplied,
                  *primes* is used
       returns    T if val is prime, else NIL"
       (case val
	     (1 nil) ; handle special cases
	     (2 t)
	     (t
	      (let ((final (ceiling (sqrt val))))
		   (loop
		    (when (> (car primes) final) (return t))
		    (when (zerop (rem val (car primes)))
			  (return nil))
		    (when (null (cdr primes))
			  (addprime))
		    (setf primes (cdr primes)))))))

(defun addprime (&optional (primes *primes*))
"Add another prime number to the list of primes. If list not supplied,
*primes* is used"
       (do ((trial (+ 2 (car (last primes))) (+ 2 trial)))
           ((primep trial) ;; It's a prime
            (nconc primes (list trial)))))

(defun nextprime (val)
"(nextprime <val>)                                       FIND NEXT PRIME NUMBER
       val      number, which need not be prime but must be a positive integer.
       returns  the next prime number after val"
       
       (do ((v (+ val (if (evenp val) 1 2)) (+ 2 v)))
	   ((primep v) v)))

(in-package "USER")
(use-package "FACTOR")
(provide "factor")
