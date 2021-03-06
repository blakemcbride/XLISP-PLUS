;;
;; The EVAL function in the original XLISP evaluated in the current lexical
;; context. This was changed to evaluate in the NIL (global) context to
;; match Common Lisp. But this created a problem: how do you EVAL an 
;; expression in the current lexical context?
;;
;; The answer is you can use the evalhook facility. The evalhook function
;; will evaluate an expression using an environment given to it as an
;; argument. But then the problem is "how do you get the current 
;; environment?" Well the getenv macro, below obtains the environent by
;; using an *evalhook* form.
;;
;; The following two macros do the job. Insteading of executing (eval <expr>)
;; just execute (eval-env <expr>). If you want, you can dispense with the
;; macros and execute:
;;
;;(evalhook <expr> nil nil (let ((*evalhook* (lambda (x env) env)))
;;				(eval nil)))
;;
;; Tom Almy  10/91
;;

#+:packages
(unless (find-package "EXT")
	(make-package "EXT" :use '("XLISP")))

(in-package "EXT")

(export '(eval-env))

(defmacro getenv () 
	  '(let ((*evalhook* (lambda (x env) env)))
		(eval nil)))	; hook function evaluates by returning 
				; environment
				
(defmacro eval-env (arg)	" EVAL that evaluates in current environment"
	  `(evalhook ,arg nil nil (getenv)))

(in-package "USER")
(provide "evalenv")
