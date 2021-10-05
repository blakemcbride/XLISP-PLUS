; Current shortcomings --
; 1. You can't value accumulate into an anonomous value using different keywords. "collect i append j" won't work, however
;    "collect i into x append j into x finally return x" will work. I don't have a reasonable fix for this.
;    No fix proposed -- it would require a multi-pass LOOP compiler.
; 2. AND not allowed to group FOR clauses.
;; Item 3 is no longer an issue, it now works like CLISP.
; 3. I consider FOR i FROM x TO i to be ill-defined since the FROM is evaluated first, should it be immediately assigned to i in which case
;    the TO i gets you nowhere, or is assignment really simultaneous in which case the value of i before the LOOP would be used for the TO.
;    CLISP goes with the latter, but I can't find any formal definition for this. I'm going with the former. If you want this to work
;    then do FOR i TO i FROM x
; 4. Shadowed symbols not handled in FOR BEING THE xxxSYMBOL...
; An extension -- IT is allowed beyond the first clause in a conditional. You can put NAMED, INITIALLY, FINALLY clauses anywhere -- they
; get properly sorted.

(unless (find-package "XLISP_LOOP") (make-package "XLISP_LOOP" :use '("XLISP")))
(use-package "XLISP_LOOP")
(in-package "XLISP_LOOP")

; We will be overriding an existing function, and need to keep access to the original
(when (eq (type-of (symbol-function 'loop))
	  'fsubr)
      (setf (symbol-function 'orig-loop)
	    (symbol-function 'loop)))

(defmacro appl (target &rest lists)
          `(setq ,target (nconc ,target (list ,@lists))))                                   


(defun deconstruct (target source &optional nonunique) ; At the top level the source should be a gensym assigned from the real source
                                   ; returns a list that needs to be consed to setf or setq
       (cond ((null target) nil) ; nil
             ((consp target)
              (nconc 
                     (cond ((null (car target)) nil)
                           ((symbolp (car target))
                            (unless nonunique (add_unique (check_unique (car target))))
                            `(,(car target) (car ,source)))
                           ((and (consp (car target)) (null (cdr target))) ; shortcut
                            (nconc `(,source (car ,source))
                                   (deconstruct (car target) source nonunique)))
                           ((consp (car target))
                            (let ((g (gensym)))
                                 (push g lvars)
                                 (nconc `(,g (car ,source))
                                        (deconstruct (car target) g nonunique))))
                           (t (error "I don't know how to deconstruct ~s" (car target))))
                     (cond ((null (cdr target)) nil)
                           ((symbolp (cdr target))
                            (unless nonunique (add_unique (check_unique (cdr target))))
                            `(,(cdr target) (cdr ,source)))
                           ((and (consp (cdr target)) (symbolp (cadr target)) (null (cddr target))) ; shortcut
                            (deconstruct (cdr target) `(cdr ,source) nonunique))
                           ((consp (cdr target))
                            (nconc `(,source (cdr ,source))
                                   (deconstruct (cdr target) source nonunique)))
                           (t (error "I don't know how to deconstruct ~s" (cdr target))))))
             (t (error "I don't know how to deconstruct ~s" target))))             


(defun deconslet (target source) ; This is the "let" version of deconstruct. At the top level the source
                                 ; should be a gensym assigned from the real source
       (cond ((null target) nil) ; nil
             ((consp target)
              (cond ((null (car target)) nil) ; handle CAR
                    ((symbolp (car target))
                     (check_unique (car target))
                     (add_unique (car target) `(car ,source)))
                    ((and (consp (car target)) (null (cdr target))) ; shortcut
                     (deconslet (car target) `(car ,source)))
                    ((consp (car target)) ; we need to recurse for car being list
                     (let ((g (gensym)))
                          (add_unique g `(car ,source))
                          (deconslet (car target) g)))
                    (t (error "I don't know how to deconstruct ~s" (car target))))
              (cond ((null (cdr target)) nil) ; handle CDR
                    ((symbolp (cdr target))
                     (check_unique (cdr target))
                     (add_unique (cdr target)  `(cdr ,source)))
                    ((and (consp (cdr target)) (symbolp (cadr target)) (null (cddr target))) ; shortcut
                     (check_unique (cadr target))
                     (add_unique (cadr target) `(cadr ,source)))
                    ((consp (cdr target))
                     (deconsletcdr (cdr target) source))
                    (t (error "I don't know how to deconstruct ~s" (cdr target)))))
             (t (error "I don't know how to deconstruct ~s" target))))             

(defun deconsletcdr (target source) ; same as deconslet but we need to use the cdr of the source, so will do a (setq source (cdr source))
       (cond ((null target) nil) ; nil
             ((consp target)
              (cond ((null (car target)) nil) ; handle CAR
                    ((symbolp (car target))
                     (check_unique (car target))
                     (add_unique (car target) `(car (setq ,source (cdr ,source)))))
                    ((and (consp (car target)) (null (cdr target))) ; shortcut (no cdr to process)
                     (deconslet (car target) `(cadr ,source)))
                    ((consp (car target)) ; we need to recurse for car being list
                     (let ((g (gensym)))
                          (add_unique g `(car (setq ,source (cdr ,source))))
                          (deconslet (car target) g)))
                    (t (error "I don't know how to deconstruct ~s" (car target))))
              (cond ((null (cdr target)) nil) ; handle CDR
                    ((symbolp (cdr target))
                     (check_unique (cdr target))
                     (add_unique (cdr target)  `(cdr ,source)))
                    ((and (consp (cdr target)) (symbolp (cadr target)) (null (cddr target))) ; shortcut
                     (check_unique (cadr target))
                     (add_unique (cadr target) `(cadr ,source)))
                    ((consp (cdr target))
                     (deconsletcdr (cdr target) source))
                    (t (error "I don't know how to deconstruct ~s" (cdr target)))))
             (t (error "I don't know how to deconstruct ~s" target))))            

       	       
; Global variables
(setq myinitially nil)  ; INITIALLY clauses    
(setq loop_test nil)    ; loop test clauses
(setq body nil)         ; body clauses
(setq sequencing nil)   ; sequencing clauses
(setq myfinally nil)    ; FINALLY clauses
(setq result_calc nil)  ; result calculation clause
(setq inform nil element nil) ; parser variables
(setq cgensym nil)       ; Implicit Accumulation variable
(setq cgensyminit nil)   ; Initial value of cgensym
(setq cresult nil)       ; Implicit accumlation type: 'collect 'append 'nconc 'number 't 'isnil
(setq named nil)    ; nil if unassigned
(setq itgensym nil) ; Current IT, nil if not allowed, T if unassigned, otherwise the uninterned symbol

(setq lvars nil) ; list of values for LET*, from last bound to first bound. Format either v or (v initial_v).
                 ; Also allowed is a group of parallel values as a list of values ((v1 initialv1) (v2 initialv2)...)
                 ; which will be initialized as a group using LET.
                 ; This gets all uvars, avars, and gensymed variable names.
(setq uvars nil) ; Unique names (for LET and WITH)
(setq avars nil) ; Association list of accumulation variables. associates with type 'list or 'number

(defun parseout (x)
       (setq inform x)
       (do ((v (parse) (parse)))
           ((null (cdr v)))
           (print v)))

(defun check_unique (var) ; Check for unique name, and add only to unique list
       (cond ((or (member var uvars) (assoc var avars))
              (error "Variable ~s needs to be uniquely declared in LOOP" var))
             ((constantp var)
              (error "Can't bind to constant ~s" var))
             ( t (push var uvars)))
       var)

(defun add_unique (var &optional initial) ; Adds known unique to lists
       (push (if initial (list var initial) var) lvars)
       var)

(defun check_accum (var type &optional initial) ; Check for accumlation variable, add if not yet defined
       (when (member var uvars) (error "Accumlation variable ~s name has been used in FOR or WITH" var))
       (when (constantp var) (error "Can't bind to constant ~s" var))
       (let ((found (assoc var avars)))
            (cond ((null found) ; new variable name
                   (add_unique var initial)
                   (push (cons var type) avars)) 
                  ((not (eq (cdr found) type))
                   (error "Variable ~s accumulation type cannot be both ~s and ~s" var type (cdr found)))
                  (t nil)))) ; do nothing -- its all good!

; We build a structure
; (let  (<lvars>)  ;; Multiple LET and LET* as needed for all the variables
;       (block <named>        ; Allows exit
;              (tagbody 
;                       ,@myinitially
;                       label1     ; looping label
;                       <loop_test>
;                       <body>
;                       <sequencing>
;                       (go label1)
;                       label2
;                       <myfinally>)
;              <result_calc>))

     

(defun build_lets (varlist body) ; Using iteration, create the nested LET and LET*
       (do ((args nil)
            (vl varlist (cdr vl)))
           ((null vl)
            (if args `(let* ,args ,body) body))
           (if (and (consp (car vl)) (consp (caar vl))) ; it is a parallel init list
               (progn
                (when args (setq body `(let* ,args ,body)) (setq args nil))
                (setq body `(let ,(car vl) ,body)))
               (push (car vl) args))))

(defun make_result () ; create the result loop structure from all the global lists
       (when cgensym (add_unique cgensym cgensyminit))
       (build_lets lvars
                   `(block ,named
                           (tagbody ,@myinitially
                                    label1 ; This will be in a separate package so no clashing!
                                    ,@loop_test
                                    ,@body
                                    ,@sequencing
                                    (go label1)
                                    label2
                                    ,@myfinally)
                           ,@result_calc)))

(defun parse () ; returns (element . type)  where type is either loop keyword or 'list or 'sym or NIL at end
       (if (null inform)
           (cons nil nil) ; Nothing left to parse
           (let ((e (car inform)))
                (setq inform (cdr inform)) ; advance forward
                (cond ((consp e) (cons e 'list))
                      ((not (symbolp e)) (cons e 'other)) ; claim we are something else
                      (t (do ((keys +loopkeywords+ (cdr keys))
                              (val (symbol-name e)))
                             ((or (null keys) (string= val (symbol-name (car keys))))
                              (cons e (if (null keys) 'sym (car keys))))))))))

(defmacro loop (&rest args)
"(loop <expr>...)                                         BASIC LOOPING FORM
     fsubr
     <expr>    the body of the loop
     returns   never returns (must use non-local exit, such as RETURN)
(loop [named <name>]                                   ADVANCED LOOPING FORM
      {<initialization and stepping clauses>}
      {initially <expr>...}
      {<body clauses>}{finally <expr>...}) 
      named <name> -- <name> is name of block, allowing non-local return
Initialization and Stepping clauses (create local variables, all expressions
          evaluated once, sequentially at start)
      with <var> [= <expr>] {and <varn> [= <exprn>]} -- declare local variables
          with optional initial values. Assignments done in parallel.
      for/as <var>
       [from/upfrom/downfrom <expr1>]
       [to/upto/downto/above/below <expr2>]
       [by <expr3>]
          default from is 0, default to gives no test, default by is 1,
          expressions may be in any order, direction defaults to up
      for/as <var> in <expr1> [by <step-fcn>] -- step over cars in expr1.
          The default step function is #'cdr
      for/as <var> on <expr1> [by <step-fcn>] -- step over list expr1
          The default step function is #'cdr
      for/as <var> = <expr1> [then <expr2>] -- assign from expr1 at start,
          successive iterations evaluate and assign from expr2. Default
          for expr2 is expr1.
      for/as <var> across <expr1> -- step across array, expr1.
      for/as <var> being each/the hash-key/hash-keys in/of <hashtable>
        [using (hash-value/hash-values <var2>)]
          Step through a hash table by keys.
      for/as <var> being each/the hash-value/hash-values in/of <hashtable>
        [using (hash-tag/hash-tags <var2>)]
          Step through a hash table by values
      for/as <var> being each/the
       symbol/symbols/external-symbol/external-symbols/present-symbol/present-symbols
       in/of <package>                                                                                             
          Step through symbols in a package. present are non-exported,
          external are exported. symbol is all visible to package.
      repeat <expr> -- repeats expr times maximum
Body clauses
      do/doing <expr>... -- evaluates expressions
      return <expr> -- returns the expression, does not continue in loop
      while <expr> -- continue if expr is not nil, else return (nil)
      until <expr> -- continue if expr is nil, else return
      always <expr> -- if expr is not nil, return, otherwise return T at end of loop.
      never <expr> -- if expr is nil, return, otherwise return T at end of loop.
      thereis <expr> -- if expr not nil, return expr, else return NIL at end of loop.
Accumulating body clauses -- into var acumulates to a named variable which is not
  returned by default.
      collect/collecting <expr> [into <var>] -- returns consed list of expressions
      append/appending <expr> [into <var>] -- returns appended list of expressions
      nconc/nconcing <expr> [into <var>] -- returns nconced list of expressions
      sum/summing <expr> [into <var>] -- returns sum of expressions
      count/counting <expr> [into <var>] -- returns number of non-nil expressions
      minimize/minimizing <expr> [into <var>] -- returns the minimum of the expressions
      maximize/maximizing <expr> [into <var>] -- returns the maximum of the expressions
Conditional body clauses -- these nest. Can use IT as argument to RETURN or
   accumulating clauses which represents the value of expr.
      if/when <expr> <clause> {and clause} [else <clause> {and <clause>}] [end]
      when <expr> <clause> {and clause} [else <clause> {and <clause>}] [end]
Finally clause -- executed before loop exit. Return here overrides default return
      finally [do] {<expr>} [return <expr>]"   
         
          (if (or (null args) (consp (car args))) ; Use the original loop
              `(orig-loop ,@args)
              (progn
               ;; Initialize the parser
               (setq inform args)
               ;; Clear out all the global lists and variables
               (setq loop_test nil body nil sequencing nil result_calc nil
                     myinitially nil myfinally nil cgensym nil cresult nil named nil itgensym nil
                     lvars nil avars nil uvars nil)
               (setq element (parse)) ;; Prime the parser
               (do ()
                   ((null (cdr element)) (make_result)) ; finished
                   (when (member (cdr element) '(list other)) ; keyword expected, reject anything else
                         (error "Keyword expected at ~s" (car element)))
                   (when (eq (cdr element) 'sym)
                         (error "~s is not a loop keyword" (car element)))
                   (let ((func (get (cdr element) :loopfcn))) ; find and process keyword
                        (when (null func) (error "Keyword ~s not expected before ~s" (car element) inform))
                        (funcall func))))))

(defun parseclause () ; parse clause in a conditional construct
        (if (member (cdr element) '(with for as repeat initially finally)) ; these aren't allowed in a conditional construct
            (error "Keyword ~s is not allowed in a LOOP conditional" (car element))
            (let ((func (get (cdr element) :loopfcn)))
                   (when (member (cdr element) '(list other)) ; keyword expected, reject anything else
                         (error "Keyword expected at ~s" (car element)))
                   (when (eq (cdr element) 'sym)
                         (error "~s is not a loop keyword" (car element)))
                 (when (null func) (error "Keyword ~s not expected in LOOP conditional" (car element)))
                 (funcall func))))

(defun possibly-replace-with-it (member) ; If expression is "it" then do a magic substitution!
       (if (and (eq (cdr member) 'sym) (string= "IT" (symbol-name (car member)))) ; got one!
           (progn
            (unless itgensym (error "Cannot use IT outside of conditional in LOOP"))
            (when (eq itgensym t) (setq itgensym (gensym)))
            (cons itgensym 'sym))
           member))

; Keywords:
(defconstant +loopkeywords+ '(for as from to downto upto upfrom downfrom above below by in on repeat with and into
                                  return do collect sum initially finally always never thereis while until
                                  append nconc count minimize maximize = then across if when unless else end
                                  collecting appending nconcing counting summing minimizing maximizing doing
                                  named
				  being each the of hash-key hash-keys hash-value hash-values symbol present-symbol
				  external-symbol symbols present-symbols external-symbols using
                                 ))

;; ** Variable initialization and stepping **
; FOR/AS <var> FROM <expr1> TO <expr2>  OR FOR/AS <var> FROM <expr1> DOWNTO <expr2>
; FROM can also be DOWNFROM to indicate decrementing or UPFROM to indicate incrementing. FROM defaults to 0 for incrementing.
; TO can be DOWNTO to indicate decrementing or UPTO to indicate incrementing. Also ABOVE and BELOW excludes final value.
; If omitted there is no test.
; Optional BY to give increment or decrement value. Value is always positive and default is 1.

; FOR <var> IN <expr1> [BY <step-fcn>]

; FOR <var> ON <expr1> [BY <step-fcn>]

; FOR <var> = <expr1> [THEN <expr2>]

; FOR <var> ACROSS <expr1>

; FOR <var> BEING EACH/THE HASH-KEY/HASH-KEYS IN/OF <hashtable> [USING (HASH-VALUE/HASH-VALUES <var2>)]

; FOR <var> BEING EACH/THE HASH-VALUE/HASH-VALUES IN/OF <hashtable> [USING (HASH-TAG/HASH-TAGS <var2>)]

; FOR <var> BEING EACH/THE SYMBOL/SYMBOLS/EXTERNAL-SYMBOL/EXTERNAL-SYMBOLS/PRESENT-SYMBOL/PRESENT-SYMBOLS IN/OF <package>                                                                                             

(defun for-count (var args &aux downward toval byval (bys 0) old_lvars)
       (setq old_lvars lvars lvars nil)
       (do ((alist args (cdr alist)) ; check argument list for consistancy, set defaults
            (froms 0)
            (tos 0)
            (upward nil))
           ((null alist)
            (when (or (> froms 1) (> tos 1) (> bys 1)) (error "Too many subclauses in FOR arithmetic"))
            (when (zerop (+ froms tos bys)) (error "At least one subclause required in FOR arithmetic"))
            (when (and upward downward) (error "Can't interate in both directions in FOR arithmetic"))
            (when (and downward (zerop froms)) (error "No default initial value going downward in FOR arithmetic"))
            (when (zerop froms) (setq args (cons (cons 'from 0) args))) ; default from is 0
            )
           (case (caar alist)
                 (from (incf froms))
                 (downfrom (incf froms) (setq downward t))
                 (upfrom (incf froms) (setq upward t))
                 (to (incf tos))
                 (downto (incf tos) (setq downward t))
                 (upto (incf tos) (setq upward t))
                 (above (incf tos) (setq downward t))
                 (below (incf tos) (setq upward t))
                 (by (incf bys))
                 (then (error "Invalid THEN subclause in FOR arithmetic"))))
       (do ((alist (reverse args) (cdr alist))) ;; process provided subclauses
           ((null alist))
           (case (caar alist)
                 ((from upfrom downfrom)
                  (add_unique (check_unique var) (cdar alist)))
                 (by
                  (if (numberp (cdar alist))
                      (setq byval (cdar alist))
                      (add_unique (setq byval (gensym)) (cdar alist)))
                  (appl sequencing (if downward
                                       `(decf ,var ,byval)
                                       `(incf ,var ,byval))))
                 ((to upto downto)
                  (if (numberp (cdar alist))
                      (setq toval (cdar alist))
                      (add_unique (setq toval (gensym)) (cdar alist)))
                  (appl loop_test (if downward
                                      `(when (< ,var ,toval) (go label2))
                                      `(when (> ,var ,toval) (go label2)))))
                 ((above below)
                  (if (numberp (cdar alist))
                      (setq toval (cdar alist))
                      (add_unique (setq toval (gensym)) (cdar alist)))
                   (appl loop_test (if downward
                                      `(when (<= ,var ,toval) (go label2))
                                      `(when (>= ,var ,toval) (go label2)))))))
        (when (zerop bys) ; default increment/decrement
             (appl sequencing (if downward
                                  `(decf ,var)
                                  `(incf ,var))))
       (setq lvars (cons lvars old_lvars))
       )
                  
       
  
(defun for-in (var args &aux (gensym (gensym)) (in (cdr (assoc 'in args))) (step (cdr (assoc 'by args))))
       (when (> (length args) (if step 2 1)) (error "Too many subclauses in FOR IN"))
       (add_unique gensym in)
       (appl loop_test
             `(when (endp ,gensym) (go label2))
             (if (consp var)
                 (let ((gensym2 (add_unique (gensym))))
                      `(setf ,gensym2 (car ,gensym) ,@(deconstruct var gensym2)))
                 `(setf ,(add_unique (check_unique var)) (car ,gensym))))
       (appl sequencing (if step
                            `(setf ,gensym (apply ,step (list ,gensym)))
                            `(setf ,gensym (cdr ,gensym)))))
       

(defun for-on (var args &aux (on (cdr (assoc 'on args))) (step (cdr (assoc 'by args))))
       (when (> (length args) (if step 2 1)) (error "Too many subclauses in FOR ON"))
       (if (consp var)
           (let ((gensym (add_unique (gensym) on))
                 (gensym2 (add_unique (gensym))))
                (appl loop_test
                      `(when (endp ,gensym) (go label2))
                      `(setf ,gensym2 ,gensym ,@(deconstruct var gensym2)))
                (appl sequencing (if step
                                     `(setf ,gensym (apply ,step (list ,gensym)))
                                     `(setf ,gensym (cdr ,gensym)))))
           (progn
            (appl loop_test `(when (endp ,(add_unique (check_unique var) on)) (go label2)))
            (appl sequencing (if step
                                 `(setf ,var (apply ,step (list ,var)))
                                 `(setf ,var (cdr ,var)))))))

(defun for-= (var args &aux (init (cdr (assoc '= args))) (then (cdr (assoc 'then args)))) 
       (when (> (length args) (if then 2 1)) (error "Too many subclauses in FOR ="))
       (when (null then) (setq then init))
       (if (consp var)
           (let ((gensym (add_unique (gensym) init)))
                (deconslet var gensym)
                (appl sequencing `(setf ,gensym ,then ,@(deconstruct var gensym t))))
           (progn
            (add_unique (check_unique var) init)
            (appl sequencing `(setf ,var ,then)))))
       

(defun for-across (var args &aux (across (cdr (assoc 'across args))) (vector (gensym)) (index (gensym)))
       (when (> (length args) 1) (error "Too many subclauses in FOR ACROSS"))
       (add_unique (check_unique var))
       (add_unique vector across)
       (add_unique index 0)
       (appl loop_test
             `(when (>= ,index (length ,vector)) (go label2))
             `(setq ,var (aref ,vector ,index)))
       (appl sequencing `(incf ,index)))

(defun for-hashkey (var obj using gensym &aux (gensym2 (gensym)))
       (add_unique gensym `(let (,gensym2) (maphash #'(lambda (x y) (push (cons x y) ,gensym2)) ,obj) ,gensym2))
       (when (and using (member (car using) '(hash-key hash-keys)))
	     (error "Invalid USING - ~s" using))
       (appl loop_test
	     `(when (endp ,gensym) (go label2))
	     (if using
		 `(setf ,var (caar ,gensym) ,(cdr using) (cdar ,gensym))
		 `(setf ,var (caar ,gensym))))
       (appl sequencing `(setf ,gensym (cdr ,gensym)))
       )

(defun for-hashvalue (var obj using gensym &aux (gensym2 (gensym)))
       (add_unique gensym `(let (,gensym2) (maphash #'(lambda (x y) (push (cons y x) ,gensym2)) ,obj) ,gensym2))
       (when (and using (member (car using) '(hash-value hash-values)))
	     (error "Invalid USING - ~s" using))
       (appl loop_test
	     `(when (endp ,gensym) (go label2))
	     (if using
		 `(setf ,var (caar ,gensym) ,(cdr using) (cdar ,gensym))
		 `(setf ,var (caar ,gensym))))
       (appl sequencing `(setf ,gensym (cdr ,gensym)))
       )

(defun for-symbols (var obj gensym &aux (gensym2 (gensym)))
       (add_unique gensym `(nconc
			    (mapcan #'(lambda (,gensym2) (mapcan #'copy-list (coerce (package-obarray ,gensym2) 'list)))
				    (cons ,obj (package-use-list ,obj)))
			    (mapcan #'copy-list (coerce (package-obarray ,obj nil) 'list))))
       (appl loop_test
	     `(when (endp ,gensym) (go label2))
	     `(setf ,var (car ,gensym)))
       (appl sequencing `(setf ,gensym (cdr ,gensym)))
       )

(defun for-present (var obj gensym)
       (add_unique gensym `(mapcan #'copy-list (coerce (package-obarray ,obj nil) 'list)))
       (appl loop_test
	     `(when (endp ,gensym) (go label2))
	     `(setf ,var (car ,gensym)))
       (appl sequencing `(setf ,gensym (cdr ,gensym)))
       )

(defun for-external (var obj gensym)
       (add_unique gensym `(mapcan #'copy-list (coerce (package-obarray ,obj t) 'list)))
       (appl loop_test
	     `(when (endp ,gensym) (go label2))
	     `(setf ,var (car ,gensym)))
       (appl sequencing `(setf ,gensym (cdr ,gensym)))
       )

(defun for-hashpackage (var &aux type obj using (gensym (gensym)))
       (add_unique (check_unique var))
       (setq element (parse)) ; should be EACH or THE
       (unless (member (cdr element) '(each the))
	       (error "~s? EACH or THE expected in FOR ~s BEING" (car element) var))
       (setq type (cdr (setq element (parse)))) ; Get type
       (unless (member type '(hash-key hash-keys hash-value hash-values symbol present-symbol external-symbol
				       symbols present-symbols external-symbols))
	       (error "~s is invalid type in FOR ~s BEING" (car element) var))
       (setq element (parse)) ; should be IN or OF
       (unless (member (cdr element) '(in of))
	       (error "~s? IN OF expected in FOR ~s BEING" (car element) var))
       (setq element (parse)) ; obj
       (when (eq (cdr element) nil) (error "Unexpected end of LOOP"))
       (setq obj (car element))
       (setq element (parse)) ; possible using
       (when (and (eq (cdr element) 'using) (member type '(hash-key hash-keys hash-value hash-values)))
	     (setq element (parse))
	     (if (and (consp (car element)) (symbolp (caar element)) (symbolp (cadar element)) (endp (cddar element))
		      (setq using (member-if #'(lambda (x) (string= (symbol-name x) (symbol-name (caar element))))
				 '(hash-key hash-keys hash-value hash-values))))
		 (progn (setq using (cons (car using) (add_unique (check_unique (cadar element))))) ; get actual match
			(setq element (parse)))
		 (error "Unknown USING - ~s" (car element))))
       (case type
	     ((hash-key hash-keys) (for-hashkey var obj using gensym))
	     ((hash-value hash-values) (for-hashvalue var obj using gensym))
	     ((symbol symbols) (for-symbols var obj gensym))
	     ((present-symbol present-symbols) (for-present var obj gensym))
	     ((external-symbol external-symbols) (for-external var obj gensym))
	     (t (error "Invalid type?? ~s" type))))
	      

       
(setf (get 'for :loopfcn)
      (lambda (&aux var args)
              (setq element (parse))
              (when (null (cdr element)) (error "Unexpected end of LOOP"))
              (setq var (car element))
              (setq element (parse))
	      (if (eq (cdr element) 'being) ; is it a package or a hash?
		  (for-hashpackage var)
		  (progn 
			 (do ((key element (setq element (parse)))) ;; collect all subclauses
			     ((null (member (cdr key) '(in on by from downfrom upfrom to upto downto above below = then across))) ; not a keyword
			      nil) ; return nothing
			     (setq element (parse)) ; get argument
			     (when (null (cdr element)) (error "Unexpected end of LOOP"))
			     (when (assoc (cdr key) args) (error "FOR keyword ~s can only be used once" key))
			     (setq args (cons (cons (cdr key) (car element)) args)))
			 (cond ((assoc 'in args) (for-in var args)) ; process based on type of for
			       ((assoc 'on args) (for-on var args))
			       ((assoc '= args) (for-= var args))
			       ((assoc 'across args) (for-across var args))
			       (t (for-count var args)))))))
                             
              



              
; REPEAT <expr>

(setf (get 'repeat :loopfcn)
      (lambda ()
              (setq element (parse)) ; get expression
              (when (null (cdr element)) (error "Unexpected end of LOOP"))
              (let ((var (gensym)))
                   (add_unique var (car element))
                   (appl loop_test `(unless (> ,var 0) (go label2)))
                   (appl sequencing `(decf ,var)))
              (setq element (parse))
              )
      )
                   

; WITH <var> [= <expr>] {AND <varn> [= <exprn>]}

(setf (get 'with :loopfcn)
      (lambda (&aux vars exprs deslist)
	      (prog ()
		    getnext
		    (setq element (parse))	; get var
		    (when (null (cdr element)) (error "Unexpected end of LOOP"))
            (cond ((consp (car element)) ; destructure this
                   (let ((target (gensym)))
                        (push target vars)
                        (push (cons (car element) target) deslist)))
                 ((and (car element) (symbolp (car element)))
                   (check_unique (car element))
                   (push (car element) vars))
                  (t (error "~s is not a valid variable name" (car element))))
		    (setq element (parse)) ; potential equals sign
		    (cond ((eq (cdr element) '=)
                   (setq element (parse)) ; get initial value
                   (push (car element) exprs)
                   (setq element (parse))) ; continue
                  (t (push nil exprs))) ; no initial value
		    (when (eq (cdr element) 'and) (go getnext))) ; Nothing left
	      (let ((l (mapcar #'(lambda (x y) (list x y)) vars exprs)))
		   (when l
			 (push (if (eql 1 (length l)) (car l) l) lvars)))
          (when deslist (mapc #'(lambda (x) (deconslet (car x) (cdr x)))
                              deslist))))


; ** Unconditional execution **
; RETURN <expr>
; DO <expr>...

(setf (get 'do :loopfcn)
      (lambda ()
              (do ((el (parse) (parse)))
                  ((not (eq (cdr el) 'list)) ; leave when not a list
                   (setq element el)) ; set the global before leaving
                  (appl body (car el)))))


(setf (get 'return :loopfcn)
      (lambda ()
              (let ((el (parse)))
                   (unless (cdr el) ; everything is valid here but null
                           (error "Something needs to follow RETURN"))
                   (appl body `(return ,(car (possibly-replace-with-it el))))
                   (setq element (parse)))))

;; ** Value accumulation **
; COLLECT <expr> [INTO <var>]                    

(setf (get 'collect :loopfcn)
      (lambda ()
              (let ((el (possibly-replace-with-it (parse))) expr var)
                   (when (null (cdr el)) (error "Unexpected end of LOOP"))
                   (setq expr (car el))
                   (setq element (parse)) ; might be INTO
                   (if (eq (cdr element) 'into)
                       (progn
                        (setq el (parse))
                        (when (or (null (car el)) (not (atom (car el)))) (error "~s is not a valid variable name" (car el)))
                        (check_accum (car el) 'list)
                        (appl body `(setq ,(car el) (nconc ,(car el) (list ,expr))))
                        (setq element (parse)))
                       (progn ; we will have to check for stepping on another collection type
                              (when (and cresult (not (eq cresult 'collect)))
                                    (error "conflicting types for implicit return value: ~s and ~s" cresult 'collect))
                              (setq cresult 'collect)
                              (unless cgensym
                                      (setq cgensyminit nil cgensym (gensym)))
                              (appl body `(setq ,cgensym (cons ,expr ,cgensym)))
                              (setq result_calc (list `(nreverse ,cgensym))))))))
		       
; APPEND <expr> [INTO <var>]

(setf (get 'append :loopfcn)
      (lambda ()
              (let ((el (possibly-replace-with-it (parse))) expr var)
                   (when (null (cdr el)) (error "Unexpected end of LOOP"))
                   (setq expr (car el))
                   (setq element (parse)) ; might be INTO
                   (if (eq (cdr element) 'into)
                       (progn
                        (setq el (parse))
                        (when (or (null (car el)) (not (atom (car el)))) (error "~s is not a valid variable name" (car el)))
                        (check_accum (car el) 'list)
                        (appl body `(setq ,(car el) (nconc ,(car el) (copy-list ,expr))))
                        (setq element (parse)))
                       (progn ; we will have to check for stepping on another collection type
                              (when (and cresult (not (eq cresult 'append)))
                                    (error "conflicting types for implicit return value: ~s and ~s" cresult 'append))
                              (setq cresult 'append)
                              (unless cgensym
                                      (setq cgensyminit nil cgensym (gensym)))
                              (appl body `(setq ,cgensym (cons ,expr ,cgensym)))
                              (setq result_calc (list `(apply #'append (nreverse ,cgensym)))))))))


; NCONC <expr> [INTO <var>]

(setf (get 'nconc :loopfcn)
      (lambda ()
              (let ((el (possibly-replace-with-it (parse))) expr var)
                   (when (null (cdr el)) (error "Unexpected end of LOOP"))
                   (setq expr (car el))
                   (setq element (parse)) ; might be INTO
                   (if (eq (cdr element) 'into)
                       (progn
                        (setq el (parse))
                        (when (or (null (car el)) (not (atom (car el)))) (error "~s is not a valid variable name" (car el)))
                        (check_accum (car el) 'list)
                        (appl body `(setq ,(car el) (nconc ,(car el) ,expr)))
                        (setq element (parse)))
                       (progn ; we will have to check for stepping on another collection type
                              (when (and cresult (not (eq cresult 'nconc)))
                                    (error "conflicting types for implicit return value: ~s and ~s" cresult 'nconc))
                              (setq cresult 'nconc)
                              (unless cgensym
                                      (setq cgensyminit nil cgensym (gensym)))
                              (appl body `(setq ,cgensym (cons ,expr ,cgensym)))
                              (setq result_calc (list `(apply #'nconc (nreverse ,cgensym)))))))))


; SUM <expr> [INTO <var>]

(setf (get 'sum :loopfcn)
      (lambda ()
              (let ((el (possibly-replace-with-it (parse))) expr var)
                   (when (null (cdr el)) (error "Unexpected end of LOOP"))
                   (setq expr (car el))
                   (setq element (parse)) ; might be INTO
                   (if (eq (cdr element) 'into)
                       (progn
                        (setq el (parse))
                        (when (or (null (car el)) (not (atom (car el)))) (error "~s is not a valid variable name" (car el)))
                        (check_accum (car el) 'number 0)
                        (appl body `(incf ,(car el) ,expr))
                        (setq element (parse)))
                       (progn ; we will have to check for stepping on another collection type
                              (when (and cresult (not (eq cresult 'number)))
                                    (error "conflicting types for implicit return value: ~s and ~s" cresult 'number))
                              (setq cresult 'number)
                              (unless cgensym
                                      (setq cgensyminit 0 cgensym (gensym)))
                              (appl body `(incf ,cgensym ,expr))
                              (setq result_calc (list cgensym)))))))

; COUNT <expr> [INTO <var>]

(setf (get 'count :loopfcn)
      (lambda ()
              (let ((el (possibly-replace-with-it (parse))) expr var)
                   (when (null (cdr el)) (error "Unexpected end of LOOP"))
                   (setq expr (car el))
                   (setq element (parse)) ; might be INTO
                   (if (eq (cdr element) 'into)
                       (progn
                        (setq el (parse))
                        (when (or (null (car el)) (not (atom (car el)))) (error "~s is not a valid variable name" (car el)))
                        (check_accum (car el) 'number 0)
                        (appl body `(when ,expr (incf ,(car el))))
                        (setq element (parse)))
                       (progn ; we will have to check for stepping on another collection type
                              (when (and cresult (not (eq cresult 'number)))
                                    (error "conflicting types for implicit return value: ~s and ~s" cresult 'number))
                              (setq cresult 'number)
                              (unless cgensym
                                      (setq cgensyminit 0 cgensym (gensym)))
                              (appl body `(when ,expr (incf ,cgensym)))
                              (setq result_calc (list cgensym)))))))



; MAXIMIZE/MINIMIZE <expr> [INTO <var>]

(setf (get 'maximize :loopfcn)
      (lambda ()
              (let ((el (possibly-replace-with-it (parse))) expr var)
                   (when (null (cdr el)) (error "Unexpected end of LOOP"))
                   (setq expr (car el))
                   (setq element (parse)) ; might be INTO
                   (if (eq (cdr element) 'into)
                       (progn
                        (setq el (parse))
                        (when (or (null (car el)) (not (atom (car el)))) (error "~s is not a valid variable name" (car el)))
                        (check_accum (car el) 'number)
                        (appl body `(setf ,(car el) (if ,(car el) (max ,(car el) ,expr) ,expr)))
                        (setq element (parse)))
                       (progn ; we will have to check for stepping on another collection type
                              (when (and cresult (not (eq cresult 'number)))
                                    (error "conflicting types for implicit return value: ~s and ~s" cresult 'number))
                              (setq cresult 'number)
                              (unless cgensym
                                      (setq cgensyminit nil cgensym (gensym)))
                              (appl body `(setf ,cgensym (if ,cgensym (max ,cgensym ,expr) ,expr)))
                              (setq result_calc (list cgensym)))))))

(setf (get 'minimize :loopfcn)
      (lambda ()
              (let ((el (possibly-replace-with-it (parse))) expr var)
                   (when (null (cdr el)) (error "Unexpected end of LOOP"))
                   (setq expr (car el))
                   (setq element (parse)) ; might be INTO
                   (if (eq (cdr element) 'into)
                       (progn
                        (setq el (parse))
                        (when (or (null (car el)) (not (atom (car el)))) (error "~s is not a valid variable name" (car el)))
                        (check_accum (car el) 'number)
                         (appl body `(setf ,(car el) (if ,(car el) (min ,(car el) ,expr) ,expr)))
                        (setq element (parse)))
                       (progn ; we will have to check for stepping on another collection type
                              (when (and cresult (not (eq cresult 'number)))
                                    (error "conflicting types for implicit return value: ~s and ~s" cresult 'number))
                              (setq cresult 'number)
                              (unless cgensym
                                      (setq cgensyminit nil cgensym (gensym)))
                              (appl body `(setf ,cgensym (if ,cgensym (min ,cgensym ,expr) ,expr)))
                              (setq result_calc (list cgensym)))))))


;; ** Termination Conditions **
; WHILE <expr>
; UNTIL <expr>

(setf (get 'while :loopfcn)
      (lambda ()
	      (let ((el (parse)))
		   (when (null (cdr el)) (error "Missing condition after WHILE"))
		   (appl body `(unless ,(car el) (go label2)))
		   (setq element (parse)))))

(setf (get 'until :loopfcn)
      (lambda ()
	      (let ((el (parse)))
		   (when (null (cdr el)) (error "Missing condition after UNTIL"))
		   (appl body `(when ,(car el) (go label2)))
		   (setq element (parse)))))

; ALWAYS <expr>

(setf (get 'always :loopfcn)
      (lambda ()
	      (let ((el (parse)))
		   (when (null (cdr el)) (error "Missing condition after ALWAYS"))
		   (appl body `(unless ,(car el) (return nil)))
		   (when (and (consp result_calc) (not (equal consp '(t))))
			 (error "ALWAYS conflicts with previous default return value"))
		   (setq result_calc (list 't))
		   (setq element (parse)))))

; NEVER <expr>

(setf (get 'never :loopfcn)
      (lambda ()
	      (let ((el (parse)))
		   (when (null (cdr el)) (error "Missing condition after NEVER"))
		   (appl body `(when ,(car el) (return nil)))
		   (when (and (consp result_calc) (not (equal consp '(t))))
			 (error "NEVER conflicts with previous default return value"))
		   (setq cresult 't)
		   (setq result_calc (list 't))
		   (setq element (parse)))))

; THEREIS <expr>

(setf (get 'thereis :loopfcn)
      (lambda ()
	      (let ((el (parse))
		    (gensym (gensym)))
		   (when (null (cdr el)) (error "Missing condition after THEREIS"))
		   (appl body `(let ((,gensym ,(car el))) (when ,gensym (return ,gensym))))
		   (when (and (consp result_calc) (not (equal consp '(nil))))
			 (error "thereis conflicts with previous default return value"))
		   (setq cresult 'isnil)
		   (setq result_calc (list 'nil))
		   (setq element (parse)))))


; NAMED <label>
; Name the block. This must be a symbol

(setf (get 'named :loopfcn)
      (lambda ()
              (setq element (parse))
              (when named (error "Cannot name the LOOP more than one time"))
              (unless (eq (cdr element) 'sym) (error "NAMED LOOP must be a symbol: ~s" (car element)))
              (setq named (car element))
              (setq element (parse))))

;; ** Conditional Execution
; IF <expr> <clause> {AND clause} [ELSE <clause> {AND <clause>}] [END]
; WHEN <expr> <clause> {AND clause} [ELSE <clause> {AND <clause>}] [END]
; UNLESS <expr> <clause> {AND clause} [ELSE <clause> {AND <clause>}] [END]

(setf (get 'if :loopfcn)
      (lambda ()
              (prog ((oldbody body) (flipped (eq (cdr element) 'unless)) it positive negative)
                    (setq itgensym t) ; allow use of it keyword
                    (setq element (parse)) ; get expression
                    (when (null (cdr element)) (error "Missing condition after IF/WHEN"))
                    (setq it (car element))
                    (setq body nil)
                    more-positive
                    (setq element (parse)) ; go beyond keyword
                    (when (null (cdr element)) (error "Unexpected end of LOOP conditional"))
                    (parseclause) ; which leaves us at the next keyword
                    (when (eq (cdr element) 'and) (go more-positive))
                    (setq positive body)
                    (setq body nil)
                    (when (not (eq (cdr element) 'else)) (go done-negative))
                    more-negative
                    (setq element (parse)) ; go beyond keyword
                    (when (null (cdr element)) (error "Unexpected end of LOOP conditional"))
                    (parseclause)
                    (when (eq (cdr element) 'and) (go more-negative))
                    done-negative
                    (when (eq (cdr element) 'end) (setq element (parse))) ; Toss optional END keyword
                    (setq negative body)
                    (setq body oldbody)
                    (appl body (if (and itgensym (not (eq itgensym t))) ; "it" was used
                                   (if flipped
                                       (if negative
                                           `(let ((,itgensym ,it)) (if ,itgensym (progn ,@negative) (progn ,@positive)))
                                           `(let ((,itgensym ,it)) (unless ,itgensym ,@positive)))

                                       (if negative
                                           `(let ((,itgensym ,it)) (if ,itgensym (progn ,@positive) (progn ,@negative)))
                                           `(let ((,itgensym ,it)) (when ,itgensym ,@positive))))
                                   (if flipped

                                       (if negative
                                           `(if ,it (progn ,@negative) (progn ,@positive))
                                           `(unless ,it ,@positive))

                                       (if negative
                                           `(if ,it (progn ,@positive) (progn ,@negative))
                                           `(when ,it ,@positive)))))
                    (setq itgensym nil) ; disallow use of it keyword
                    )))
 
;; ** Miscellaneous Operations **
; INITIALLY <expr>...
; FINALLY <expr>...
;  DO and RETURN are allowed after FINALLY!

(setf (get 'initially :loopfcn) (lambda ()
                                 (do ((el (parse) (parse)))
                                     ((not (eq (cdr el) 'list)) ; leave when not a list
                                       (setq element el)) ; set the global before leaving
                                     (appl myinitially (car el)))))

(setf (get 'finally :loopfcn) (lambda ()
                                 (do ((el (parse) (parse)))
                                     ((not (member (cdr el) '(list do return))) ; leave when not a list, do, or return
                                       (setq element el)) ; set the global before leaving
                                     (case (cdr el)
                                           (do nil)
                                           (return (progn (setq el (parse))
                                                           (unless (cdr el) (error "Something needs to follow RETURN"))
                                                           (appl myfinally `(return ,(car el)))))
                                           (t (appl myfinally (car el)))))))

;; SYNONYMS
(setf (get 'as :loopfcn) (get 'for :loopfcn))
(setf (get 'doing :loopfcn) (get 'do :loopfcn))
(setf (get 'collecting :loopfcn) (get 'collect :loopfcn))
(setf (get 'appending :loopfcn) (get 'append :loopfcn))
(setf (get 'nconcing :loopfcn) (get 'nconc :loopfcn))
(setf (get 'counting :loopfcn) (get 'count :loopfcn))
(setf (get 'summing :loopfcn) (get 'sum :loopfcn))
(setf (get 'minimizing :loopfcn) (get 'minimize :loopfcn))
(setf (get 'maximizing :loopfcn) (get 'maximize :loopfcn))
(setf (get 'when :loopfcn) (get 'if :loopfcn))
(setf (get 'unless :loopfcn) (get 'if :loopfcn)) ; not a synonym, but we check it internal to the loopfcn so can combine                          

; (loop-finish)
; Local macro that compiles a (go label2)

(in-package "XLISP")
(defmacro loop-finish ()
"(loop-finish)                                          EXIT LOOP
          macro
          This function will exit the body of an (advanced) loop,
          causing execution of any finally clause.
          "
          `(go xlisp_loop::label2))
(export 'loop-finish)
(in-package "XLISP_LOOP")

(push :loop *features*)
(provide "loop")
(in-package "USER")
