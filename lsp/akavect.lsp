; This demonstration program changed to use packages. By doing so, a
; conflict in the function name "search" is avoided.

; To play, load this file. You can then do one of three things:
; 1. execute (akalah:play ...)
; 2. execute (use-package 'akalah) or (use-package "AKALAH"), then
;    you can execute (play ...)
; 3. execute (import 'akalah:play), then you can execute (play ...)

; Note that if you have defined the symbol "play" (or, for case 2, "meplay"
; as well) then you will get a correctable error. The response is to unintern
; the symbol in your current package (unintern 'play), then (continue).

; This is AKALAH.LSP modified to use an array for the playing field rather
;  than a list.  The major result is a modest increase in speed because of
;  greatly reduced garbage collections

; To play against the computer:
; (meplay #pits-per-side #pebbles-per-pit #search-depth #computer-plays-first)
;
; To have the computer play against itself:
; (play #pits-per-side #pebbles-per-pit #search-depth)


; The playing arena:
; 13    12 11 10 9  8  7
;	0  1  2  3  4  5     6 


; Pick up the pile of stones in a pit and seed them counterclockwise,
; except for the oponents kalah hole.
; If the last pebble lands in your own empty hole, pick up that pebble
;  and the one oposite of the opponents hole, and put them in ones kalah hole
; If the last pebble lands in your own kalah hole, play again
; If as a result of your move, you have no more pebbles, then your oponent
; takes all of his pebbles.
; The first player to get at greater than 50% of the pebbles wins.

(provide "akavect")

(unless (find-package "AKALAH")
	(make-package "AKALAH" :use '("XLISP")))

(in-package "AKALAH") ;; We don't have to leave this package since
                      ;; the package is part of the environment
                      ;; encapsulated with the LOAD function

(export '(play meplay))  ;; The only accessable functions

(shadow 'search)	;; We don't want to use the one in XLISP

(defun makefield (length contents)
	(let ((result (make-array length)))
	     (dotimes (i length) (setf (aref result i) contents))
	     result))

(defconstant *maxvalue* 1000)
(defconstant *minvalue* (- *maxvalue*))
(defconstant *firsta* 0)
(defvar *firstb*)
(defvar *enda*)
(defvar *endb*)
(defvar *moves*)
(defvar *halfall*)
(defvar *board*)
(defvar *lasta*)
(defvar *lastb*)

#+:generic (defmacro copy (array) `(generic ,array))	; not the generic solution!
#-:generic (defmacro copy (array) `(concatenate 'array ,array)) ;The generic solution

(defmacro empty (position hole)	; empty out the given hole
	`(setf (aref ,position ,hole) 0))

(defmacro kalaholefn (whoseturn)   ; the scoring hole for the given player
	`(if ,whoseturn *endb* *enda*))

(defmacro opposholefn (hole)	; calculate the opposing hole
	 `(- *lastb* ,hole))

(defmacro ownsidep (hole whoseturn)
	`(if ,whoseturn (> ,hole *enda*) (< ,hole *firstb*)))

(defmacro allownzerok (position whoseturn)
       `(zerop (countown ,position ,whoseturn)))

(defmacro prinb (x) `(dotimes (i ,x) (princ #\space)))

(defmacro wincheck (position whoseturn)
	`(> (aref ,position (kalaholefn ,whoseturn)) *halfall*))

(defun search (startpos depth whoseturn)
   (if (zerop depth)
       (list startpos (evaluate startpos whoseturn))
       (let (bestval nextval bestpos succlist)
	    (setq succlist (successorsfn startpos whoseturn))
	    (when (> depth 1) (setq succlist (reorder succlist whoseturn)))
	    (setq 
	        beta    *maxvalue*
	        bestval *minvalue*
		bestpos (car succlist))
	    (dolist (this succlist)
	    	(when (wincheck this whoseturn)
		    (return-from search (list this *maxvalue*)))
		(setq nextval (- (alphabeta this
					    (- beta)
					    (- bestval)
					    (1- depth)
					    (not whoseturn))))
		(when (> nextval bestval)
		      (setq bestval nextval)
		      (setq bestpos this)))
	    (list bestpos bestval))))	; return value

(defun alphabeta (position alpha beta depth whoseturn)
    (if (zerop depth)
    	(evaluate position whoseturn)
	(let (bestval nextval succlist)
	    (setq succlist (successorsfn position whoseturn))
	    (when (> depth 1) (setq succlist (reorder succlist whoseturn)))
	    (setq bestval alpha)
	    (dolist (this succlist)
	    	(when (wincheck this whoseturn)
		      (return-from alphabeta *maxvalue*))
		(setq nextval (- (alphabeta this
					    (- beta)
					    (- bestval)
					    (1- depth)
					    (not whoseturn))))
		(when (> nextval bestval) (setq bestval nextval))
		(when (<= beta bestval) (return-from alphabeta bestval)))
	    bestval)))

(defun successorsfn (position whoseturn 
			&aux 
			(picuphole (1- (if whoseturn *firstb* *firsta*)))
			succlist 
			succ 
			stones 
			disthole 
			lasthole)
   (dotimes (dummy *enda*)
	    (when (not (zerop (aref position (setq picuphole (1+ picuphole)))))
		  (setq	succ (copy position))
		  (setf (aref succ *moves*) 
			(cons (1+ dummy) (aref succ *moves*)))
		  (setq stones (aref succ picuphole)) ; stones in this pit
		  (empty succ picuphole)
		  (setq disthole picuphole)
		  (dotimes (dummy2 stones) ; drop in successive holes except
		  			   ; opponents kalah hole
		  	   (setq disthole (nextdistholefn disthole whoseturn))
			   (dropin succ disthole 1))
		  (setq lasthole disthole)
		  (cond ((allownzerok succ whoseturn) ; all played out
		  	 (opptakesallfn succ whoseturn))
			((eq lasthole (kalaholefn whoseturn)) ; last in kalah
			 (setq succ (successorsfn succ whoseturn)))
			((and (eq (aref succ lasthole) 1) ; last into own empty
			      (> (aref succ (opposholefn lasthole)) 0)
			      (ownsidep lasthole whoseturn))
			 (dropin succ 
			 	 (kalaholefn whoseturn)
				 (1+ (aref succ (opposholefn lasthole))))
			 (empty succ lasthole)
			 (empty succ (opposholefn lasthole))
			 (when (allownzerok succ whoseturn)
			       (opptakesallfn succ whoseturn))))
		  (setq succlist (nconc (preparelisfn succ) succlist))))
    (if (null succlist)
    	(progn	(setq succ (copy position))
		(opptakesallfn succ whoseturn)
		(list succ))
	succlist))

(defun dropin (position hole number) 
	(setf (aref position hole) (+ number (aref position hole))))

(defun nextdistholefn (disthole whoseturn)
	(cond	((and whoseturn (eql disthole *lasta*)) *firstb*) ; skip own pile
		((and (not whoseturn) (eql disthole *lastb*)) *firsta*)
		((< disthole *endb*) (1+ disthole))
		(t *firsta*)))

(defun preparelisfn (x)
	(if (arrayp x)
	    (list x)
	    (unimbedfn x)))

(defun reorder (poslist whoseturn)
       (mapcar #'car (sort (mapcar #'(lambda (x) 
					     (cons x
						   (evaluate x whoseturn)))
				   poslist)
			   #'(lambda (x y) (>= (cdr x) (cdr y))))))
				
(defun evaluate (position whoseturn) ; assign the value to the position
				     ; could obviously use work
	(let ((ownkala (aref position (kalaholefn whoseturn)))
	      (oppkala (aref position (kalaholefn (not whoseturn)))))
	     (cond ((> ownkala *halfall*) *maxvalue*)
	     	   ((> oppkala *halfall*) *minvalue*)
		   (t (- ownkala oppkala)))))
		   
(defun printins ()
       (terpri)
       (format t "Select Hole:~%    ")
       (dotimes (i *enda*)
		(let ((n (- *enda* i)))
		     (prin1 n)
		     (prinb (if (> 10 n) 2 1))
		     )))

(defun listmoves (position &aux (l (aref position *moves*)))
       (setf (aref position *moves*) nil)
       (terpri)
       (format t "Moves: ")
       (map nil #'(lambda (x) (format t "~s " x)) (reverse l)))

(defun printpos (position)
	     (terpri)
	     (prin1 (aref position *enda*))
	     (prinb (if (> 10 (aref position *enda*)) 3 2))
	     (dotimes (n *enda*)
	     	 (let ((val (aref position (- *enda* n 1))))
		 	(prin1 val)
			(prinb (if (> 10 val) 2 1))))
	     (terpri)
	     (prinb 4)
	     (dotimes (n *enda*)
	         (let ((val (aref position (+ *firstb* n))))
		 	(prin1 val)
			(prinb (if (> 10 val) 2 1))))
	     (prinb 2)
	     (prin1 (aref position *endb*))
	     (terpri)
	     (terpri))


; check if player is out of pieces (then opponent will take remainder)
(defun countown (position whoseturn)
       (reduce #'+ position :start (if whoseturn *firstb* *firsta*)
			    :end   (if whoseturn *endb* *enda*)))

(defun opptakesallfn (position whoseturn)
	(dropin position
		(kalaholefn (not whoseturn))
		(countown position (not whoseturn)))
	(do	((count *enda* (1- count))
		 (hole (if whoseturn *firsta* *firstb*) (1+ hole)))
		((zerop count))
		(empty position hole)))

(defun nextmove (depth whoseturn)
       (terpri)
       (setq *board* (search (car *board*) depth whoseturn))
       (listmoves (car *board*))
       (printpos (car *board*))
       (print (cdr *board*))
       (terpri))

(defun unimbedfn (poslist)
	(do	((list poslist (cdr list))
		 (result nil (if (arrayp (car list))
		 		 (cons (car list) result)
				 (append (unimbedfn (car list)) result))))
	        ((null list) result)))

(defun initialize (holes pebbles 
			 &aux (temp (makefield (+ (* 2 holes) 3) pebbles)))
       ; initialize the playing area
	(setf *enda* holes
	      *endb* (+ holes holes 1)
	      *lasta* (1- *enda* )
	      *firstb* (1+ *enda*)
	      *lastb* (1- *endb*)
	      *halfall* (* holes pebbles)
	      *moves* (1+ *endb*)
	      (aref temp *enda*) 0
	      (aref temp *endb*) 0
	      (aref temp *moves*) nil
	      *board* (list temp 0)))

(defun play (holes pebbles depth) ; play the game
	(initialize holes pebbles)
	(do ((whoseturn nil (not whoseturn))
	     (scorea 0 (aref (car *board*) *enda*))
	     (scoreb 0 (aref (car *board*) *endb*)))
	    ((or (> scorea *halfall*)
		 (> scoreb *halfall*)
		 (= scorea scoreb *halfall*)))
	    (nextmove depth whoseturn)))

(defun meplay (holes pebbles depth computer-first) 
  (prog (picuphole)
	(initialize holes pebbles)
	(when computer-first (setq succ (copy (car *board*)))
	 	 (go y))
n	(setq succ (copy (car *board*)))
	(printins)
	(printpos succ)
	(when (> (aref succ *enda*) *halfall*) 
	      (format t "You win!!~%")
	      (return t))	; win for side a
	(when (> (aref succ *endb*) *halfall*) 
	      (format t "I win!!!~%")
	      (return nil)) ; win for side b
	(when (= (aref succ *enda*) (aref succ *endb*) *halfall*)
	      (format t "We tie???~%")
	      (return nil))
x	(princ "Hole? ") (setq picuphole (read))
	(if (or (not (numberp picuphole))
		(>= picuphole *firstb*)
		(> 1 picuphole)
		(zerop (setq stones 
			     (aref succ (setq picuphole (1- picuphole))))))
	    (go x))
	(empty succ picuphole)
	(setq disthole picuphole)
	(dotimes (dummy stones)
	         (dropin succ 
			 (setq disthole (nextdistholefn disthole nil)) 1))
	(setq lasthole disthole)
	(cond	((allownzerok succ nil)
		 (opptakesallfn succ nil)
		 (setq *board* (list succ 0))
		 (go n))
		((eql lasthole *enda*)
		 (setq *board* (list succ 0))
		 (go n))
		((and (eql (nth lasthole succ) 1)
		      (> (aref succ (opposholefn lasthole)) 0)
		      (> *enda* lasthole))
		 (dropin succ *enda* (1+ (aref succ(opposholefn lasthole))))
		 (empty succ lasthole)
		 (empty succ (opposholefn lasthole))
		 (when (allownzerok succ nil)
		       (opptakesallfn succ nil)
		       (setq *board* (list succ 0))
		       (go n))))
	(printpos succ)
y	(format t "(Computer is thinking...)~%")
	(setq *board* (search succ depth t))
	(listmoves (car *board*))
	(go n)))

