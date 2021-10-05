;;;; CEREBRUM
;;;
;;; A framework for the genetic programming of neural networks
;;;
;;; By Peter Dudey
;;;
;;; Based on the work of John Koza
;;;
;;; Requires the file "utils.lsp" and an example file.  To see the form of the
;;; example file, look at "examples.xor".
;;;
;;; The command to run the main function is:  (evolve)
;;;


(setq *readtable-case* :upcase)
(load "utils")                          ; Some utilities
(load "examples.aeq1")                   ; The example file

(setq *maximum-depth*                   ; Maximum depth of initial critters initially 5
      15
      *maximum-offspring-depth*         ; Used by "graft" to prevent gigantic
      (expt *maximum-depth* 2)          ;  offspring
      *start-from-scratch*              ; If t, generates initial population
      t                                 ;  If nil, uses population on disk
      *population-size*                 ; Number of critters per generation  initially 100
      200
      *number-to-cross-breed*           ; Number of critters produced by
      70                               ;  breeding, rather than copying  initially 35
      *number-of-generations*       
      50
      *raw-fitness-weight*              ; Multiplier for raw fitness score
      20
      *parsimony-weight*                ; Each branch in a critter reduces
      .2                                ;  fitness by this much (initially .2)
      *maximum-fitness*                 ; The highest conceivable fitness
      (* *maximum-raw-fitness* *raw-fitness-weight*)
      *graph-statistics*                ; If t, produces spiffy graphs
      t
      *print-statistics*                ; If t, prints stats to screen
      t
      *save-statistics*                 ; Nil, or file to save stats to
      "stats.cer"
      critters                        ; array of critters
      (make-array *population-size*)
      critters_new                    ; array of new critters
      (make-array *population-size*)
      )                     


(defun create-axon (&optional (depth 0))
;;
;; Creates a random axon having 1 to 5 dendrite inputs, produced by the
;; "create-dendrite" function.
;;
 (let ((dendrites nil))
  (dotimes (i (1+ (random 5)))
   (setq dendrites (cons (create-dendrite (1+ depth)) dendrites))) 
 (cons 'axon dendrites)))


(defun create-dendrite (depth)
;;
;; Creates a random dendrite having two inputs.  The first input is a weight,
;; produced by "create-weight", and the second is either a neuron produced by
;; "create-axon" or a detector from the global list *detectors*.  If depth
;; is greater than or equal to the global *maximum-depth*, then the second
;; input will always be a detector.
;;
 (list 'dendrite
       (create-weight (1+ depth))
       (if (or (>= depth *maximum-depth*) (= 0 (random 2)))
           (nth (random (length *detectors*)) *detectors*)
           (create-axon (1+ depth)))))


; (defun create-weight (depth)
; ;;
; ;; Creates a random weight.  If depth is greater than or equal to the global
; ;; *maximum-depth*, this will merely be a random number between -2 and 2.
; ;; Otherwise, there is a 50% chance that it will be an arithmetic function of
; ;; two other weights (instead of a random number).
; ;;
;  (if (or (>= depth *maximum-depth*) (= 0 (random 2)))
;      (- (float (/ (random 40001) 10000)) 2)
;      (case (random 4)
;       (0 (list '+ (create-weight (1+ depth)) (create-weight (1+ depth))))
;       (1 (list '- (create-weight (1+ depth)) (create-weight (1+ depth))))
;       (2 (list '* (create-weight (1+ depth)) (create-weight (1+ depth))))
;       (3 (list '% (create-weight (1+ depth)) (create-weight (1+ depth)))))))

(defun create-weight (depth)
;;
;; Creates a random weight.  If depth is greater than or equal to the global
;; *maximum-depth*, this will merely be a random number between -2 and 2.
;; Otherwise, there is a 50% chance that it will be an arithmetic function of
;; two other weights (instead of a random number).
;;
       ;; I fail to see why this needs to be an expression -- it's all constants so evaluate it here
 (if (or (>= depth *maximum-depth*) (= 0 (random 2)))
     (- (float (/ (random 40001) 10000)) 2)
     (case (random 4)
      (0 (+ (create-weight (1+ depth)) (create-weight (1+ depth))))
      (1 (- (create-weight (1+ depth)) (create-weight (1+ depth))))
      (2 (* (create-weight (1+ depth)) (create-weight (1+ depth))))
      (3 (% (create-weight (1+ depth)) (create-weight (1+ depth)))))))


(defun % (x y)
;;
;; Safe division.  Returns 0 if y is 0, otherwise floats the result of a
;; normal division.
;;
 (if (= y 0)
     0
     (float (/ x y))))


(defun axon (&rest dendrites)
;;
;; The neural summation function.  Returns 1 if the sum of the dendrites is
;; greater than or equal to 1, otherwise returns 0.
;;
;; If you want to have neurons that can return more than two values, override
;; this.
;;
 (if (>= (apply '+ dendrites) 1)
     1
     0))


(defun dendrite (x y)
;;
;; Multiplication.  This is defined separately so that the dendrites can
;; easily be distinguished from the regular multiplications (in weights).
;;
 (* x y))


(defun create-initial-population ()
       ;;
       ;; Creates an initial population of critters, and saves them in the directory
       ;; *critter-directory*.
       ;;
       (setq critters (make-array *population-size*)
             critters_new (make-array *population-size*))
       (dotimes (i *population-size*)
                (setf (aref critters i)
                      (create-axon))))


(defun branch-type (tree)
;;
;; Returns either 'axon, 'dendrite, 'weight, or nil.  Detectors are considered
;; to be of type axon.
;;
 (if (atom tree)
     ;; If it's an atom, check to see if it's a detector or a number.
     (cond
      ((member tree *detectors* :test 'equal) 'axon)
      ((numberp tree) 'weight)
      (t nil))
     ;; If it's a list, determine the type by looking at its car
     (case (car tree)
      ('axon 'axon)
      ('dendrite 'dendrite)
      ((+ - * %) 'weight)
      (t nil))))


(defun count-branchings (critter
                         &optional
                         (allowed-types '(axon dendrite weight)))
;;
;; Returns the number of possible branching/grafting points in the critter
;; which are of an appropriate type.  Detectors are considered to be of
;; type axon.
;;
 (if (atom critter)
     ;; If it's an atom of the appropriate type, return 1.  Return 0 for
     ;; any other atom.
     (if (member (branch-type critter) allowed-types)
         1
         0)
     ;; If it's a list, count the number of appropriately-typed sub-trees.
     ;; If the whole thing is of the appropriate type, add 1.
     (let ((tally
            (if (member (branch-type critter) allowed-types)
                1
                0)))
      (dolist (sub-tree critter tally)
       (setq tally (+ tally (count-branchings sub-tree allowed-types)))))))


(defun nth-branch (n critter)
;;
;; Returns branch number n (0-based) of critter.  Branch number 0 would be
;; the whole thing, number 1 is the cadr, number 2 is the cadadr...
;;
 (if (= 0 n)
     ;; If n is 0, return the whole thing.
     critter
     ;; If n is not 0, go along the cdr.  (The car, being the function name,
     ;; is not a valid sub-tree.)  If a sub-tree has n or more branches,
     ;; apply "nth-branch" recursively.  If not, decrement n and continue.
     (dolist (sub-tree (cdr critter))
      (if (>= (count-branchings sub-tree) n)
          (return (nth-branch (1- n) sub-tree))
          (setq n (- n (count-branchings sub-tree)))))))


(defun graft (limb limb-type place recipient &optional (depth 0))
;;
;; Returns a copy of recipient, with the appropriate limb replaced by the
;; given one.  Destructive to recipient.
;;
 (cond 
  ;; If depth squared, plus the number of branches in limb, is greater than
  ;; *maximum-offspring-depth*, return recipient unchanged
  ((> (+ (expt depth 2) (count-branchings limb)) *maximum-offspring-depth*)
   recipient)
  ;; If the limb and recipient are the same type and n is 0, return just the
  ;; limb
  ((and (equal (branch-type recipient) limb-type) (= place 0))
   limb)
  (t
   ;; If the limb is the same type as the recipient, decrement n to account
   ;; for the branch that has not been replaced:  the whole thing
   (if (equal (branch-type recipient) limb-type)
       (setq place (1- place)))
   ;; Go along the cdr.  (The car, being the function name, is not a valid
   ;; sub-tree.)  If a sub-tree has n or more appropriate branches, apply
   ;; "graft" recursively.  If not, decrement place and continue.
   (do* ((i 1 (1+ i))
         (sub-tree (nth 1 recipient) (nth i recipient))
         (sub-tree-branchings (count-branchings sub-tree (list limb-type))
                              (count-branchings sub-tree (list limb-type))))
    ((< place 0) recipient)
    (if (>= sub-tree-branchings (1+ place))
        (setf (nth i recipient)
              (graft limb limb-type place sub-tree (1+ depth))))
    (setq place (- place sub-tree-branchings))))))


(defun cross (critter1 critter2 &aux result)
       ;;
       ;; Grafts a piece of critter1 onto critter2, and returns the result.
       ;; Destructive to critter2.
       ;;
       ;; Keep picking random pieces out of critter1 until you get one of a type
       ;; that critter2 has.  (This prevents trying to transplant dendrites into
                                    ;; critters which are simple detectors.)
;        (format *logfile* "critter 1:")
;        (pp critter1 *logfile*)
;        (format *logfile* "critter 2:")
;        (pp critter2 *logfile*)

       (let* ((transplant
               (do ((try
                     (nth-branch (random (count-branchings critter1)) critter1)
                     (nth-branch (random (count-branchings critter1)) critter1)))
                   ((> (count-branchings critter2 (list (branch-type try))) 0) try)))
              (insertion-point 
                               (random (count-branchings critter2
                                                         (list (branch-type transplant))))))
             (setf result (graft transplant
                                 (branch-type transplant)
                                 insertion-point
                                 critter2))
;;             (format *logfile* "result: ")
;;             (pp result *logfile*)
             result))

              
(defun stochastic-critter ()
;;
;; Picks a critter out of the directory *critter-directory*.  Critters which
;; correspond to larger values in the global list *fitness-list* are
;; proportionately more likely to be chosen.
;;
;; The global variable *total-fitness* must be the sum of *fitness-list*.
;;
 ;; Pick a number from 0 to total-fitness
 (let* ((dart (if (zerop *total-fitness*) 0 (random *total-fitness*)))
        ;; Go along *fitness-list*, decrementing dart by each fitness as you
        ;; go along, until you have less than zero left, i.e., the last
        ;; "slice" of the "roulette wheel" passed the mark. Set n to the
        ;; number of that slice.
        (n (do* ((i 0 (1+ i))
                 (left (- dart (car *fitness-list*))
                       (- left (nth i *fitness-list*))))
            ((< left 0) i)))
        )
       (aref critters n)))
;;        (result nil))
  ;; Go find critter number n in the directory *critter-directory*
;;  (setq result (copy-tree (aref critters n)))   ;; this is poorly written -- we don't need symbol "result"
;;  result))


(defun save-run-info ()
;;
;; Saves parameters into the file *save-statistics*, and erases any
;; previous stats file at that location.
;;
 (with-open-file (f *save-statistics* :direction :output
                                      :if-exists :supersede
                                      :if-does-not-exist :create)
  (format f "CEREBRUM~2%by Peter Dudey~2%")
  (if *start-from-scratch*
      (format f "Maximum Initial Depth: ~a~%" *maximum-depth*)
      (format f "(Building on previous population)~%"))
  (format f "Population Size: ~a~%" *population-size*)
  (format f "Number Produced by Breeding: ~a~%" *number-to-cross-breed*)
  (format f "Number of Generations: ~a~%" *number-of-generations*)
  (format f "Raw Fitness Weight: ~a~%" *raw-fitness-weight*)
  (format f "Parsimony Weight: ~a~%" *parsimony-weight*)
  (format f "Maximum Conceivable Fitness: ~a~%" *maximum-fitness*)
  (format f "Critters Stored In: ~a~%" "critters")))


(defun fitness (critter)
;;
;; Determines *maximum-raw-fitness*, minus the sum of squares of differences
;; between the "right" answers and the critter's answers to each of the
;; examples in the global list *examples*.  (For normal 1-or-0 problems, this
;; is *maximum-raw-fitness* minus the number of misses.)
;;
;; This raw fitness is then multiplied by *raw-fitness-weight*, and adjusted
;; for parsimony.  (Each branching point in a critter reduces its fitness by
;; *parsimony-weight*.)
;;
;; The greater of 0 and this adjusted fitness (rounded up) is then returned.
;;
;; *examples* should be a list of lists.  The car of each sub-list is the
;; right answer, and the cdr is the list of detector inputs, parallel to
;; the global list *detectors*.
;;
;; If for some reason (e.g., competition between neural nets) you override
;; this function, make sure that it returns only non-negative integers.
;;
 (let ((tally *maximum-raw-fitness*))
  ;; Determine basic fitness
  (dolist (example *examples*)
   ;; Set the detector inputs
   (do ((i 0 (1+ i)))
    ((= i (length *detectors*)))
    (set (nth i *detectors*) (nth (1+ i) example)))
   ;; Decrease tally by the amount of error (difference squared)
   (setq tally (- tally (expt (- (car example) (if (>= (eval critter) 1) 1 0)) 2))))
  ;; Multiply by raw fitness weight
  (setq tally (* tally *raw-fitness-weight*))
  ;; Adjust for parsimony
  (setq tally (- tally (* *parsimony-weight* (count-branchings critter))))
  ;; Return 0 if tally < 0, otherwise tally
  (if (< tally 0)
      0
      (ceiling tally))))


(defun fitness-list ()
;;
;; Returns a list of the fitnesses of the critters in the directory 
;; *critter-directory*.
;;
 (let ((result nil))
  (dotimes (i *population-size*)
	   (setq result
		 (append result
			 (list (fitness (aref critters i))))))

  result))

#|
(defun count (item sequence)
;;
;; WARNING!  THIS IS NOT THE STANDARD COUNT FUNCTION!
;;
 (let ((result 0))
  (dolist (s sequence)
   (if (equal item s)  ; The standard count function uses "eq"
       (setq result (1+ result))))
  result))
|#

(defun fitness-count ()
;;
;; Produces a list for the histogram.  The number of items in the list is
;; *maximum-fitness* plus 1 (to account for zero-fitness critters).  Each
;; item is the number of critters with that fitness.
;;
 (let ((result nil))
  (dotimes (i (1+ *maximum-fitness*))
   (setq result (append result (list (count i *fitness-list* :test #'equal)))))
  result))

#|
(defun greatest (numbers)
;;
;; Returns the largest number in the list.
;;
 (let ((best (car numbers)))
  (dolist (n numbers)
   (if (> n best)
       (setq best n)))
  best))
|#

(defun compute-statistics ()
;;
;; Defines variables used in "generation" and "report-statistics".
;;
 (setq *fitness-list*
       (fitness-list)
       *total-fitness*
       (apply '+ *fitness-list*)
       *best-history*
       (append *best-history* (list (apply #'max *fitness-list*)))
       *average-history*
       (append *average-history*
               (list (% *total-fitness* *population-size*))))
 nil)  ; So we don't have to pass up that huge *average-history* list
                                     

#|
(defun edit (critter)
;;
;; Replaces all arithmetic weight constructions with constants.  This is
;; used when the final, best individual is printed out, to make it easier
;; to read.
;;
 (if (atom critter)
     critter
     (if (member (car critter) '(axon dendrite))
         (cons (car critter) (mapcar 'edit (cdr critter)))
         (eval critter))))
|#

(defun print-statistics (generation stream)
;;
;; Prints the generation number, best individual number, best fitness, and
;; average fitness to stream.
;;
;; If this is the last generation, it is noted if stream is t (the screen).
;; If stream is non-t (a file), the best critter is tacked on to the file.
;;
 (let* ((best-fitness (nth generation *best-history*))
        (best-number (position best-fitness *fitness-list*)))
  (format stream "~2%Generation Number ~a:" generation)
  (if (and (= generation *number-of-generations*) (equal t stream))
      (format t "  (LAST GENERATION--HIT ANY LETTER AND RETURN TO EXIT)~%")
      (format stream "~%"))
  (format stream "Best Individual: Number ~a~%" best-number)
  (format stream "Best Fitness: ~a (of ~a)~%" best-fitness *maximum-fitness*)
  (format stream "Average Fitness: ~a~%" (nth generation *average-history*))
  (cond 
   ((and (= generation *number-of-generations*) (not (equal t stream)))
    (format stream "~%Here's the best individual: ~2%")
;;    (pp (edit (aref critters best-number))
    (pp (aref critters best-number)
	stream)))))

(defun report-statistics (generation)
;;
;; The user interface thang.  Puts all sorts of nifty information on the
;; screen, and (if applicable) saves the text part of it to the file *save-
;; statistics*.
;;
;; If this is the last generation and *save-statistics* is non-nil, the
;; best critter is tacked on to the end of that file.
;;
 (if *save-statistics*
     (with-open-file (stats *save-statistics* :direction :output
                                              :if-exists :append
                                              :if-does-not-exist :create)
      (print-statistics generation stats)))
 (if *graph-statistics*
     (turtlegraphicsup))
 (when  *print-statistics*
     (goto-xy 0 0)
     (print-statistics generation t))
 (cond
  (*graph-statistics*
   (format t "~% Population vs Fitness                              ~
              Fitness vs Frequency")
   (format t "~17% Time vs Fitness  (blue = best, yellow = average)")
   (draw-box 410 555 0 290)
   (draw-box 410 555 310 600)
   (draw-box 180 380 0 600)
   (graph (1- *population-size*) *maximum-fitness* *fitness-list*
          411 554 1 289 green)
   (graph *maximum-fitness* *population-size* (fitness-count)
          411 554 311 599 red)
   (graph generation *maximum-fitness* *average-history*
          181 379 1 599 yellow)
   (graph generation *maximum-fitness* *best-history*
          181 379 1 599 ltblue))))


(defun generation ()
;;
;; Replaces the ".old" files in the directory *critter-directory* with the
;; next generation.  Makes use of temporary ".new" files.
;;
       ;; Note that this is crappy because elements can be selected multiple times.
 ;; First, make some babies
 (dotimes (i *number-to-cross-breed*)
  (setf (aref critters_new i)
       (cross (stochastic-critter) (copy-tree (stochastic-critter)))))
 ;; Then, copy some survivors
 (do ((i *number-to-cross-breed* (1+ i)))
  ((= i *population-size*))
  (setf (aref critters_new i)  
       (stochastic-critter)))
 ;; Copy all of the ".new" files into the ".old" ones
 (dotimes (i *population-size*)
  (setf (aref critters i)
       (copy-tree (aref critters_new i)))))

(defun evolve ()
;;
;; The main function.  Creates an initial population, and makes repeated
;; calls to "compute statistics", "report-statistics", and "generation".
;;
;;
 ;; Set up initial history lists
;;       (setq *logfile* (open "log.txt" :direction :output))
 (setq *best-history* nil)
 (setq *average-history* nil)
 (setq *maximum-fitness*                 ; The highest conceivable fitness
       (* *maximum-raw-fitness* *raw-fitness-weight*))
 (setq      *maximum-offspring-depth*         ; Used by "graft" to prevent gigantic
      (expt *maximum-depth* 2))          ;  offspring
 ;; If statistics are being saved, save parameters
 (if *save-statistics*
     (save-run-info))
 ;; If the *start-from-scratch* is non-nil, create a new population
 (if (or *start-from-scratch* (null (eql (length critters) *population-size*))) 
     (create-initial-population))
 ;; Do the generations
 (dotimes (i *number-of-generations*)
  (compute-statistics)
  (report-statistics i)
  (generation))
 ;; Produce statistics for the last generation
 (compute-statistics)
 (report-statistics *number-of-generations*)
       (gmode nil)
;;       (close *logfile*)
)


(setq *random-state* (make-random-state t))  ; Seed random number generator


;; If you need to load any override files (e.g., an axon function that has
;; three possible outputs), do it here.

(set-stack-mark 6000)
(expand 5)
