; Blocks World from Winston&Horn
; modified for XLISP and graphics by Tom Almy


#-:classes (load "classes")
#-:qt(error "requires Qt graphics -- load gblocks.lsp instead") 
(require "qt")

#+:packages
(unless (find-package "BLOCKS")
        (make-package "BLOCKS" :use '("XLISP")))

(in-package "BLOCKS")
(use-package "GRAPHIC")
(export '(black white m d))
(export '(b1 b2 b3 b4 w5 b6 w7 l8 b9 table))


;
; Functions for graphic assistance

(defvar *bx* 0)		; text communication region
(defvar *by* 21)
(defvar *gx* 50)	; Graphic region origin
(defvar *gy* 100)
(defvar *ymax* 349)	; height of display
(defvar *step-size* 10)	; lcd of block widths 
(defvar *delay-time* 0.3)	; delay time in seconds
(defvar *bkg* black) ; background color
(defvar *undraw* black)            ; undrawing color
(defvar *graspcolor* white) ; (grasping color)
(defvar *ungraspcolor* grey)

(defun setgpos (x y) (move (round (+ *gx* x)) (round (+ *gy* y))))


; Go back to the text window
(defun bottom ()
    (mode 2)
    nil)

; Go to graphics mode


(defun gmodeVGA (&aux dims) ; standard 640x480 VGA
			    ; Modified so it will work in Windows as well
       (setq dims (gmode 640 480))
       (setq *ymax* (1+ (fourth dims)))
       (setq *by* 9)
       (setq *gy* (truncate (* 2.5 *charheight*)))
       (display-blocks))

; abstract classes for ball types

; basic blocks support nothing
(defclass basic-block (name color width height position supported-by (fill graphic:nofill)))

(defmethod basic-block :support-for () nil)

(defmethod basic-block :top-location  () 
	(list (+ (first position) (/ width 2))
	      (+ (second position) height)))

(defmethod basic-block :drawname ()
        (colors color) ; text color same as block color
	(setgpos (+ (first position) 
		    (/ (- width (* *charwidth* (flatc name))) 2))
	         (+ (second position) (/ height 2) (- (/ *charheight* 2))))
	(princ name))

(defmethod basic-block :undrawname ()
	   (colors *undraw*) ; text color same as block color
	   (brush *undraw* graphic:solid)
	   (drawrect (+ *gx* (first position) 1)
                     (+ *gy* (second position) 1)
                     (- width 2)
                     (- height 2)))


(defmethod basic-block :draw (undraw)
           (brush (first (colors (if undraw *undraw* color))) fill)
           (drawrect (+ *gx* (first position))
                     (+ *gy* (second position))
                     (1- width)
                     (1- height)))
                        

; movable-blocks can be moved
(defclass movable-block () () basic-block)

(defmethod movable-block :new-position (newpos)
	(send self :draw t)
	(send self :undrawname)
	(setf position newpos)
	(send self :draw nil)
      	(send self :drawname))

; load-bearing blocks can support other blocks, and can be moved
(defclass load-bearing-block (support-for) () movable-block)

; we can't have multiple inheritance, so we need a separate class for table
; table blocks can support other blocks but cannot be moved.

(defclass table-block (support-for) () basic-block)

; Specific classes for table brick wedge and ball

(defclass brick () () load-bearing-block)

(defclass wedge () () movable-block)

(defmethod wedge :draw (undraw)
	(colors (if undraw *undraw* color))
	(move (+ *gx* (first position)) (+ *gy* (second position)))
	(drawrel (1- width) 0 
		 (- 1 (/ width 2)) (1- height )
		 (- (/ width 2) width 1) (- 1 height)))

(defclass ball  () () movable-block)

(defmethod ball :draw (undraw)
	(brush (first (colors (if undraw *undraw* color))) fill)
        (drawellipse (+ (first position) *gx*)
                     (+ (second position) *gy* 1)
                     (- (min width height) 3)))

(defclass hand  (name position grasping))

(defmethod hand :top-location  () position)

(defmethod hand :draw (undraw)
	(colors (if undraw *undraw* (if grasping *graspcolor* *ungraspcolor*)))
	(move (+ *gx* -7 (first position)) (+ *gy* (second position)))
	(drawrel 5 0 0 10 5 0 0 -10 5 0 0 20 -15 0 0 -20))

(defmethod hand :new-position (newpos)
	(send self :draw t)
	(setf position newpos)
	(send self :draw nil))

; define all the individual blocks

(setf *blocks*
      (list
        (send table-block :new :name 'table :width 430 :height 20 
			       :position '(0 0) :color ltgrey :fill graphic:dense7)
	(send brick :new :name 'b1 :width 40 :height 40 
			       :position '(0 20) :color '(50 50 170)) ;; An easier to see blue
	(send brick :new :name 'b2 :width 40 :height 40 
			       :position '(40 20) :color green)
	(send brick :new :name 'b3 :width 80 :height 80 
			       :position '(80 20) :color cyan :fill graphic:diagcross)
	(send brick :new :name 'b4 :width 40 :height 40 
			       :position '(160 20) :color red)
	(send wedge :new :name 'w5 :width 40 :height 80 
			       :position '(200 20) :color magenta)
	(send brick :new :name 'b6 :width 80 :height 40 
			       :position '(240 20) :color brown)
	(send wedge :new :name 'w7 :width 40 :height 40 
			       :position '(320 20) :color ltblue)
	(send ball  :new :name 'l8 :width 40 :height 40 
			       :position '(360 20) :color ltcyan :fill graphic:dense6)
	(send brick :new :name 'b9 :width 30 :height 30 
			       :position '(400 20) :color ltred)
       ))

(dolist (l *blocks*) (set (send l :name) l))

(dolist (l (rest *blocks*)) ; all blocks but the table
	(setf (send table :support-for) 
	      (cons l (send table :support-for))
	      (send l :supported-by)
	      table))

(definst hand *hand* :name '*hand* :position '(0 120))

(defun display-blocks ()
	(cls)
	(dolist (l *blocks*) (send l :drawname)(send l :draw nil))
	(send *hand* :draw nil)
	(bottom)
	t)

(defmethod basic-block :put-on (support) ; default case is bad
        (gmode nil) ;; force to text display
	(format t
		"Sorry, the ~a cannot be moved.~%"
		name))

(defmethod movable-block :put-on (support)
	(if (send self :get-space support)
	    (and (send *hand* :grasp self)
	    	 (send *hand* :move  self support)
		 (send *hand* :ungrasp self))
            (progn
             (gmode nil) ;; force text display
             (format t
                     "Sorry, there is no room for ~a on ~a.~%"
                     name
                     (send support :name)))))

(defmethod movable-block :get-space (support)
	(or (send self :find-space support)
	    (send self :make-space support)))

(defmethod hand :grasp (obj)
	(unless (eq grasping obj)
		(when (send obj :support-for)
		      (send obj :clear-top))
		(when grasping
		      (send grasping :rid-of))
		(let ((lift (max-height self obj)))
		     (send self :new-position lift)
		     (pause *delay-time*)
		     (send self :new-position 
		     	(list (first (send obj :top-location)) (second lift)))
		     (pause *delay-time*)
		     (send self :new-position (send obj :top-location))
		     (pause *delay-time*))
		(send self :draw t)
		(setf grasping obj)
		(send self :draw nil))
	t)

(defmethod hand :ungrasp (obj)
	(when (send obj :supported-by)
	      (send self :draw t)
	      (setf grasping nil)
	      (send self :draw nil)
	      t))


(defmethod movable-block :rid-of ()
	(send self :put-on table))

(defmethod movable-block :make-space (support)
	(dolist (obstruction (send support :support-for))
		(send obstruction :rid-of)
		(let ((space (send self :find-space support)))
		     (when space (return space)))))

(defmethod  load-bearing-block :clear-top ()
	(dolist (obstacle support-for) (send obstacle :rid-of))
	t)


(defmethod hand :move (obj support)
	(send obj :remove-support)
	(let ((newplace (send obj :get-space support)))
          (let ((lift (max-height obj support)))
	     (send obj :new-position lift)
	     (send self :new-position (send obj :top-location))
	     (pause *delay-time*)
	     (send obj :new-position (list (first newplace) (second lift)))
     	     (send self :new-position (send obj :top-location))
	     (pause *delay-time*)
	     (send obj :new-position newplace)
	     (send self :new-position (send obj :top-location))
	     (pause *delay-time*)))
	(send support :add-support obj)
	t)


; helper function to find height necessary to move object

(defun max-height (obj1 obj2)
	(let    ((source (first (send obj1 :top-location)))
	         (dest   (first (send obj2 :top-location))))
	(let	((roof 0) (min (min source dest)) (max (max source dest)) )
		(dolist (obstacle *blocks*)
			(let ((x (send obstacle :top-location)))
			     (when (and (>= (first x) min)
			     		(<= (first x) max)
					(> (second x) roof))
				   (setf roof (second x)))))
		(list (first (send obj1 :position)) (+ 30 roof)))))
				   
#+:times (defun pause (time)
                (gmode t) ;; forces update
	   (let ((fintime (+ (* time internal-time-units-per-second)
			     (get-internal-run-time))))
		(loop (when (> (get-internal-run-time) fintime)
			    (return-from pause)))))
#-:times (defun pause () (dotimes (x (* time 1000))))


; remove-support-for is defined twice, for each load bearing class

(defmethod load-bearing-block :remove-support-for (obj)
	(setf support-for (remove obj support-for))
	t)

(defmethod table-block :remove-support-for (obj)
	(setf support-for (remove obj support-for))
	t)

(defmethod movable-block :remove-support ()
	(when supported-by
	      (send supported-by :remove-support-for self)
	      (setf supported-by nil))
	t)



(defmethod load-bearing-block :add-support (obj)
	(setf support-for 
	      (cons obj support-for)
	      (send obj :supported-by) 
	      self)
	t)

(defmethod table-block :add-support (obj)
	(setf support-for 
	      (cons obj support-for)
	      (send obj :supported-by) 
	      self)
	t)

(defmethod basic-block :add-support (obj)
	t)

(defmethod movable-block :find-space (support)
	(do     ((offset (- (send support :width) width)
	                 (- offset *step-size*)))
		((< offset 0))
		 (unless (intersections-p self offset
		 			  (first (send support :position))
					  (send support :support-for))
			 (return (list (+ offset (first (send support 
			 				      :position)))
				       (+ (second (send support :position))
				          (send support :height)))))))

(defun intersections-p (obj offset base obstacles)
	(dolist (obstacle obstacles)
		(let* ((ls-proposed (+ offset base))
			(rs-proposed (+ ls-proposed (send obj :width)))
			(ls-obstacle (first (send obstacle :position)))
			(rs-obstacle (+ ls-obstacle (send obstacle :width))))
		      (unless (or (>= ls-proposed rs-obstacle)
		      		  (<= rs-proposed ls-obstacle))
			      (return t)))))


(defun m (a b) (gmode t) (send a :put-on b) (bottom))
(defun d () (gmode t) (display-blocks))

(defun white ()
       (setf *bkg* white) ; try for white background
       (setf *undraw* white)            ; undraw color needs to be white as well
       (setf *graspcolor* black) ; black when grasping, was 15
       (colors black *bkg*)
       (d))

(defun black ()
       (setf *bkg* black)
       (setf *undraw* black)
       (setf *graspcolor* white)
       (colors white *bkg*)
       (d))

(gmodeVGA)
(d)

(princ "Commands: (m <block> <destination block) ;; move block\n")
(princ "          (d)  ;; redraws screen\n")
(princ "          (white) ;; sets white background\n")
(princ "          (black) ;; sets black background\n")

(in-package "USER")
(use-package "BLOCKS")
(provide "qtblocks")
