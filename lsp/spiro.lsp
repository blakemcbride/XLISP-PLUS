;; Spirographic Program

(defun p2r (d a)
"P2R <length> <angle>      CONVERT FROM POLAR TO RECTANGULAR
     <length>	length
     <angle>    angle in radians
     returns	list of x and y coordinates"
       (list (* d (cos a))
	     (* d (sin a))))

(defun r2p (x y)
"R2P <x> <y>               CONVERT FROM RECTANGULAR TO POLAR
     <x>        x coordinate
     <y>	y coodinate
     returns	list of length and angle in radians"
       (list (sqrt (* x x) (* y y))
	     (atan y x)))

(defun v+ (v1 v2) "adds two vectors"
       (list (+ (first v1) (first v2)) (+ (second v1) (second v2))))


(defun calcpos (radius1 theta1 radius2 theta2)
       "adds two polar coodinate vectors and returns rectangular result"
       (v+ (p2r radius1 theta1) (p2r radius2 theta2)))

(defvar *delta* (/ pi 18.0) "step size")
(defvar *gx* 300 "x coordinate of center of spiral")
(defvar *gy* 300 "y coordinate of center of spiral")

(defun spiro (sradius lradius drawradius steeth lteeth &optional (initheta 0.0))
"SPIRO <sradius> <lradius> <drawradius> <steeth> <lteeth> [<initheta>]
       Imagine a small gear turning inside a large stationary gear with a pen
       attached to the small gear which draws on paper below.
       <sradius>        Radius of small gear
       <lradius>        Internal radius of large gear
       <drawradius>     Position of pen from center or small gear, this can actually
                        be larger than the gear size!
       <steeth>         Normally steeth and lteeth are the number of teath in the
       <lteeth>         gears, and would be proportional to the radius of the gears
                        but since this is all imaginary, the ratio of sizes doesn't
                        have to match!
       <initheta>       initial angle of pen on gear. Normally gears are meeting at
                        0 degrees and the pen at 0 degrees.
"
       (let ((pos (calcpos (- lradius sradius) 0.0 drawradius initheta)))
	    (move (round (+ *gx* (first pos)))
		  (round (+ *gy* (second pos)))))
       (do* ((turns (/ (lcm lteeth steeth) lteeth)) ;; Turns of outer gear
	     (stheta initheta (+ stheta (* *delta* (- 1 (/ steeth lteeth)))))
	     (ltheta 0.0 (- ltheta (* *delta* (/ steeth lteeth))))
	     (pos (calcpos (- lradius sradius) ltheta drawradius stheta)
		  (calcpos (- lradius sradius) ltheta drawradius stheta)))
	    ((< ltheta (* -2 pi turns)) (gmode nil))
	    (draw (round (+ *gx* (first pos)))
		  (round (+ *gy* (second pos))))
	    ))

(defun ospiro (sradius lradius drawradius steeth lteeth &optional (initheta 0.0)) ;; outside version
"OSPIRO <sradius> <lradius> <drawradius> <steeth> <lteeth> [<initheta>]
       Imagine a small gear turning outside a large stationary gear with a pen
       attached to the small gear which draws on paper below.
       <sradius>        Radius of small gear
       <lradius>        Internal radius of large gear
       <drawradius>     Position of pen from center or small gear, this can actually
                        be larger than the gear size!
       <steeth>         Normally steeth and lteeth are the number of teath in the
       <lteeth>         gears, and would be proportional to the radius of the gears
                        but since this is all imaginary, the ratio of sizes doesn't
                        have to match!
       <initheta>       initial angle of pen on gear. Normally gears are meeting at
                        0 degrees and the pen at 0 degrees.
"
       (let ((pos (calcpos (+ lradius sradius) 0.0 drawradius initheta)))
	    (move (round (+ *gx* (first pos)))
		  (round (+ *gy* (second pos)))))
       (do* ((turns (/ (lcm lteeth steeth) lteeth)) ;; Turns of outer gear
	     (stheta initheta (+ stheta (* *delta* (+ 1 (/ steeth lteeth)))))
	     (ltheta 0.0 (+ ltheta (* *delta* (/ steeth lteeth))))
	     (pos (calcpos (+ lradius sradius) ltheta drawradius stheta)
		  (calcpos (+ lradius sradius) ltheta drawradius stheta)))
	    ((> ltheta (+ *delta* (* 2 pi turns))) (gmode nil))
	    (draw (round (+ *gx* (first pos)))
		  (round (+ *gy* (second pos))))
	    ))

(progn (princ "Functions to run are SPIRO and OSPIRO, which are described with the GLOS function") nil)
