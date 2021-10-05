; Improved Graphics Support -- QT
; Tom Almy  March 2011

; XLISP's original graphics support dates back to MS-DOS. The Windows
; version was intended to compatible and used the same command set. With
; the change to Qt to have Linux and Mac OS X versions, I decided that a
; change was in order, but I attempted to maintain backwards
; compatibility for whatever that is worth, to the maximum extent
; reasonable.
; 
; In the Qt versions of XLISP, when graphics operations are performed
; (signified by using the MODE command to enter a graphics mode) a
; graphics window is displayed. XLISP attempts to keep the normal XLISP
; dialog in the original text window while placing all graphics in the
; graphics window. However there is an issue with where to place text
; printed by the application. To resolve this, there are two operating
; modes "text" and "graphics". In text mode text is sent to the text
; window while in graphics mode text is sent to the graphics window.
; While the goto-xy function was the sole method to position text (and
; still works in both modes, positioning in the correct window) text can
; also be positioned with the move and draw functions.
; 
; Text mode is entered with any of these actions:
; 1. Prompt for a new command.
; 2. Error or Break messages.
; 3. Executing MODE 0, 1, 2, or 3 commands.
; 
; Graphics mode is entered with any of these actions:
; 1. Executing MODE >3 command.
; 2. Executing any move or draw function.
; 
; All graphic output is buffered. The display is updated whenever text
; mode is entered or a MODE command is executed. Setting a mode with the
; same resolution as the current mode can be used to update the display.
; 
; The CLS function clears the text window in text mode and the graphics
; window in either text or graphics mode.
; 
; The CLEOL function only works in text mode. Text can only be cleared
; on the graphics display by drawing a filled rectangle of the
; background color over the area occupied by the text.
; 
; The standard graphics commands:
; 
; CLS -- clear the window
; CLEOL -- clear to end of line
; GOTO-XY -- in text mode sends the ANSI sequence to the text window. In
; graphics mode sets the x and y coordinate expecting a text write. Note
; that coordinate system starts with 1,1 in the upper left corner. The
; other graphics commands are pixel based and start with 0,0 in the
; lower left corner.
; MODE -- Sets mode. With one argument, sets VGA/EGA/CGA resolutions or
; text modes equivalent to the MSDOS version. However the color palate
; is 2^24 colors in all modes. With two or 4 arguments, the second
; argument is ignored. With 4 arguments, the third and fourth argument
; set the resolution (the size of the display window). The mode function
; returns the number of characters per line, the number of lines, the x
; size and the y size. From this information, the size of a character
; can be calculated. Unrecognized mode numbers will give a display size
; of 1024x768, and specifying 4 arguments will cause the first two to be
; ignored.
; COLOR -- The single argument version intends to match the existing 8
; bit MSDOS color selection. The four least significant bits select the
; foreground (drawing) color while the three least significant of the
; four most significant bits select the background color. Because the
; most significant bit was used for "blink" in character modes, the
; background brightness bit is bit 8 and bit 7 is used for exclusive or
; mode. In the Qt implementation, XOR is problematic so has been
; disabled. The three argument and 6 argument versions are carried over
; from the Windows version, with three selecting RGB foreground only and
; 6 selecting RGB foreground and background. Red foreground bit 8 would
; select exclusive OR if it worked properly.
; MOVE, MOVEREL, DRAW, DRAWREL -- carried over intact.
; 
; Qt adds additional built in functions FONT, BRUSH, DRAWRECT, and DRAWELLIPSE

; (font <size> <family> <style>)                         SELECT GRAPHICS FONT
;      Part of Qt graphics. Graphics must first be initialized with MODE or
;      GMODE function.
;      <size>    Size in points
;      <family>  1 - Helvetica, 2 - Times, 0 or other FIXNUM - Courier
;      <style>   0 - Normal, 1 - Bold, 2 - Italic, 3 - Bold and Italic
;      returns   T
; 
; (brush <color> <style>)                                  DEFINE FILL BRUSH
;      Part of Qt graphics. Graphics must first be initialized with MODE or
;      GMODE function.
;      <color>   Either a single fixnum color code or a list of three colors
;                Red, Green, and Blue.
;      <style>   0 - no fill, 1 - solid fill, 2-8 heavy through light shade, 
;                9 - horizontal lines, 10 - vertical lines, 11 - Cross lines
;                12 - / diagonal slashes, 13 - \ diagonal slashes, 14 -
;                diagonal cross pattern.
; 
; (drawrect <x1> <y1> <width> <height>)                    DRAW A RECTANGLE
;      Part of Qt graphics. Outline drawn in COLOR and filled in BRUSH.
;      <x1><y1>  Coordinates of one corner
;      <width>   width of rectangle
;      <height>  height of rectangle
;      returns   T
; 
; (drawellipse <x> <y> <width> [<height>])                  DRAW AN ELLIPSE
;      Part of Qt graphics. Outline drawn in COLOR and filled in BRUSH.
;      <x><y>    Coodinates of center
;      <width>   Width of ellipse
;      <height>  height of ellipse. If not given, same as width. (A circle)
; 


;
; This QT extension provides some additional features. If you use COLORS
; and GMODE you should not use COLOR and MODE.
; 
; COLORS -- like COLOR but also has two argument version that sets
; foreground and background both with 16 bit values. The single argument
; version only sets the foreground color. Returns the curently selected
; (via COLORS) foreground and background colors. The Single and double
; arguments can also be "triples" -- a list of R, G, and B intensities.
; 
; Constants are defined for the 16 color palette (see colors.lsp)
; 
; GMODE -- (GMODE nil) sets text mode and updates the graphics display
;          (GMODE t)  sets graphics mode and updates the graphics
; 		    display
;          (GMODE x y) resizes the display
;
;DRAWRECTCORNERS -- DRAWRECT with corner coodinates
;DRAWELLIPSECENTER -- DRAWELLIPSE with center coordinates
;
;POINT -- draw a point          

#+:packages
(unless (find-package "GRAPHIC")
	(make-package "GRAPHIC" :use '("XLISP")))

(in-package "GRAPHIC")

(export '(black blue green cyan red magenta brown ltgrey grey ltblue
		ltgreen ltcyan ltred ltmagenta yellow white))


(defconstant black 0)
(defconstant blue 1)
(defconstant green 2)
(defconstant cyan 3)
(defconstant red 4)
(defconstant magenta 5)
(defconstant brown 6)
(defconstant ltgrey 7)
(defconstant grey 8)
(defconstant ltblue 9)
(defconstant ltgreen 10)
(defconstant ltcyan 11)
(defconstant ltred 12)
(defconstant ltmagenta 13)
(defconstant yellow 14)
(defconstant white 15)

(export '(helvetica times courier normal bold italic bold-italic))
(defconstant helvetica 1)
(defconstant times 2)
(defconstant courier 0)
(defconstant normal 0)
(defconstant bold 1)
(defconstant italic 2)
(defconstant bold-italic 3)

(export '(nofill solid dense1 dense2 dense3 dense4 dense5 dense6 dense7
                 horlines verlines crosslines slash backslash diagcross))

(defconstant nofill 0)
(defconstant solid 1)
(defconstant dense1 2)
(defconstant dense2 3)
(defconstant dense3 4)
(defconstant dense4 5)
(defconstant dense5 6)
(defconstant dense6 7)
(defconstant dense7 8)
(defconstant horlines 9)
(defconstant verlines 10)
(defconstant crosslines 11)
(defconstant slash 12)
(defconstant backslash 13)
(defconstant diagcross 14)

(export '(gmode colors point drawrectcorners drawellipsecenter
                *xsize* *ysize* *columns* *rows* 
		*charwidth* *charheight*))

(defvar *xsize* 1024)
(defvar *ysize* 768)
(defvar *columns* 170)
(defvar *rows* 76)
(defvar *charwidth* (truncate 1024 170))
(defvar *charheight* (truncate 768 76))
(defvar *forecolor* '(255 255 255))
(defvar *backcolor* '(0 0 0))

(defun gmode (x &optional y &aux setting)
"(graphic:gmode <x> [<y>])                CONTROL GRAPHICS MODE
       Defined in qt.lsp
       <x>     If NIL sets text mode and updates display
               If T sets graphics mode and updates display
               If a fixnum, sets desired width in pixels.
       <y>     Fixnum desired height in pixels.
       returns NIL if <x> is T or NIL otherwise returns
               a list of number of columns, rows, width, and
               height. Also sets variables *xsize* *ysize*
               *columns* *rows* *charwidth* and *charheight*"
   
       (cond (y (setf setting (mode 20 0 x y)))
	     (x (mode 20 0 *xsize* *ysize*))
	     (t (mode 3)))
       (when setting
	     (setf *columns* (first setting))
	     (setf *rows* (second setting))
	     (setf *xsize* (third setting))
	     (setf *ysize* (fourth setting))
	     (setf *charwidth* (truncate *xsize* *columns*))
	     (setf *charheight* (truncate *ysize* *rows*)))
       setting)

(defun palette (n)
       (if (consp n)
	   n
	   (let ((val (min 15 (max 0 n))))
		(elt
		 #((0 0 0) 
		   (0 0 170)  
		   (0 170 0)  
		   (0 170 170)  
		   (170 0 0)  
		   (170 0 170)  
		   (170 85 0)  
		   (170 170 170)  
		   (85 85 85)  
		   (85 85 255)  
		   (85 255 85)  
		   (85 255 255)  
		   (255 85 85)  
		   (255 85 255)  
		   (255 255 85)  
		   (255 255 255)) val))))
       
(defun colors (&rest n)
"(graphic:colors <foreground> [<background>])       SET THE FORE/BACK COLORS
       <foreground> Either a list of R G B values in range 0-255 or
                    a fixnum color value.
       <background> If specified, sets background color as well. Either
                    a list of R G B values or a fixnum color value.
       returns      A list of the list of RGB foreground colors and a
                    list of RBG background colors.
Color values can be specified by constants graphic:black blue green cyan red
magenta brown ltgrey grey ltblue ltgreen ltcyan ltred ltmagenta yellow or
white."

      (let ((val (case (length n)
		     (3 (list n *backcolor*))
		     (6 (list (list (first n) (second n) (third n))
			      (cddr n)))
		     (1 (list (palette (first n)) *backcolor*))
		     (2 (list (palette (first n)) (palette (second
							    n))))
		     (t (error "invalid number of arguments")))))
	   (apply #'color (append (first val) (second val)))
	   (setf *forecolor* (first val))
	   (setf *backcolor* (second val))
	   val))


(defun point (x y)
"(graphic:point <x> <y>)                                       DRAW A POINT
       <x>      x coordinate of point
       <y>      y coodinate of point
       returns  T"
       (move x y x y))

(defun drawrectcorners (x1 y1 x2 y2)
"(graphic:drawrectcorners <x1> <y1> <x2> <y2>)                  DRAW A RECTANGLE
       Draws in current color and fills with current brush
       <x1>      x coodinate of a corner
       <y1>      y coodinate of a corner
       <x2>      x coordinate of opposing corner
       <y2>      y coordinate of opposing corner
       returns   T"
       (drawrect x1 y1 (- x2 x1) (- y2 y1)))

(defun drawellipsecenter (x y w &optional (h w))
"(graphic:drawellipsecenter <x> <y> <w> [<h>])              DRAW AN ELLIPSE
       Draws in current color and fills with current brush
       <x> <y>  Center coordinates
       <w>      Width
       <h>      height (default is width)
       returns  T"
       (drawellipse (- x (round w 2))
                    (- y (round h 2))
                    w h))

(in-package "USER")
(use-package "GRAPHIC")
(provide "qt")

