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

; These tests check all the OPEN modes (I hope), and the various
; combinations of reads/writes/positions on ascii and binary files


(setq tst "test.dat")
(setq tstbak "test.bak")
(should-work (delete-file tst))					;; 1
(should-work (delete-file tstbak))				;; 2
(should-be-false (open tst :direction :probe))			;; 3

(should-fail (open tst))					;; 4
(should-fail (open tst :direction :output :if-does-not-exist :error)) ;; 5
(should-fail (open tst :direction :output :if-exists :append))	;; 6
(should-fail (open tst :direction :output :if-exists :overwrite)) ;; 7

(should-be-false (open tst :direction :probe))			;; 8
(should-fail (open tst :direction :probe :if-does-not-exist :error)) ;; 9
(should-work (open tst :direction :probe 
		      :if-does-not-exist :create)) ;; 10
(should-be-true (open tst :direction :probe))			;; 11

(should-be-true (setq x (open tst)))				;;
(should-be-true (zerop (file-length x)))			;;
(should-fail (read-char x))					;;
(should-be-false (read-char x nil))				;;
(should-be-true (eq (read-char x nil 'x) 'x))                   ;;
(should-fail (write-char #\x x))				;;
(close x)

(should-fail (open tst :direction :output 
		   :if-exists :error :if-does-not-exist nil))	;;
(should-be-false (open tstbak :direction :probe))		;;
(should-be-false 
       (open tstbak :direction :output :if-does-not-exist nil)) ;;
(should-work (setq x (open tst :direction :output)))		;;
(should-be-true (open tstbak :direction :probe))		;;
(should-fail (read-char x))
(should-work (dotimes (i 10)(format x "First Lines~%")))
(should-be-true (setq p (file-position x)))
(should-work (format x "Line two~%Last line~%"))
(should-be-true (file-position x p))
(should-work (format x "Line too"))
(close x)

(should-work (setq x (open tst :direction :io 
			   :if-exists :overwrite)))
(should-be-true (string= "First Lines" (read-line x)))
(should-be-true (string= "First Lines" (read-line x)))
(should-be-true (string= "First Lines" (read-line x)))
(should-be-true (string= "First Lines" (read-line x)))
(should-be-true (string= "First Lines" (read-line x)))
(should-be-true (string= "First Lines" (read-line x)))
(should-be-true (string= "First Lines" (read-line x)))
(should-be-true (string= "First Lines" (read-line x)))
(should-be-true (string= "First Lines" (read-line x)))
(should-be-true (string= "First Lines" (read-line x)))
(should-be-true (string= "Line too" (read-line x)))
(should-be-true (string= "Last line" (read-line x)))
(should-be-true (eql (file-position x) (file-length x)))
(should-fail (file-postition x (1+ (file-length x))))
(should-fail (read-line x))
(should-be-false (read-line x nil))
(should-be-true (eq (read-line x nil 'x) 'x))
(should-be-true (file-position x p))
(should-be-true (string= "Line too" (read-line x)))
(should-be-true (file-position x p))
(should-work (format x "Line two~%"))
(should-be-true (string= "Last line" (read-line x)))
(should-be-true (file-position x p))
(should-be-true (string= "Line two" (read-line x)))
(should-work (format x "New Last line"))
(should-be-true (file-position x p))
(should-be-true (string= "Line two" (read-line x)))
(should-be-true (string= "New Last line" (read-line x)))
(setq p (file-position x))
(close x)
(should-work (setq x (open tst :direction :output :if-exists :append)))
(should-be-true (eql p (file-position x)))
(close x)
(should-work (setq x (open tst :direction :io :if-exists :supersede)))
(should-be-true (zerop (file-length x)))
(close x)
(should-be-false (open tst :direction :output 
		       :if-exists nil :if-does-not-exist :error))

(should-work (delete-file tstbak))

;; Test other keywords 
(should-work (close (open tst :direction :output :if-exists :rename)))
(should-be-true (open tstbak :direction :probe))
(should-work (delete-file tstbak))

(should-work (close (open tst :direction :output :if-exists :new-version)))
(should-be-true (open tstbak :direction :probe))
(should-work (delete-file tstbak))

(should-work (close (open tst :direction :output 
			  :if-exists :rename-and-delete)))
(should-be-false (open tstbak :direction :probe))

;; Check read/write/peek character
(should-work (setq x (open tst :direction :io 
			   :element-type 'character :if-exists :supersede)))
(should-work (progn (format x "abcd fghijklmnopqrstuvwxyz")
		    (file-position x :start)))
(should-be-true (eq #\a (read-char x)))
(should-be-true (eq #\b (peek-char nil x)))
(should-be-true (eql 1 (file-position x)))
(should-be-true (eq #\b (read-char x)))
(should-work (write-char #\? x))
(should-be-true (eq #\d (read-char x)))
(should-be-true (eq #\space (peek-char nil x)))
(should-be-true (eq #\f (peek-char t x)))
(should-be-true (eq #\f (read-char x)))
(should-work (file-position x 2))
(should-be-true (eq #\? (peek-char nil x)))
(should-be-true (file-position x :end))
(should-fail (peek-char nil x))
(should-be-false (peek-char nil x nil))
(should-be-true (eq (peek-char nil x nil 'x) 'x))
(should-fail (read-char x))
(should-be-false (read-char x nil))
(should-be-true (eq (read-char x nil 'x) 'x))
(should-be-true (file-position x 0))
(should-be-true (string= (read-line x) "ab?d fghijklmnopqrstuvwxyz"))
(should-be-false (peek-char nil x nil))
(should-be-false (peek-char t x nil))
(close x)

;; Check read/write byte
(should-work (setq x (open tst :direction :io 
			   :element-type 'fixnum :if-exists :supersede)))
(should-work (dotimes (i 256) (write-byte i x)))
(should-work (file-position x 0))
(should-be-true (let (z) 
		     (dotimes (i 256) 
			      (setq z (cons (eql i (read-byte x)) z)))
		     (every #'(lambda (x) x) z)))
(should-fail (read-byte x))
(should-be-false (read-byte x nil))
(should-be-true (eq (read-byte x nil 'x) 'x))
(close x)	     

;; Check read/write byte -- bignums option
(should-work (setq x (open tst :direction :io
			   :element-type '(signed-byte 8)
			   :if-exists :supersede)))
(should-work (dotimes (i 256) (write-byte (- i 128) x)))
(should-work (file-position x 0))
(should-be-true (eql -128 (read-byte x)))
(should-work (file-position x 128))
(should-be-true (eql 0 (read-byte x)))
(should-work (file-position x 255))
(should-be-true (eql 127 (read-byte x nil)))
(should-be-false (read-byte x nil))
(close x)

(should-work (setq x (open tst :direction :io
			   :element-type '(signed-byte 24)
			   :if-exists :supersede)))
(should-work (write-byte 0 x))
(should-work (write-byte #x7fbead x))
(should-work (write-byte #x-800000 x))
(should-work (file-position x 1))
(should-be-true (eql #x7fbead (read-byte x)))
(should-be-true (eql 2 (file-position x)))
(should-be-true (eql 3 (file-length x)))
(should-be-true (eql #x-800000 (read-byte x)))
(should-be-false (read-byte x nil))
(close x)

(should-work (setq x (open tst :direction :io
			   :element-type '(unsigned-byte 24)
			   :if-exists :supersede)))
(should-work (write-byte 0 x))
(should-work (write-byte #x7fbead x))
(should-work (write-byte #x80bead x))
(should-work (file-position x 0))
(should-be-true (eql 0 (read-byte x)))
(should-be-true (eql #x7fbead (read-byte x)))
(should-be-true (eql #x80bead (read-byte x)))
(should-be-false (read-byte x nil))
(close x)


(should-work (setq x (open tst :direction :io
			   :element-type '(signed-byte 32)
			   :if-exists :supersede)))
(should-work (write-byte 0 x))
(should-work (write-byte #x7fbeadfa x))
(should-work (write-byte #x-80000000 x))
(should-work (file-position x 1))
(should-be-true (eql #x7fbeadfa (read-byte x)))
(should-be-true (eql 2 (file-position x)))
(should-be-true (eql 3 (file-length x)))
(should-be-true (eql #x-80000000 (read-byte x)))
(should-be-false (read-byte x nil))
(close x)

(should-work (setq x (open tst :direction :io
			   :element-type '(unsigned-byte 32)
			   :if-exists :supersede)))
(should-work (write-byte 0 x))
(should-work (write-byte #x7fbeadfa x))
(should-work (write-byte #x80beadfa x))
(should-work (file-position x 0))
(should-be-true (eql 0 (read-byte x)))
(should-be-true (eql #x7fbeadfa (read-byte x)))
(should-be-true (eql #x80beadfa (read-byte x)))
(should-be-false (read-byte x nil))
(close x)

(should-work (setq x (open tst :direction :io
			   :element-type '(signed-byte 72)
			   :if-exists :supersede)))
(should-work (write-byte 0 x))
(should-work (write-byte #x7fbeadface00000000 x))
(should-work (write-byte #x-800000000000000000 x))
(should-work (file-position x 1))
(should-be-true (eql #x7fbeadface00000000 (read-byte x)))
(should-be-true (eql 2 (file-position x)))
(should-be-true (eql 3 (file-length x)))
(should-be-true (eql #x-800000000000000000 (read-byte x)))
(should-be-false (read-byte x nil))
(close x)

(should-work (setq x (open tst :direction :io
			   :element-type '(unsigned-byte 72)
			   :if-exists :supersede)))
(should-work (write-byte 0 x))
(should-work (write-byte #x7fbeadface00000000 x))
(should-work (write-byte #x80beadface00000000 x))
(should-work (file-position x 0))
(should-be-true (eql 0 (read-byte x)))
(should-be-true (eql #x7fbeadface00000000 (read-byte x)))
(should-be-true (eql #x80beadface00000000 (read-byte x)))
(should-be-false (read-byte x nil))
(close x)

(should-work (setq x (open tst :direction :output
			   :element-type 'signed-byte
			   :if-exists :supersede)))
(should-work (dotimes (i 7) (write-byte 0 x)))
(close x)
(should-work (setq x (open tst :direction :input
			   :element-type '(signed-byte 32))))
(should-be-true (eql (file-length x) 1))
(should-be-true (eql 0 (read-byte x)))
(should-be-false (read-byte x nil))
(close x)


(should-work (delete-file tst))

(should-be-false (file-position *terminal-io*))
(should-be-false (file-length *terminal-io*))

(terpri)

(princ test-number)
(princ " tests performed.")
(terpri)

(princ bugs-found)
(princ " bugs found.")
(terpri)



