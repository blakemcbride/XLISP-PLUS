#-:common (load "common")

(unless (find-package "ACKERMAN")
        (make-package "ACKERMAN" :use '("XLISP")))

(in-package "ACKERMAN")
(export '(ack))

(defun ack (m n) "Ackerman's Function"
	(cond ((zerop m) (1+ n))
	      ((zerop n) (ack (1- m) 1))
	      ((ack (1- m) (ack m (1- n))))))

(provide "ackerman")
(print "To execute, run (ackerman:ack 3 10)")
