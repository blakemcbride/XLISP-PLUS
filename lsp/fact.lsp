; Factorial function, recursive and iterative
(provide "fact")

(defun fact (n) "(fact <n>) -- calculate n!"
       (cond ((zerop n) 1)
	     ((= n 1) 1)
	     (t (* n (fact (- n 1))))))

(defun facti (n &aux (v 1))  "(fact <n>) -- calculate n!, Iterative version"
       (dotimes (i n) (setq v (* v (1+ i))))
       v)
