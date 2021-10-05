(defun fib (x) "Calculate Fibonacci number using recursion"
       (if (< x 2)
           x
           (+ (fib (1- x)) (fib (- x 2)))))

(defun fibi (n) "Calculate Fibonacci number using iteration"
	(do	((i 1 (1+ i))
		 (fib-i-1 0 fib-i)
		 (fib-i   1 (+ fib-i fib-i-1)))
		((= i n) fib-i)))
