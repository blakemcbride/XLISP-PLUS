;; This is John McCarthy's Tak benchmark
#-:common (load "common")

(unless (find-package "TAK")
        (make-package "TAK" :use '("XLISP")))

(in-package "TAK")
(export '(tak dotak))

(defun tak (x y z) "Tak Benchmark function, commonly called with arguments 18 12 6, conveniently defined as DOTAK"
  (if (not (< y x))
      z
      (tak (tak (1- x) y z)
           (tak (1- y) z x)
           (tak (1- z) x y))))

(defun dotak () "Run the TAK benchmark"
  (tak 18 12 6))

(provide "tak")
(print "To execute, run (tak:dotak)")
