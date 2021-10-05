; Good ol towers of hanoi
;
; Usage:
;      (hanoi <n>)
;          <n> - an integer the number of discs

; Improved by Tom Almy 2/2011

(defun hanoi(n)
       (labels (

                (print-move (n from to )
                            (format t "Move disk ~s from ~s to ~s\n" n from to))
                (transfer (from to via n)
                          (if (eql n 1)
                              (print-move n from to)
                              (progn (transfer from via to (- n 1))
                                     (print-move n from to)
                                     (transfer via to from (- n 1))))))

                (transfer 'A 'B 'C (the fixnum n))))

(provide "hanoi")
