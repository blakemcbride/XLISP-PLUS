;; Load with :print t  to see operation
(format nil "Enter ~A now. 92" 92)
(format nil "Enter ~A now. (a b)" '(a b))
(format nil "Enter ~6A now. bye---" "bye")
(format nil "Enter ~4,2,3,'.@A now. ...bye" "bye")
(setq x '|88|)
(format nil "Enter ~A and ~S now. 88 \\88" x x)
(format nil "Enter ~B now. 110" 6)
(format nil "Enter ~B now. (a b)" '(a b))
(format nil "Enter ~,,' :B now. 111 111 111 111" 4095)
(defun foo (x y)
         (format nil "Time ~D fl~:@P like ~R arrow~:P." x y))
(foo 3 1)
(foo 1 3)
(foo 0.3 0)
(foo -1 -1)

(format nil "~2R ~:*~10R ~:*~26R ~:*~R ~:*~@R ~:*~:R ~:*~:@R  10011 19 J nineteen XIX nineteenth XVIIII" 19)
(format nil "~(~2R ~:*~10R ~:*~26R ~:*~R ~:*~@R ~:*~:R ~:*~:@R~)  10011 19 j nineteen xix nineteenth xviiii" 19)
(format nil "~@:(~2R ~:*~10R ~:*~26R ~:*~R ~:*~@R ~:*~:R ~:*~:@R~)  10011 19 J NINETEEN XIX NINETEENTH XVIIII" 19)

(format nil "~B ~:*~O ~:*~D ~:*~X  1101 15 13 d" 13)
(format nil "~3@*~A ~v@*~A ~2*~A  d c e" 'a 'b 'c 'd 2 'e)

(format nil "~? ~A  3 4 foo" "~D ~D"  '(3 4) 'foo)
(format nil "~@? ~A  3 4 foo" "~D ~D" 3 4 'foo)

(format nil "~@[length is ~D ~] ~@[depth is ~D ~]  depth is 13" nil 13)

(format nil "~[one~;two~;three~]  one" 0)
(format nil "~[one~;two~;three~]  two" 1)
(format nil "~[one~;two~;three~]  three" 2)
(format nil "~[one~;two~;three~]       " 3) 
(format nil "~[one~;two~:;three~] three" 3)

(format nil "~:[no data~;total is ~D~]  no data" nil)
(format nil "~:[no data~;total is ~:*~D~]  13" 13)
(format nil "~@[total is ~D~]  " nil)
(format nil "~@[total is ~D~]  13" 13)

(format nil "~:{ ~@?~:^ ...~}   a" '(("a")))
(format nil "~:{ ~@?~:^ ...~}   a ... b ... c" '(("a") ("b") ("c")))

(setq sandwich "Inside - ~#[nothing~; ~A~; ~A and ~A~:;~
~@{~#[~; and~] ~A~^,~}~]." x nil)
(format nil sandwich)
(format nil sandwich "bacon" "lettuce")
(format nil sandwich "bacon" "lettuce" "tomato")
(format nil sandwich "bacon" "lettuce" "tomato" "cheese")
