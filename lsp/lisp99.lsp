;;; Lisp example of "99 Bottles of beer on the wall"
(labels ((foo (x)
	   (and (<= 0 x) (cons x (foo (1- x))))))
  (format t (format nil 
		    "~~{~~%~~@(~~%~~R ~A ~A,~~)~~:*~~
		    ~~%~~@(~~R ~0@*~A!~~)~~
		    ~~%~~@(~2@*~A!~~)~~
		    ~~%~~@(~~[~A~~:;~~:*~~R~~] ~0@*~A ~A!~~)~~:*~~v,0^~~:*~~}~~
		    ~~%~~@(~~%~3@*~~*~A ~0@*~A ~A,~~)~~
		    ~~%~~@(~3@*~A ~0@*~A!~~)~~%~~@(~4@*~A.~~)~~
		    ~~%~~@(~~:*~~R ~0@*~A ~A.~~)"
		    "bottle~:p of beer"
		    "on the wall"
		    "take one down, pass it around"	
		    "no more"
		    "go to the store and buy some more"
		    )
	  (foo 99) 99))

