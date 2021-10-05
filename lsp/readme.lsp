Brief description of included Lisp source files and related documentation:

init.lsp*	Default initialization file
makewks.lsp     Builds the xlisp.wks file.

Utilities:
backquot.lsp	Adds working nested backquotes
classes.lsp*	Useful functions for OOP
common.lsp*	More Common Lisp compatible functions.
common2.lsp*    Still more Common Lisp compatible functions.
edit.lsp	Access external editor on .lsp files
evalenv.lsp	EVAL in current lexical context
glos.lsp	Glossary function
glos.txt	data file for glossary function
infix.lsp	Read macros for Infix to prefix converter (Winston and Horn)
inspect.lsp*    Structure Editor (new version)
loop.lsp    Common Lisp LOOP Macro. Wow!
matrix.lsp	Poor implementation of multidimensional arrays
memo.lsp	Memoization facility
pp.lsp*		Pretty printer
profile.lsp	Profile utility
rational.lsp    An attempt at implementing "rationalize"
readme.lsp      This file
repair.lsp 	Structure Editor (old version)
sendmacr.lsp	Read macros for "send" function
step.lsp*	Simple single-step utility
stepper.lsp	More advanced single-step utility
stepper.doc	Documentation for stepper.lsp
turtle.lsp	Turtle graphics primitives, from PC-LISP

* Functions documented in manual

Examples (not all of which are useful):
ackerman.lsp	Ackerman's function
akalah.lsp	"Kalah" (stones&pits) game
akavect.lsp	(same as akalah.lsp, but uses arrays rather than lists)
art.lsp		Simple OOP example
blocks.lsp	Winston & Horn's "Blocks world". Uses classes.lsp.
change.lsp	Change maker
clsdemo.lsp     Tutorial of XLISP implementing Smalltalk's Collection classes
dragon.lsp	Dragon curve, originally from PC-Lisp. Uses turtle.lsp
example.lsp	Simple OOP example
fact.lsp	Factorial function
factor.lsp      Finds prime factors, determines if a value is prime.
fib.lsp		Fibonacci function
formattest.lsp  Tests of the FORMAT function
gblocks.lsp	blocks.lsp, with graphic display
hanoi.lsp	Tower of hannoi puzzle
hdwr.lsp	OOP example of hardware simulation
ifthen.lsp	Mini expert system from Winston & Horn
lisp99.lsp      99 Bottles of Beer on the Wall in LISP. Somewhat obfuscated
match.lsp	Pattern matcher from Winston & Horn
prolog.lsp	Tiny Prolog interpreter
qa.lsp		Question Answering program
queens.lsp	Queens puzzle
queens2.lsp	Queens puzzle -- semi-graphical
search.lsp	Searching functions from Winston & Horn
sort.lsp	Sorting routines
tak.lsp		McCarthy's TAK benchmark.
tconc.lsp	tconc implementation
turtles.lsp	OOP turtle graphics example.
wildcard.lsp	Wildcard pattern matcher
