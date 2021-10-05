; Many people have had trouble creating an initial workspace where the tools
; package is accessable. This sample LSP file will create a workspace which
; will be loaded by default.
; To build, delete any existing xlisp.wks file, run xlisp, and then load
; this file. You may want to customize for the actual tools you want.
(expand 5)		; Or whatever you want -- object here is to make
			; an enlarged image to reduce garbage collections.

;; The first three loads complete XLISP's documented core functions
(load "common")		; Common Lisp extensions MUST LOAD FIRST Adds feature :common
(load "common2")	; more Common Lisp extensions, adds feature :common2
(load "classes")	; Classes, adds feature :classes

;; Collection classes
(require "collections")    ; Not really a demo but a group of collection classes, package COLLECTIONS, feature :collections


;; These are optional debugging packages that go in TOOLS
(require "profile")     ; Profiler
;(require "step")        ; Simple stepper tool, ESTEP
(require "stepper")	; Stepper tool, STEP
(require "pp")		; Pretty printer tool (nicer than pprint)
(require "inspect")     ; Inspector (has INSPECT and DESCRIBE)
;(require "repair")      ; Old version of Inspect
;(require "edit")        ; link to external editor
(use-package :tools)	; makes package :tools accessable
(push :largemem *features*) ; document.lsp will load documention into the image
                       ; rather than reading it from the file. Makes image about 250k larger
(require "document")	; Glossary (glos.txt must be in current directory)
                        ;  and DOCUMENTATION function

; We have to load the documentation feature last so that everything above gets documented.
; At this point, the def* functions have been modified to implement the documentation fields.

; Instead of "document", you might want "glos" 
;(require "glos")        ; Glossary, without documentation function

; New LOOP loaded after glossary to pick up documentation
; This one also adds the :loop feature and a new package, XLISP_LOOP
(require "loop")

;; Various extensions -- go into package EXT
(require "evalenv")    ; EVAL-ENV is EVAL that evaluates in the current environment
;(require "infix")      ; Square brackets give algebraic infix
;(require "sendmacr")   ; Square brackets give send, surprisingly (from a retrospective viewpoint) like Objective-C
;(require "backquot")   ; Nesting backquotes (replaces definition in package XLISP)
(require "rational")   ; Adds an attempt at the RATIONALIZE function 
(require "memo")	; Adds MEMOIZE UNMEMOIZE DEFUN-MEMO and CLEAR-MEMOIZE
#+:graphics (require "turtle")     ; Turtle Graphics. Adds feature :turtle for compatibility

#+:qt (require "qt")    ; Qt graphics extensions, package GRAPHIC
;(use-package :ext)     ; Make the extensions visible


(setq *startup-functions* (list (lambda () (princ "\nLoaded workspace xlisp.wks\n\n"))))
(gc)                    ; Do a garbage collect
(save "xlisp")		; save image
(gc)
