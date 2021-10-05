;;;
;;; The Memoization facility, from Norvig's "Paradigms of Artificial
;;; Intelligence Programming.
;;; Adapted for XLisp by Leo Sarasua (modifications marked LSG)

;;; Placed in package MEMOIZE

(provide 'memo)

#+:packages
(unless (find-package "EXT")
	(make-package "EXT" :use '("XLISP")))

(in-package "EXT")
(export '(defun-memo memoize unmemoize clear-memoize))


(defmacro defun-memo (fn args &rest body) ; LSG
  "Define a memoized function."
  `(memoize (defun ,fn ,args . ,body)))

(defun memo (fn &key (key #'first) (test #'eql) (size 97) name)
  "Return a memo-function of fn."
  (let ((table (make-hash-table :test test :size size)))
    (setf (get name 'memo) table)
    #'(lambda (&rest args)
        (let ((k (funcall key args)))
          (multiple-value-bind (val found-p)
              (gethash k table)
            (if found-p val
                (setf (gethash k table) (apply fn args))))))))

(defun memoize (fn-name &key (key #'first) (test #'eql) (size 97))

"(memoize <fn> &key :key :test :size)            MEMOIZE FUNCTION\
Replace fn-name's global definition with a memoized version.\
  <fn>      The function to memoize
  <key>     Argument list member to use as hash key, default FIRST\
  <test>    Test function for hash table, default EQL\
  <size>    Size of hash table, default 97\
  returns   function or NIL if already memoized"  
  (unless (get fn-name 'memo) ; don't memoize a memoized function
          (clear-memoize fn-name)
          (psetf (symbol-function fn-name)
                 (memo (symbol-function fn-name)
                       :name fn-name :key key :test test :size size)
                 (get fn-name 'saved-fn)
                 (symbol-function fn-name))
          fn-name))

(defun unmemoize (fn-name)
"(unmemoize <fn>)                             UN-MEMOIZE A FUNCTION\
Un-memoize a function\
  <fn>      The function to unmemoize\
  returns   T if successful, NIL if function not memoized"

       (when (get fn-name 'memo)
             (setf (symbol-function fn-name) (get fn-name 'saved-fn))
             (remprop fn-name 'memo)
             (remprop fn-name 'saved-fn)
             t))


(defun clear-memoize (fn-name)
  "Clear the hash table from a memo function."
  (let ((table (get fn-name 'memo)))
    (when table (clrhash table))))

(in-package "USER")
