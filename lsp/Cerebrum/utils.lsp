;;;
;;; Utilities for CEREBRUM
;;;


(require "pp")  ; The pretty-printing file


;; Graphing stuff


(require "turtle")  ; Load the turtlegraphis file
(use-package "EXT")
(setq *GMODE* 640)

(defun draw-box (&optional (bottom 0) (top 350)
                           (left 0) (right 600)
                           (c white))
;;
;; Draws a box.  Surprise!
;;
 (color c)
 (move left bottom)
 (draw left top)
 (draw right top)
 (draw right bottom)
 (draw left bottom))


(defun graph (max-x max-y y-list &optional (bottom 0) (top 350)
                                           (left 0) (right 600)
                                           (c white))
;;
;; Draws a graph.  Max-x and max-y are the highest possible ratings along
;; each axis, and y-list is a list of y-values that correspond to the integer
;; values of x.
;;
 ;; Don't want any of that nasty zero-division!
 (if (> max-x 0)
     (let* ((x-unit (float (/ (- right left) max-x)))
            (y-unit (float (/ (- top bottom) max-y)))
            (x nil)
            (y nil))
      (color c)
      (setq y (round (+ bottom (* (car y-list) y-unit))))
      (move left y)
      (do ((i 1 (1+ i)))
       ((= i (length y-list)))
       (setq x (round (+ left (* i x-unit))))
       (setq y (round (+ bottom (* (nth i y-list) y-unit))))
       (draw x y)))))
