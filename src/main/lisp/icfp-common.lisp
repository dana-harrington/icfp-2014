;;;; Lisp Version of the ICFP-2014 AI

(defun at(x list) 
  ;; Recursively look for the 'x'th element, 0-indexed
  (if (= x 0) (car list) (at (- x 1) (cdr list))))

;;; mod using integer division. Useful for when no longer in lisp
;; (defun mod (x n) (- x (* n (/ x n))))

;;; Movement Constants
(defun north () 0)
(defun west () 3)
(defun south () 2)
(defun east () 1)

(defun left (direction) (mod (- direction 1) 4))
(defun right (direction) (mod (+ direction 1) 4))
(defun back (direction) (mod (+ direction 2) 4))

;;; Map functions
(defun get-tile(x y map) (at x (at y map)))
(defun wall-tile? (tile) (= tile 0))
(defun empty-tile? (tile) (= tile 1))
(defun pill-tile? (tile) (= tile 2))
(defun power-tile? (tile) (= tile 3))
(defun fruit? (tile) (= tile 4))
(defun our-tile? (tile) (= tile 5))
(defun ghost-tile? (tile) (= tile 6))

;;; Input Getters
(defun get-map (input) (at 0 input))
(defun get-man-status (input) (at 1 input))
(defun get-ghosts (input) (at 2 input))
(defun get-fruit (input) (at 3 input))

;;; lambda-man properties
(defun lambda-vitality (input) (at 0 (get-man-status input)))
(defun lambda-loc (input) (at 2 (get-man-status input)))
(defun lambda-direction (input) (at 3 (get-man-status input)))
(defun lambda-lives (input) (at 4 (get-man-status input)))
(defun lambda-score (input) (at 5 (get-man-status input)))

;;; ghost properties
(defun ghost-vitality (ghost input) (at 0 (at ghost (get-ghosts input))))
(defun ghost-loc (ghost input) (at 1 (at ghost (get-ghosts input))))
(defun ghost-direction (ghost input) (at 2 (at ghost (get-ghosts input))))
(defun scared-ghost? (status) (= status 1))
(defun invis-ghost? (status) (= status 2))
(defun normal-ghost? (status) (= status 0))

;; The following code could be used by the compiler to test:
;; - using a function
;; - passing in multiple arguments
;; - building CONS
;; - branching ('at' uses if)
;; - recursive calls
;; - constants
;; (defun at(x list) (if (= x 0) (car list) (at (- x 1) (cdr list))))
;; (at 3 (cons 0 (cons 1 (cons 2 (cons 3 nil)))))
;; Expected result: 3



(defun step (our-state world-state) (cons 0 1))

;;; Main is the initial function
;;; NB Main requires a number of things to be handled by the compiler:
;;;    The assembly for main must be at the top of the file
;;;    We need to handle "pairs" (which are cons with no nil at the end)
;;;    We need to be able to pass functions as arguments
(defun main (world-state ghost-logic) (cons 0 step))



