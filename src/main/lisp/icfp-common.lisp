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

(defun forward (direction) direction)
(defun left (direction) (mod (- direction 1) 4))
(defun right (direction) (mod (+ direction 1) 4))
(defun back (direction) (mod (+ direction 2) 4))

;;; Map functions
;;;   NB The map has its origin at the top left
(defun loc-x (location-pair) (car location-pair))
(defun inc-x (location-pair) (cons (add 1 (loc-x location-pair)) (loc-y location-pair)))
(defun dec-x (location-pair) (cons (sub 1 (loc-x location-pair)) (loc-y location-pair)))
(defun loc-y (location-pair) (car (cdr location-pair)))
(defun inc-y (location-pair) (cons (loc-x location-pair) (add 1 (loc-y location-pair))))
(defun dec-y (location-pair) (cons (loc-x location-pair) (sub 1 (loc-y location-pair))))
(defun get-tile(x y map) (at x (at y map)))
(defun get-tile (location map) (get-tile (loc-x location) (loc-y location) map))
(defun wall-tile? (tile) (= tile 0))
(defun empty-tile? (tile) (= tile 1))
(defun pill-tile? (tile) (= tile 2))
(defun power-tile? (tile) (= tile 3))
(defun fruit? (tile) (= tile 4))
(defun starting-tile? (tile) (= tile 5))
(defun ghost-starting-tile? (tile) (= tile 6))

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

;;; lambda-man AI

(defun infront-of (location direction) 
  (if (= direction (north)) (dec-y location) 
    (if (= direction (south)) (inc-y location) 
      (if (= direction (east)) (inc-x location) 
        (if (= direction (west)) (dec-x location))))))

(defun tile-infront (location direction map) (get-tile (infront-of location direction) map))

;; Should we move forward? Basic AI will say "can I? Then sure!"
(defun move-forward? (lambda-loc direction map) (empty-tile? (tile-infront lambda-loc direction map)))

(defun left-of (location direction) 
  (if (= direction (north)) (dec-x location) 
    (if (= direction (south)) (inc-x location) 
      (if (= direction (east)) (dec-y location) 
        (if (= direction (west)) (inc-y location))))))

(defun tile-left-of (location direction map) (get-tile (left-of location direction) map))

;; Should we move left? Basic AI will say "can I? Then sure!"
(defun move-left? (lambda-loc direction map) (empty-tile? (tile-left-of lambda-loc direction map))) 

(defun right-of (location direction) 
  (if (= direction (north)) (inc-x location) 
    (if (= direction (south)) (dec-x location) 
      (if (= direction (east)) (inc-y location) 
        (if (= direction (west)) (dec-y location))))))

(defun tile-right-of (location direction map) (get-tile (right-of location direction) map))

;; Should we move right? Basic AI will say "can I?" Then sure!"
(defun move-right? (lambda-loc direction map) (empty-tile? (tile-right-of lambda-loc direction map)))

(defun next-movement (lambda-loc direction map) 
  (if (move-forward? lambda-loc direction map) (forward direction)
    (if (move-left? lambda-loc direction map) (left direction) 
      (if (move-right? lambda-loc direction map) (right direction) 
        (back direction))))) ;; assume we are always able to turn around

;; The following code could be used by the compiler to test:
;; - defining a function
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

