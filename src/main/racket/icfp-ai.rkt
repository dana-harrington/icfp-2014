#lang racket
;;;; Racket Version of the ICFP-2014 AI

(define (at x list) 
  ;; Recursively look for the 'x'th element, 0-indexed
  (if (= x 0) (car list) (at (- x 1) (cdr list))))

;;; mod using integer division. Useful for when no longer in lisp
;; (define (remainder x n) (- x (* n (/ x n))))

;;; Movement Constants
(define (north) 0)
(define (west ) 3)
(define (south) 2)
(define (east) 1)

(define (forward direction) direction)
(define (left direction) (remainder (- direction 1) 4))
(define (right direction) (remainder (+ direction 1) 4))
(define (back direction) (remainder (+ direction 2) 4))

;;; Map functions
;;;   NB The map has its origin at the top left
(define (loc-x location-pair) (car location-pair))
(define (inc-x location-pair) (cons (+ 1 (loc-x location-pair)) (loc-y location-pair)))
(define (dec-x location-pair) (cons (- 1 (loc-x location-pair)) (loc-y location-pair)))
(define (loc-y location-pair) (car (cdr location-pair)))
(define (inc-y location-pair) (cons (loc-x location-pair) (+ 1 (loc-y location-pair))))
(define (dec-y location-pair) (cons (loc-x location-pair) (- 1 (loc-y location-pair))))
(define (get-tile location map) (at (loc-x location) (at (loc-y location) map)))
(define (wall-tile? tile) (= tile 0))
(define (empty-tile? tile) (= tile 1))
(define (pill-tile? tile) (= tile 2))
(define (power-tile? tile) (= tile 3))
(define (fruit? tile) (= tile 4))
(define (starting-tile? tile) (= tile 5))
(define (ghost-starting-tile? tile) (= tile 6))

;;; Input Getters
(define (get-map input) (at 0 input))
(define (get-man-status input) (at 1 input))
(define (get-ghosts input) (at 2 input))
(define (get-fruit input) (at 3 input))

;;; lambda-man properties
(define (lambda-vitality input) (at 0 (get-man-status input)))
(define (lambda-loc input) (at 2 (get-man-status input)))
(define (lambda-direction input) (at 3 (get-man-status input)))
(define (lambda-lives input) (at 4 (get-man-status input)))
(define (lambda-score input) (at 5 (get-man-status input)))

;;; ghost properties
(define (ghost-vitality ghost input) (at 0 (at ghost (get-ghosts input))))
(define (ghost-loc ghost input) (at 1 (at ghost (get-ghosts input))))
(define (ghost-direction ghost input) (at 2 (at ghost (get-ghosts input))))
(define (scared-ghost? status) (= status 1))
(define (invis-ghost? status) (= status 2))
(define (normal-ghost? status) (= status 0))

;;; lambda-man AI

(define (infront-of location direction) 
  (if (= direction (north)) (dec-y location) 
    (if (= direction (south)) (inc-y location) 
      (if (= direction (east)) (inc-x location) 
          (dec-x location)))))

(define (tile-infront location direction map) (get-tile (infront-of location direction) map))

;; Should we move forward? Basic AI will say "can I? Then sure!"
(define (move-forward? lambda-loc direction map) (empty-tile? (tile-infront lambda-loc direction map)))

(define (left-of location direction) 
  (if (= direction (north)) (dec-x location) 
    (if (= direction (south)) (inc-x location) 
      (if (= direction (east)) (dec-y location) 
          (inc-y location)))))

(define (tile-left-of location direction map) (get-tile (left-of location direction) map))

;; Should we move left? Basic AI will say "can I? Then sure!"
(define (move-left? lambda-loc direction map) (empty-tile? (tile-left-of lambda-loc direction map))) 

(define (right-of location direction) 
  (if (= direction (north)) (inc-x location) 
    (if (= direction (south)) (dec-x location) 
      (if (= direction (east)) (inc-y location) (dec-y location)))))

(define (tile-right-of location direction map) (get-tile (right-of location direction) map))

;; Should we move right? Basic AI will say "can I?" Then sure!"
(define (move-right? lambda-loc direction map) (empty-tile? (tile-right-of lambda-loc direction map)))

(define (next-move lambda-loc direction map) 
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
;; (define (at x list) (if (= x 0) (car list) (at (- x 1) (cdr list))))
;; (at 3 (cons 0 (cons 1 (cons 2 (cons 3 nil)))))
;; Expected result: 3

;; Another test: Pass a function as an argument
;; (define (f g x) (* 2 (g x))
;; (define (g x) (+ x 3))
;; (f g 5)
;; Expected result: 16

(define (step our-state world-state) (cons 0 1))

;;; Main is the initial function
;;; NB Main requires a number of things to be handled by the compiler:
;;;    The assembly for main must be at the top of the file
;;;    We need to handle "pairs" (which are cons with no nil at the end)
;;;    We need to be able to pass functions as arguments
(define (main world-state ghost-logic) (cons 0 step))

;; AI Test Code
(define world-map (cons(cons 0 (cons 0 0)) (cons 0 (cons 1 1))))
(define lambda-man-loc (list 2 1))
(define lambda-man-dir (north))

(next-move lambda-man-loc lambda-man-dir world-map)

