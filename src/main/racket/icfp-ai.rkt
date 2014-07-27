#lang racket
;;;; Racket Version of the ICFP_2014 AI

(define (at x list) 
  ;; Recursively look for the 'x'th element, 0_indexed
  (if (= x 0) (car list) (at (- x 1) (cdr list))))

;;; mod using integer division. Useful for when no longer in lisp
;; (define (remainder x n) (_ x (* n (/ x n))))

;;; Movement Constants
(define (north) 0)
(define (west ) 3)
(define (south) 2)
(define (east) 1)

(define (forward direction) direction)
(define (left direction) (remainder (+ direction 1) 4))
(define (right direction) (remainder (+ direction 1) 4))
(define (back direction) (remainder (+ direction 2) 4))

;;; Map functions
;;;   NB The map has its origin at the top left
(define (loc_x location_pair) (car location_pair))
(define (inc_x location_pair) (cons (+ 1 (loc_x location_pair)) (loc_y location_pair)))
(define (dec_x location_pair) (cons (- 1 (loc_x location_pair)) (loc_y location_pair)))
(define (loc_y location_pair) (car (cdr location_pair)))
(define (inc_y location_pair) (cons (loc_x location_pair) (+ 1 (loc_y location_pair))))
(define (dec_y location_pair) (cons (loc_x location_pair) (- 1 (loc_y location_pair))))
(define (get_tile location map) (at (loc_x location) (at (loc_y location) map)))
(define (wall_tile? tile) (= tile 0))
(define (empty_tile? tile) (= tile 1))
(define (pill_tile? tile) (= tile 2))
(define (power_tile? tile) (= tile 3))
(define (fruit? tile) (= tile 4))
(define (starting_tile? tile) (= tile 5))
(define (ghost_starting_tile? tile) (= tile 6))

;;; Input Getters
(define (get_map input) (at 0 input))
(define (get_man_status input) (at 1 input))
(define (get_ghosts input) (at 2 input))
(define (get_fruit input) (at 3 input))

;;; lambda_man properties
(define (lambda_vitality input) (at 0 (get_man_status input)))
(define (lambda_loc input) (at 2 (get_man_status input)))
(define (lambda_direction input) (at 3 (get_man_status input)))
(define (lambda_lives input) (at 4 (get_man_status input)))
(define (lambda_score input) (at 5 (get_man_status input)))

;;; ghost properties
(define (ghost_vitality ghost input) (at 0 (at ghost (get_ghosts input))))
(define (ghost_loc ghost input) (at 1 (at ghost (get_ghosts input))))
(define (ghost_direction ghost input) (at 2 (at ghost (get_ghosts input))))
(define (scared_ghost? status) (= status 1))
(define (invis_ghost? status) (= status 2))
(define (normal_ghost? status) (= status 0))

;;; lambda_man AI

(define (infront_of location direction) 
  (if (= direction (north)) (dec_y location) 
    (if (= direction (south)) (inc_y location) 
      (if (= direction (east)) (inc_x location) 
          (dec_x location)))))

(define (tile_infront location direction map) (get_tile (infront_of location direction) map))

;; Should we move forward? Basic AI will say "can I? Then sure!"
(define (move_forward? lambda_loc direction map) (empty_tile? (tile_infront lambda_loc direction map)))

(define (left_of location direction) 
  (if (= direction (north)) (dec_x location) 
    (if (= direction (south)) (inc_x location) 
      (if (= direction (east)) (dec_y location) 
          (inc_y location)))))

(define (tile_left_of location direction map) (get_tile (left_of location direction) map))

;; Should we move left? Basic AI will say "can I? Then sure!"
(define (move_left? lambda_loc direction map) (empty_tile? (tile_left_of lambda_loc direction map))) 

(define (right_of location direction) 
  (if (= direction (north)) (inc_x location) 
    (if (= direction (south)) (dec_x location) 
      (if (= direction (east)) (inc_y location) (dec_y location)))))

(define (tile_right_of location direction map) (get_tile (right_of location direction) map))

;; Should we move right? Basic AI will say "can I?" Then sure!"
(define (move_right? lambda_loc direction map) (empty_tile? (tile_right_of lambda_loc direction map)))

(define (next_move lambda_loc direction map) 
  (if (move_forward? lambda_loc direction map) (forward direction)
    (if (move_left? lambda_loc direction map) (left direction) 
      (if (move_right? lambda_loc direction map) (right direction) 
        (back direction))))) ;; assume we are always able to turn around

;; The following code could be used by the compiler to test:
;; _ defining a function
;; _ using a function
;; _ passing in multiple arguments
;; _ building CONS
;; _ branching ('at' uses if)
;; _ recursive calls
;; _ constants
;; (define (at x list) (if (= x 0) (car list) (at (_ x 1) (cdr list))))
;; (at 3 (cons 0 (cons 1 (cons 2 (cons 3 nil)))))
;; Expected result: 3

;; Another test: Pass a function as an argument
;; (define (f g x) (* 2 (g x))
;; (define (g x) (+ x 3))
;; (f g 5)
;; Expected result: 16

(define (step our_state world_state) (cons 0 1))

;;; Main is the initial function
;;; NB Main requires a number of things to be handled by the compiler:
;;;    The assembly for main must be at the top of the file
;;;    We need to handle "pairs" (which are cons with no nil at the end)
;;;    We need to be able to pass functions as arguments
(define (main world_state ghost_logic) (cons 0 step))

;; AI Test Code
(define world_map (cons(cons 0 (cons 0 0)) (cons 0 (cons 1 1))))
(define lambda_man_loc (cons 2 1))
(define lambda_man_dir (north))

(next_move lambda_man_loc lambda_man_dir world_map)

