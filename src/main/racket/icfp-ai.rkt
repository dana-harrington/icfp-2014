#lang racket
;;;; Racket Version of the ICFP_2014 AI

(define (at x list) 
  ;; Recursively look for the 'x'th element, 0_indexed
  (if (= x 0) (car list) (at (- x 1) (cdr list))))

;; TODO Remove this before compiling. atom? will exist in our language but not racket
(define (atom? n) (not (or (pair? n) (null? n))))
(define (lt a b) (< a b))

;; TODO Uncomment before compiling
;;(define (modulo x n) (- x (* n (/ x n))))
;;(define (or a b) (> (+ a b) 0))
;;(define (not a) (if (= a 1) 0 1))
;;(define (lt a b) (if (>= a b) 0 1))

(define (size list) (size_help list 0))
(define (size_help list n) (if (atom? list) n (size_help (cdr list) (+ n 1))))  

;;; Movement Constants
(define (north) 0)
(define (west) 3)
(define (south) 2)
(define (east) 1)

(define (forward direction) direction)
(define (left direction) (modulo (- direction 1) 4))
(define (right direction) (modulo (+ direction 1) 4))
(define (back direction) (modulo (+ direction 2) 4))

;;; Map functions
;;;   NB The map has its origin at the top left
(define (loc_x location_pair) (car location_pair))
(define (inc_x location_pair) (cons (+ (loc_x location_pair) 1) (loc_y location_pair)))
(define (dec_x location_pair) (cons (- (loc_x location_pair) 1) (loc_y location_pair)))
(define (loc_y location_pair) (cdr location_pair))
(define (inc_y location_pair) (cons (loc_x location_pair) (+ (loc_y location_pair) 1)))
(define (dec_y location_pair) (cons (loc_x location_pair) (- (loc_y location_pair) 1)))
(define (height map) (size map))
(define (length map) (size (car map)))
(define (get_tile location map) (if (or (or (>= (loc_x location) (length map)) (lt (loc_x location) 0))
                                        (or (>= (loc_y location) (height map)) (lt (loc_y location) 0))) 
                                    0 ;; Say it is a wall
                                    (at (loc_x location) (at (loc_y location) map))))
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
(define (lambda_loc input) (at 1 (get_man_status input)))
(define (lambda_direction input) (at 2 (get_man_status input)))
(define (lambda_lives input) (at 3 (get_man_status input)))
(define (lambda_score input) (at 4 (get_man_status input)))

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
(define (move_forward? lambda_loc direction map) (not (wall_tile? (tile_infront lambda_loc direction map))))

(define (left_of location direction) 
  (if (= direction (north)) (dec_x location) 
    (if (= direction (south)) (inc_x location) 
      (if (= direction (east)) (dec_y location) 
          (inc_y location)))))

(define (tile_left_of location direction map) (get_tile (left_of location direction) map))

;; Should we move left? Basic AI will say "can I? Then sure!"
(define (move_left? lambda_loc direction map) (not (wall_tile? (tile_left_of lambda_loc direction map)))) 

(define (right_of location direction) 
  (if (= direction (north)) (inc_x location) 
    (if (= direction (south)) (dec_x location) 
      (if (= direction (east)) (inc_y location) (dec_y location)))))

(define (tile_right_of location direction map) (get_tile (right_of location direction) map))

;; Should we move right? Basic AI will say "can I?" Then sure!"
(define (move_right? lambda_loc direction map) (not (wall_tile? (tile_right_of lambda_loc direction map))))

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
;; (define (at x list) (if (= x 0) (car list) (at (- x 1) (cdr list))))
;; (at 3 (cons 0 (cons 1 (cons 2 (cons 3 nil)))))
;; Expected result: 3

;; Another test: Pass a function as an argument
;; (define (f g x) (* 2 (g x))
;; (define (g x) (+ x 3))
;; (f g 5)
;; Expected result: 16

(define (step our_state world_state) (cons our_state (next_move (lambda_loc world_state) (lambda_direction world_state) (get_map world_state))))

;; Test AI Code
;(define world_map (cons(cons 0 (cons 0 (cons 0 0))) (cons (cons 1 (cons 1 (cons 1 0))) 0)))
;(define lambda_man_loc (cons 0 1))
;(define lambda_man_dir (south))
;(define lambda_state (cons 0 (cons lambda_man_loc (cons lambda_man_dir (cons 0 0)))))
;(define world_state (cons world_map (cons lambda_state (cons 0 0))))

(lambda (world_state ghost_logic) (cons 0 step))

;(next_move (lambda_loc world_state) (lambda_direction world_state) (get_map world_state))