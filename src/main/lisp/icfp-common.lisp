;;;; Lisp Version of the ICFP-2014 AI


(defun at(x list) 
  ;; Recursively look for the 'x'th element, 0-indexed
  (if (= x 0) (car list) (at (- x 1) (cdr list))))

;;; Movement Constants
(defun up () 0)
(defun left () 3)
(defun down () 2)
(defun right () 1)

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

;;; simple test
(print (at 3 (cons 0 (cons 1 (cons 2 (cons 3 nil))))))





