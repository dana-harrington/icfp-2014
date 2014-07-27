(define (len l) (if (atom? l) 0 (+ 1 (len (cdr l)))))
(define nil 0)
(len (cons 1 (cons 2 (nil))))
; Expect 2
