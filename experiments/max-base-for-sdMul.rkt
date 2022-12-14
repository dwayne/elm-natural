#lang racket

(define (inequality n)
  (<= (- (expt 2 (* 2 n))
         (expt 2 n))
      (expt 2 53)))

;; We want to find the largest value of n such that (inequality n) is true.

(define (solve-inequality lo hi)
  (if (<= lo hi)
      (let ((mid (quotient (+ lo hi) 2)))
        (if (inequality mid)
            (solve-inequality (+ mid 1) hi)
            (solve-inequality lo (- mid 1))))
      hi))

;; I know (inequality 1) is true and (inequality 30) is false,
;; so that's a good range to search.
(solve-inequality 1 30)
