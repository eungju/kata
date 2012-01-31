#lang scheme

(require srfi/1)

(define (prime? n)
  (every (lambda (x) (not (zero? (remainder n x)))) (iota (- n 2) 2)))

(require rackunit)
(check equal? (prime? 2) true)
(check equal? (prime? 3) true)
(check equal? (prime? 4) false)
(check equal? (prime? 5) true)
(check equal? (prime? 6) false)
(check equal? (prime? 7) true)
(check equal? (prime? 8) false)
(check equal? (prime? 9) false)
(check equal? (prime? 11) true)
