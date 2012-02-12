#lang scheme

(define (gcd-euclid a b)
  (if (= b 0)
      a
      (gcd-euclid b (remainder a b))))

(require rackunit)
(check-equal? (gcd-euclid 2 3) 1)
(check-equal? (gcd-euclid 2 4) 2)
(check-equal? (gcd-euclid 3 12) 3)
(check-equal? (gcd-euclid 12 36) 12)
(check-equal? (gcd-euclid 36 12) 12)
