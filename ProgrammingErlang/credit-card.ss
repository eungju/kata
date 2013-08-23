#lang scheme
(require srfi/1)

(define (digit->integer c)
  (- (char->integer c) (char->integer #\0)))

(define (luhn-checksum ccn)
  (remainder
   (apply + (map (lambda (d i)
                   (let ((p (* (digit->integer d) (+ 1 (remainder i 2)))))
                     (+ (quotient p 10) (remainder p 10))))
                 (reverse (string->list ccn))
                 (iota (string-length ccn))))
   10))

(define (luhn? ccn)
  (eq? 0 (luhn-checksum ccn)))
  
(require rackunit)
(check-eq? (luhn-checksum "1") 1)
(check-eq? (luhn-checksum "12") 4)
(check-eq? (luhn-checksum "80") 7)
(check-eq? (luhn? "34") true)
(check-eq? (luhn? "123") false)
