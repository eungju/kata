#lang scheme

(define units '(25 10 5 1))

(define (count-change change)
  (let loop ((remaining change) (usable-units units) (acc '()))
    (cond ((zero? remaining) (reverse acc))
          ((> (car usable-units) remaining) (loop remaining (cdr usable-units) acc))
          (true (loop (- remaining (car usable-units)) usable-units (cons (car usable-units) acc))))))

(require rackunit)
(check equal? (count-change 1) '(1))
(check equal? (count-change 2) '(1 1))
(check equal? (count-change 5) '(5))

(check equal? (count-change 50) '(25 25))
(check equal? (count-change 26) '(25 1))
(check equal? (count-change 14) '(10 1 1 1 1))
