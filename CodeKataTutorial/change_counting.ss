#lang scheme

(define (count-change change units)
  (let loop ((remaining change) (usable-units units) (acc '()))
    (cond ((zero? remaining) (reverse acc))
          ((> (car usable-units) remaining) (loop remaining (cdr usable-units) acc))
          (true (loop (- remaining (car usable-units)) usable-units (cons (car usable-units) acc))))))

(require rackunit)
(define US-units '(25 10 5 1))
(check equal? (count-change 1 US-units) '(1))
(check equal? (count-change 2 US-units) '(1 1))
(check equal? (count-change 5 US-units) '(5))
(check equal? (count-change 50 US-units) '(25 25))
(check equal? (count-change 26 US-units) '(25 1))
(check equal? (count-change 14 US-units) '(10 1 1 1 1))

(define (count-change/bf change units)
  (if (zero? change)
      0
      (apply min (map (lambda (u) (+ (count-change/bf (- change u) units) 1)) (filter (lambda (u) (<= u change)) units)))))
 
(check equal? (count-change/bf 14 '(25 10 7 5 1)) 2)
