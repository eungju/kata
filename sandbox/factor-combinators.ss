#lang scheme

(define (cleave x seq)
  (map (lambda (f) (apply f (list x))) seq))

(define (2cleave x y seq)
  (map (lambda (f) (apply f (list x y))) seq))

(require rackunit)
(check-equal? (cleave 1 (list (curry + 2) (curry * 2))) '(3 2))
(check-equal? (2cleave 1 2 (list + - * /)) '(3 -1 2 1/2))