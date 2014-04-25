#lang scheme

(define (quicksort xs)
  (if (or (empty? xs) (empty? (cdr xs)))
      xs
      (let-values ([(a b) (partition (lambda (x) (< x (car xs))) xs)])
        (append (quicksort a) (cons (car b) (quicksort (cdr b)))))))

(require rackunit)
(check-equal? (quicksort '()) '())
(check-equal? (quicksort '(3)) '(3))
(check-equal? (quicksort '(1 3)) '(1 3))
(check-equal? (quicksort '(3 1)) '(1 3))
(check-equal? (quicksort '(4 1 3)) '(1 3 4))
(check-equal? (quicksort '(4 1 3 2)) '(1 2 3 4))
