#lang scheme

(define (combination xs)
  (if (empty? xs) '(())
      (let ((x (car xs))
            (subs (combination (cdr xs))))
        (append subs (map (lambda (sub) (cons x sub)) subs)))))


(define (permutation xs)
  (if (empty? xs)
      '(())
      (append-map (lambda (x) (map (lambda (sub) (cons x sub)) (permutation (remove x xs)))) xs)))

(require rackunit)
(check-equal? (combination '(1)) '(() (1)))
(check-equal? (combination '(1 2)) '(() (2) (1) (1 2)))
(check-equal? (combination '(1 2 3)) '(() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3)))
(check equal? (permutation '()) '(()))
(check equal? (permutation '(1)) '((1)))
(check equal? (permutation '(1 2)) '((1 2) (2 1)))
(check equal? (permutation '(1 2 3)) '((1 2 3) (1 3 2) (2 1 3) (2 3 1) (3 1 2) (3 2 1)))
