#lang scheme

(define (combination xs)
  (if (empty? xs)
      '(())
      (let ((x (car xs)) (ys (combination (cdr xs))))
        (append ys (map (lambda (sub) (cons x sub)) ys)))))

(define (permutation xs)
  (if (empty? xs)
      '(())
      (append-map (lambda (x)
                    (map (lambda (ys) (cons x ys)) (permutation (remove x xs))))
                  xs)))
  
(require rackunit)
(check-equal? (combination '(1)) '(() (1)))
(check-equal? (combination '(1 2)) '(() (2) (1) (1 2)))
(check-equal? (combination '(1 2 3)) '(() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3)))
(check equal? (permutation '()) '(()))
(check equal? (permutation '(1)) '((1)))
(check equal? (permutation '(1 2)) '((1 2) (2 1)))
(check equal? (permutation '(1 2 3)) '((1 2 3) (1 3 2) (2 1 3) (2 3 1) (3 1 2) (3 2 1)))
