#lang scheme

(define (select-kth k xs)
  (let-values ([(a b) (partition (lambda (x) (< x (car xs))) xs)])
    (cond ((= (length a) k) (car b))
          ((> (length a) k) (select-kth k a))
          (true (select-kth (- k (length a) 1) (cdr b))))))

(require rackunit)
(check-equal? (list-ref (sort '(3 9 4 1 6) <) 2) 4)
(check-equal? (select-kth 0 '(3)) 3)
(check-equal? (select-kth 0 '(3 1)) 1)
(check-equal? (select-kth 1 '(3 1)) 3)
(check-equal? (select-kth 0 '(4 1 3)) 1)
(check-equal? (select-kth 1 '(4 1 3)) 3)
(check-equal? (select-kth 2 '(4 1 3)) 4)
