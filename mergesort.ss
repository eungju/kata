#lang scheme

(require srfi/1)

(define (merge as bs)
  (let loop ((xs as) (ys bs) (acc '()))
    (cond ((every empty? (list xs ys)) (reverse acc))
          ((empty? xs) (loop xs '() (append (reverse ys) acc)))
          ((empty? ys) (loop '() ys (append (reverse xs) acc)))
          (true (let ((x (car xs)) (y (car ys)))
                  (if (> x y)
                      (loop xs (cdr ys) (cons y acc))
                      (loop (cdr xs) ys (cons x acc))))))))

(define (mergesort xs)
  (if (or (empty? xs) (empty? (cdr xs)))
      xs
      (let-values ([(a b) (split-at xs (floor (/ (length xs) 2)))])
        (merge (mergesort a) (mergesort b)))))

(require rackunit)
(check-equal? (mergesort '()) '())
(check-equal? (mergesort '(3)) '(3))
(check-equal? (mergesort '(1 3)) '(1 3))
(check-equal? (mergesort '(3 1)) '(1 3))
(check-equal? (mergesort '(4 1 3)) '(1 3 4))
(check-equal? (mergesort '(4 1 3 2)) '(1 2 3 4))
