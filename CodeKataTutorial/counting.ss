#lang scheme

(define (counting n)
  (cond ((= n 1) (+ 1 1))
        ((= n 2) (+ (counting (- n 1)) 1 (counting (- n 1))))
        ((= n 3) (+ (counting (- n 1)) (counting (- n 2)) 1 (counting (- n 1))))
        (else (+ (counting (- n 1))
                 (counting (- n 2))
                 (counting (- n 3))
                 (counting (- n 1))))))

(require rackunit)
(check-equal? (counting 1) 2)
(check-equal? (counting 2) 5)
(check-equal? (counting 3) 13)
(check-equal? (counting 4) 33)
(check-equal? (counting 5) 84)
(check-equal? (counting 10) 9003)

(define (counting/bottomup n)
  (cond ((= n 1) (+ 1 1))
        ((= n 2) (+ (counting (- n 1)) 1 (counting (- n 1))))
        ((= n 3) (+ (counting (- n 1)) (counting (- n 2)) 1 (counting (- n 1))))
        (else (let loop ((i 4) (n-1 (counting 3)) (n-2 (counting 2)) (n-3 (counting 1)))
                (if (> i n)
                    n-1
                    (loop (+ i 1) (+ n-1 n-2 n-3 n-1) n-1 n-2)
              )))))

(check-equal? (counting/bottomup 10) 9003)
(check-equal? (counting/bottomup 100) 31203505712236975348528576040635119800253)
