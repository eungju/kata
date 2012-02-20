#lang scheme

(define (fibonacci n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (true (+ (fibonacci (- n 1)) (fibonacci (- n 2))))))

(require rackunit)
(check equal? (fibonacci 0) 0)
(check equal? (fibonacci 1) 1)
(check equal? (fibonacci 2) 1)
(check equal? (fibonacci 3) 2)
(check equal? (fibonacci 4) 3)
(check equal? (fibonacci 5) 5)
(check equal? (fibonacci 6) 8)

(define (fibonacci/bottomup n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (let loop ((i 2) (u 0) (v 1))
                (if (> i n)
                    v
                    (loop (+ i 1) v (+ u v)))))))

(check equal? (fibonacci/bottomup 100) 354224848179261915075)
