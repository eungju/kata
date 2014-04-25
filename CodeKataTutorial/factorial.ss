#lang scheme

(define (factorial-topdown n)
  (if (= n 0)
      1
      (* n (factorial-topdown (- n 1)))))

(define (factorial-bottomup n)
  (let loop ((i 1) (acc 1))
    (if (<= i n)
        (loop (+ i 1) (* acc i))
        acc)))

(require rackunit)

(check equal? (factorial-topdown 1) 1)
(check equal? (factorial-topdown 3) 6)
(check equal? (factorial-topdown 4) 24)
(check equal? (factorial-topdown 0) 1)

(check equal? (factorial-bottomup 1) 1)
(check equal? (factorial-bottomup 3) 6)
(check equal? (factorial-bottomup 4) 24)
(check equal? (factorial-bottomup 0) 1)