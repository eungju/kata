#! /usr/bin/env racket
#lang scheme

(define (guess-best-result t s)
  (if s
      (if (zero? (quotient t 3)) (remainder t 3) (quotient (+ t 4) 3))
      (quotient (+ t 2) 3)))

(define (solve s p ts)
  (+
   (count (lambda (t) (>= (guess-best-result t false) p)) ts)
   (min s (count (lambda (t) (and (< (guess-best-result t false) p) (>= (guess-best-result t true) p))) ts))))
  
(require rackunit)

(check equal? (guess-best-result 0 false) 0)
(check equal? (guess-best-result 1 false) 1)
(check equal? (guess-best-result 2 false) 1)
(check equal? (guess-best-result 3 false) 1)
(check equal? (guess-best-result 4 false) 2)
(check equal? (guess-best-result 5 false) 2)
(check equal? (guess-best-result 6 false) 2)
(check equal? (guess-best-result 7 false) 3)

(check equal? (guess-best-result 0 true) 0)
(check equal? (guess-best-result 1 true) 1)
(check equal? (guess-best-result 2 true) 2)
(check equal? (guess-best-result 3 true) 2)
(check equal? (guess-best-result 4 true) 2)
(check equal? (guess-best-result 5 true) 3)
(check equal? (guess-best-result 6 true) 3)
(check equal? (guess-best-result 7 true) 3)
(check equal? (guess-best-result 8 true) 4)
(check equal? (guess-best-result 9 true) 4)
(check equal? (guess-best-result 10 true) 4)
(check equal? (guess-best-result 11 true) 5)

(check equal? (solve 1 5 '(15 13 11)) 3)
(check equal? (solve 0 8 '(23 22 21)) 2)
(check equal? (solve 1 1 '(8 0)) 1)
(check equal? (solve 2 8 '(29 20 8 18 18 21)) 3)

(require srfi/13)
(define (main)
  (let loop ((i 1) (t (string->number (read-line))))
    (when (<= i t)
        (begin
          (let* ((tokens (string-tokenize (read-line)))
                 (parameters (map string->number tokens)))
            (printf "Case #~a: ~a~n" i (solve (second parameters) (third parameters) (cdddr parameters))))
          (loop (+ i 1) t)))))
;(main)