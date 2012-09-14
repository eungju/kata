#lang scheme

(define *wanted* (string->list "HACKERCUP"))

(define (solve sentence)
  (let loop ((remaining (string->list sentence))
             (need *wanted*)
             (found 0))
  (cond ((empty? need) (loop remaining *wanted* (+ found 1)))
        ((member (car need) remaining) (loop (remove (car need) remaining) (cdr need) found))
        (true found))))

(require rackunit)
(check equal? (solve "WELCOME TO FACEBOOK HACKERCUP") 1)
(check equal? (solve "CUP WITH LABEL HACKERCUP BELONGS TO HACKER") 2)
(check equal? (solve "QUICK CUTE BROWN FOX JUMPS OVER THE LAZY DOG") 1)
(check equal? (solve "MOVE FAST BE BOLD") 0)
(check equal? (solve "HACK THE HACKERCUP") 1)

(define (main)
  (let loop ((i 1) (t (string->number (read-line))))
    (when (<= i t)
        (begin
          (let* ((sentence (read-line)))
            (printf "Case #~a: ~a~n" i (solve sentence)))
          (loop (+ i 1) t)))))

(main)
