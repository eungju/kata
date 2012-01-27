#lang scheme

(require srfi/1)

(define (generate n p1 m a b)
  (let loop ((i 2) (acc (list p1)))
    (cond ((> i n) (reverse acc))
          (true (loop (+ i 1) (cons (+ (remainder (+ (* a (car acc)) b) m) 1) acc))))))
  
(define (products n p1 w1 m k a b c d)
  (zip (generate n p1 m a b) (generate n w1 k c d)))

(define (better-than? a b)
  (or (and (< (car a) (car b)) (<= (cadr a) (cadr b)))
      (and (< (cadr a) (cadr b)) (<= (car a) (car b)))))

(define (bargains ps)
  (filter (lambda (a) (not (any (lambda (b) (better-than? b a)) ps))) ps))

(define (terrible-deals ps)
  (filter (lambda (c) (not (any (lambda (d) (better-than? c d)) ps))) ps))

(define (solve n p1 w1 m k a b c d)
  (let ((ps (products n p1 w1 m k a b c d)))
    (map length (list (terrible-deals ps) (bargains ps)))))

(require rackunit)
(check equal? (solve 5 1 4 5 7 1 0 1 2) '(3 3))
(check equal? (solve 3 1 3 3 3 1 0 1 1) '(3 3))
(check equal? (solve 8 1 3 3 3 1 0 1 2) '(2 3))
(check equal? (solve 13 5 7 5 9 1 3 2 5) '(2 2))
(check equal? (solve 11 2 3 5 7 11 13 17 19) '(3 1))

(require srfi/13)

(define (main)
  (let loop ((i 1) (t (string->number (read-line))))
    (when (<= i t)
        (begin
          (let* ((tokens (string-tokenize (read-line)))
                 (parameters (map string->number tokens)))
            (printf "Case #~a: ~a~n" i (apply solve parameters)))
          (loop (+ i 1) t)))))

;(main)
