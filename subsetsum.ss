#lang scheme

(require srfi/1)

(define (sum xs)
  (fold + 0 xs))

(define (subset-sum-bf k xs subset)
  (if (empty? xs)
      (and (not (empty? subset)) (= k (sum subset)))
      (or (subset-sum-bf k (cdr xs) subset)
          (subset-sum-bf k (cdr xs) (cons (car xs) subset)))))

(define (subset-sum?bf k xs)
  (subset-sum-bf k xs '()))

(define (subset-sum k xs subset)
  (if (empty? xs)
      (and (not (empty? subset)) (= k (sum subset)))
      (if (> (sum subset) k)
          false
          (or (subset-sum k (cdr xs) subset)
              (subset-sum k (cdr xs) (cons (car xs) subset))))))

(define (subset-sum? k xs)
  (subset-sum k xs '()))

(require rackunit)
(check-equal? (sum '(-3 -2 5)) 0)
(check-equal? (subset-sum?bf 0 '(-7 -3 -2 5 8)) true)
(check-equal? (subset-sum?bf 0 '(1 2)) false)
(check-equal? (subset-sum? 0 '(-7 -3 -2 5 8)) true)
(check-equal? (subset-sum? 0 '(1 2)) false)
(check-equal? (subset-sum? 0 '(8 5 -2 -3 -7 -3)) false)
