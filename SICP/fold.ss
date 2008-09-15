(define (foldl f initial xs)
  (if (null? xs)
      initial
      (foldl f (f initial (car xs)) (cdr xs))))

(define (foldr f initial xs)
  (if (null? xs)
      initial
      (f (car xs) (foldr f initial (cdr xs)))))

(define (sum xs fold)
  (fold (lambda (a b) (+ a b)) 0 xs))

(define (sum-right-associative xs)
  (sum xs foldr))

(define (sum-left-associative xs)
  (sum xs foldl))