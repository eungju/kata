#lang scheme

(require srfi/1)

(define (distance a b)
  (sqrt (+ (expt (- (car a) (car b)) 2) (expt (- (cadr a) (cadr b)) 2))))

(define (closest-pair/bf ps)
  (let loop ((p (car ps)) (ps (cdr ps)) (d +inf.0))
    (if (empty? ps)
        d
        (loop (car ps) (cdr ps) (fold min d (map (curry distance p) ps))))))

(define (closest-pair ps)
  (cond ((or (empty? ps) (empty? (cdr ps))) false)
        ((or (empty? (cddr ps)) (empty? (cdddr ps))) (closest-pair/bf ps))
        (true (let-values ([(l r) (split-at ps (floor (/ (length ps) 2)))])
                (let* ((m (car r))
                       (d (min (closest-pair l) (closest-pair r)))
                       (inside-strip? (lambda (a) (< (abs (- (car a) (car m))) d)))
                       (l-strip (drop-while (negate inside-strip?) l))
                       (r-strip (take-while inside-strip? r)))
                  (fold min d (append-map (lambda (a) (map (lambda (b) (distance a b)) r-strip)) l-strip)))))))


;(closest-pair (filter inside-strip? xs))

(require rackunit)
(check = (distance '(0 0) '(0 1)) 1)
(check = (closest-pair/bf '((0 0) (0 1))) 1)
(check = (closest-pair/bf '((0 0) (2 0) (5 0))) 2)
(check = (closest-pair '((0 0) (5 0) (6 0) (8 0))) 1)
(check-= (closest-pair '((0 2) (6 67) (43 71) (39 107) (189 140))) 36.2215 0.0001)