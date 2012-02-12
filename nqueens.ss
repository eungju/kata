#lang scheme

(require srfi/1)

(define (attack? a b)
  (or
   (= (car a) (car b))
   (= (cadr a) (cadr b))
   (= (abs (- (car a) (car b))) (abs (- (cadr a) (cadr b))))))

(define (safe? queens)
  (if (empty? queens)
      true
      (let ((a (car queens)) (others (cdr queens)))
        (if (any (lambda (b) (attack? a b)) others)
            false
            (safe? others)))))

(define (nqueens- n r queens)
  (if (= r n)
      (list queens)
      (let loop ((c 0) (solutions '()))
        (if (= c n)
            solutions
            (if (safe? (cons (list r c) queens))
                (loop (+ c 1) (append (nqueens- n (+ r 1) (cons (list r c) queens)) solutions))
                (loop (+ c 1) solutions))))))

(define (nqueens n) (nqueens- n 0 '()))

(require rackunit)
(check-equal? (safe? '((0 0) (0 2))) false)
(check-equal? (safe? '((0 0) (2 0))) false)
(check-equal? (safe? '((0 0) (1 1))) false)
(check-equal? (safe? '((1 1) (2 0))) false)
(check-equal? (safe? '((1 1) (0 2))) false)
(check-equal? (safe? '((0 0) (1 2))) true)
(check-equal? (safe? '((0 0) (1 2) (2 4))) true)
(check-equal? (safe? '((0 0) (1 2) (1 3))) false)

(check-equal? (nqueens 4) '(((3 1) (2 3) (1 0) (0 2)) ((3 2) (2 0) (1 3) (0 1))))
(check-equal? (length (nqueens 8)) 92)
(check-equal? (length (nqueens 10)) 724)