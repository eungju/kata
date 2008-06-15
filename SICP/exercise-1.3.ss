(define (sum-of-squares a b)
  (+ (* a a) (* b b)))
         
(define (sum-of-squares-of-two-largers a b c)
  (sum-of-squares (cond ((and (>= a b) (>= a c)) a)
                        ((and (>= b a) (>= b c)) b)
                        ((and (>= c a) (>= c b)) c))
                  (cond ((and (>= a b) (<= a c) a))
                        ((and (>= b a) (<= b c) b))
                        ((and (>= c a) (<= c b) c)))))


(= 13 (sum-of-squares-of-two-largers 1 2 3))
(= 5 (sum-of-squares-of-two-largers 1 2 1))
(= 2 (sum-of-squares-of-two-largers 1 1 1))
