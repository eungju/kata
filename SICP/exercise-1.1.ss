(eq? 10 10)

(eq? 12 (+ 5 3 4))

(eq? 8 (- 9 1))

(eq? 3 (/ 6 2))

(eq? 6 (+ (* 2 4) (- 4 6)))

(define a 3)

(define b (+ a 1))

(eq? 19 (+ a b (* a b)))

(eq? false (= a b))

(eq? 3 (if (and (> b a) (< b (* a b)))
         a
         b))

(eq? 16 (cond ((= a 4) 6)
            ((= b 4) (+ 6 7 a))
            (else 25)))

(eq? 6 (+ 2 (if (> b a) b a)))

(eq? 16 (* (cond ((> a b) a)
               ((< a b) b)
               (else -1))
         (+ a 1)))

