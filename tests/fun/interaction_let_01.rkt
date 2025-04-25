(define (foo [x : Integer]) : Integer
    (let ([y (+ x 1)])
        (+ y x)))
(foo 10)
