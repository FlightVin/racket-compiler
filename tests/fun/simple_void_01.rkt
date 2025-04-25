(define (setter [x : Integer]) : Void
    (set! x 5))
(let ([y 10])
    (begin (setter y) y))
