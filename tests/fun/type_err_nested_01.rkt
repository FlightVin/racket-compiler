(define (inner-error [x : Integer]) : Integer
    (+ x #t))
(inner-error 10)
