(define (make-pair [a : Integer] [b : Boolean]) : (Vector Integer Boolean)
    (vector a b))
(vector-ref (make-pair 42 #t) 0)
