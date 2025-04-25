(define (get-first [v : (Vector Integer Boolean)]) : Integer
    (vector-ref v 0))
(get-first (vector 99 #f))
