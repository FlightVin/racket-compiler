(let ([v (vector 1 2)])
  (begin
    (vector-set! v 0 55)
    (vector-ref v 0)))
