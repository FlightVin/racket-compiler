(let ([v (vector 42 #t (vector 1 2))])
  (if (vector-ref v 1)
      (vector-ref v 0)
      (vector-ref (vector-ref v 2) 0)))
