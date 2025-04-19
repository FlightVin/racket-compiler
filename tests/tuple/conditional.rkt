(let ([v (vector 10 20 30)])
  (if (< (vector-ref v 0) (vector-ref v 1))
      (vector-ref v 2)
      (vector-ref v 0)))
