(let ([outer (vector (vector 1 2) (vector 3 4))])
  (let ([inner (vector-ref outer 0)])
    (begin
      (vector-set! inner 0 42)
      (vector-ref (vector-ref outer 0) 0))))
