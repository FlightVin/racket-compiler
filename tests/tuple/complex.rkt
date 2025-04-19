(let ([v1 (vector 1 2 3)])
  (let ([v2 (vector 4 5 6)])
    (let ([v3 (vector v1 v2)])
      (begin
        (vector-set! (vector-ref v3 0) 0 42)
        (vector-ref v1 0)))))
