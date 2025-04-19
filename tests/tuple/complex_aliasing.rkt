(let ([inner (vector 1 2 3)])
  (let ([v1 (vector inner 4)])
    (let ([v2 (vector inner 5)])
      (let ([v3 (vector v1 v2)])
        (begin
          (vector-set! (vector-ref (vector-ref v3 0) 0) 1 42)
          (vector-ref (vector-ref (vector-ref v3 1) 0) 1))))))
