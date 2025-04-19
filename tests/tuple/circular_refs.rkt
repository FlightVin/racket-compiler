(let ([v1 (vector 1 2)])
  (let ([v2 (vector 3 4)])
    (let ([outer (vector v1 v2)])
      (begin
        (vector-set! v1 1 outer)
        (vector-set! (vector-ref (vector-ref v1 1) 0) 0 42)
        (vector-ref v1 0)))))
