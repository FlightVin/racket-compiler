(let ([v1 (vector 10 20)])
  (let ([v2 v1])
    (begin
      (vector-set! v2 1 99)
      (+ (vector-ref v1 0) (vector-ref v1 1))
    )))
