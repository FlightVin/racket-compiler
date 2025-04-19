(let ([shared (vector 10 20 30)])
  (let ([v1 (vector shared (vector 40 50))])
    (let ([v2 (vector (vector 60 70) shared)])
      (let ([v3 (vector v1 v2)])
        (begin
          (vector-set! (vector-ref (vector-ref v3 0) 0) 1 99)
          (vector-ref (vector-ref (vector-ref v3 1) 1) 1))))))
