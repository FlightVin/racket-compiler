(let ([v1 (vector 1 2 3 4 5)])
  (let ([v2 (vector v1 v1 v1)])
    (let ([v3 (vector v2 v2)])
      (let ([i 0])
        (begin
          (while (< i 3)
            (begin
              (vector-set! (vector-ref (vector-ref (vector-ref v3 0) i) 0) i (+ i 10))
              (set! i (+ i 1))))
          (vector-ref (vector-ref (vector-ref (vector-ref v3 1) 0) 0) 2))))))
