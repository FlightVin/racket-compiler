(let ([v (vector 0 1 2)])
  (let ([i 0])
    (begin
      (while (< i 3)
        (begin
          (vector-set! v 0 (+ (vector-ref v 0) 10))
          (set! i (+ i 1))))
      (vector-ref v 2))))
