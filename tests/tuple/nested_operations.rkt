(let ([nums (vector 1 2 3)])
  (let ([flags (vector #t #f #t)])
    (let ([combined (vector nums flags (vector 4 5 6))])
      (let ([i 0])
        (begin
          (while (< i 3)
            (begin
              (if (vector-ref flags 0)
                  (vector-set! nums 0 (+ (vector-ref nums 0) 
                                         (vector-ref (vector-ref combined 2) 0)))
                  (void))
              (set! i (+ i 1))))
          (vector-ref nums 2))))))
