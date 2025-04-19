(let ([base (vector 1 2 3)])
  (let ([path1 (vector base 4)])
    (let ([path2 (vector 5 base)])
      (let ([selector #t])
        (let ([chosen (if selector path1 path2)])
          (begin
            (vector-set! (vector-ref chosen (if selector 0 1)) 2 42)
            (vector-ref base 2)))))))
