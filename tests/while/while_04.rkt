(let ([continue #t])
  (let ([count 0])
    (begin
      (while continue
        (begin
          (set! count (+ count 1))
          (if (eq? count 3)
              (set! continue #f)
              (void))))
      count)))
