(let ([a 1])
  (begin
    (set! a 2)
    (begin
      (set! a (+ a 10))
      a)
    (+ a 1)))
