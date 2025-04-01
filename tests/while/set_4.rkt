(let ([x 100])
  (begin
    (let ([x 50])
      (set! x 60))
    x))