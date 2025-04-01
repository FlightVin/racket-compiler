(let ([x 5])
  (let ([y x])
    (begin
      (set! x 15)
      (+ x y))))
