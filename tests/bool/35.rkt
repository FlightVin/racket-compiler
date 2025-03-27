(let ([x 100])
  (let ([y (let ([x 50])
             (if (< x 60)
                 x
                 0))])
    (let ([x (if (eq? y 50)
                 (+ x y)
                 (- x y))])
      x)))
