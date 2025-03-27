(let ([x #t])
  (let ([y #f])
    (if x
        (let ([x #f])
          (if x 1 2))
        (let ([y #t])
          (if y 3 4)))))
