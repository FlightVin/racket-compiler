(let ([x #t])
  (let ([y (if x #f #t)])
    (let ([z (if y #t #f)])
      (if z
          (if x 1 2)
          (if y 3 4)))))
