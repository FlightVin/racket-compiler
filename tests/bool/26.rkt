(let ([result
       (if (let ([x #t]) 
             (let ([y #f])
               (if x y x)))
           (if #t
               (let ([z 42])
                 z)
               0)
           (let ([a #f])
             (if a 100 200)))])
  result)
