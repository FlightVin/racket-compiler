(let ([cond1 #t]
      [cond2 #f])
  (if cond1
      (if cond2
          1
          (let ([cond1 #f])
            (if cond1
                2
                3)))
      (if cond2
          4
          5)))
