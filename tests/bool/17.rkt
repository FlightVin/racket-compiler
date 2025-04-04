(let ([a 1])
  (let ([b_cond #f]) (let ([b 0])
    (let ([c 1])
      (if (or (and (eq? a c) (not b_cond)) 
              (< a b))
          42
          24)))))
