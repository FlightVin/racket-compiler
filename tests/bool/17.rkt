(let ([a 1])
  (let ([b 0])
    (let ([c 1])
      (if (or (and (eq? a c) (not b)) 
              (< a b))
          42
          24))))
