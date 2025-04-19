(let ([a 5])
  (let ([b 10])
    (let ([v (vector (+ a b) (- b a) (* a 2))])
      (vector-ref v 0))))
